-- | A module that handles a lot of the ugliness of asynchronous programming when calling a subprocess
-- | using the purescript-node-child-process module.  The problem with the purescript child process module is that it
-- | doesn't use Aff, and it doesn't handle the scenario where you have a dependency chain of calling several commands
-- | in a row or of saving the output of a command.
-- |
-- | The code here looks and is very mutable.  Part of the reason for that is because the onData function in purescript
-- | and in node has to take a callback which returns nothing (Unit).  This is a problem because how can we save any
-- | data?  That means we have to use a Ref or STRef (or maybe a Writer monad) to accumulate the data.  Moreover, this
-- | is a problem for callbacks.  The onExit function has the same problem.
module Command.Command where

import Control.Bind ((>=>))
import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Control.Monad.Eff.Ref (Ref, REF, newRef, writeRef, readRef, modifyRef)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Node.Buffer (Buffer, BUFFER, toString)
import Node.ChildProcess ( SpawnOptions
                         , defaultSpawnOptions
                         , spawn
                         , ChildProcess
                         , CHILD_PROCESS
                         , onExit
                         , Exit(..)
                         , stdout
                         )
import Node.Encoding (Encoding(..))
import Node.Stream (onData)
import Node.Process (PROCESS)
import Prelude (Unit, bind, pure, show, ($), (<>), discard)


type Cmd = { command :: String
           , args :: Array String
           , opts :: SpawnOptions
           , combineErr :: Boolean
           , output :: Ref String -- FIXME: Make this a selectable type (eg Duplex, Socket, or other source)
           , process :: Ref (Maybe ChildProcess)
           , save :: Boolean
           }


-- | Represents how to run a command
newtype Command = Command Cmd


instance showCommand :: Show Command where
  show (Command cmd) =  "{ command: " <> cmd.command <> "\n"
                     <> ", args: " <> foldl (\acc n ->  acc <> " " <> n) "" cmd.args <> "\n"
                     <> "}\n"


getCmdOpt :: Command -> SpawnOptions
getCmdOpt (Command {opts}) = opts


setCmdOpt :: Command -> SpawnOptions -> Command
setCmdOpt (Command c) sopt = Command { command: c.command
                                     , args: c.args
                                     , opts: sopt
                                     , combineErr: c.combineErr
                                     , output: c.output
                                     , process: c.process
                                     , save: c.save
                                     }


setOptDir :: SpawnOptions -> Maybe String -> SpawnOptions
setOptDir o@{cwd: c} dir = {cwd: dir, stdio: o.stdio, env: o.env, detached: o.detached, uid: o.uid, gid: o.gid}


setCmdDir :: Command -> Maybe String -> Command
setCmdDir c@(Command cmd) dir = Command { command: cmd.command
                                        , args: cmd.args
                                        , opts: setOptDir (getCmdOpt c) dir
                                        , combineErr: cmd.combineErr
                                        , output: cmd.output
                                        , process: cmd.process
                                        , save: cmd.save
                                        }


setCmdSave :: Command -> Boolean -> Command
setCmdSave c@(Command cmd) enable = Command { command: cmd.command
                                            , args: cmd.args
                                            , opts: cmd.opts
                                            , combineErr: cmd.combineErr
                                            , output: cmd.output
                                            , process: cmd.process
                                            , save: enable
                                            }

makeOptsWithDir :: String -> Maybe SpawnOptions
makeOptsWithDir dir = (Just $ setOptDir defaultSpawnOptions (Just dir))


-- | Function to save data from a buffer into an Ref
onDataSave :: forall e
            . Ref String
           -> Buffer
           -> Eff ( ref :: REF, buffer :: BUFFER, console :: CONSOLE | e) Unit
onDataSave ref buff = do
  bdata <- toString UTF8 buff
  modified <- modifyRef ref \current -> current <> bdata
  combined <- readRef ref
  log bdata
  --pure unit

-- | These are all the effects of running a child process.  It has a REF side effect due to "mutating" the
-- | Command object, it has a CHILD_PROCESS due to calling node's spawn function, it has EXCEPTION, because the spawn
-- | could fail, it has BUFFER, because the stdout of the ChildProcess is being saved to an IO Buffer, and it has
-- | CONSOLE because output is logged.
type CmdEff e = ( cp :: CHILD_PROCESS
                , ref :: REF
                , exception :: EXCEPTION
                , buffer :: BUFFER
                , console :: CONSOLE
                | e
                )


type ProcEffect e = ( process :: PROCESS
                    , ref :: REF
                    , buffer :: BUFFER
                    , cp :: CHILD_PROCESS
                    , exception :: EXCEPTION
                    , console :: CONSOLE
                    | e
                    )

-- | A default exit handler for launching commands
defaultExitHdlr :: forall e. Exit -> Eff (console :: CONSOLE | e) Unit
defaultExitHdlr exit = case exit of
      Normally x -> log $ "Got exit code of: " <> show x
      _ -> log $ "Exited due to signal: " <> show exit


-- | An exit handler which takes a reference of an initial value of an empty string, indicating the (unfinished) state.
-- | Allows the caller of onExitState to pass in a ref and check this ref
onExitState :: forall e. Ref String -> Exit -> Eff (ref :: REF, console :: CONSOLE | e) Unit
onExitState ref exit = case exit of
  Normally x -> do
    writeRef ref $ show x
    log $ "Got exit code of: " <> show x
  BySignal sig ->  do
    writeRef ref $ "" <> show sig
    log $ "Exited abnormally due to signal: " <> show exit


-- | Creates a default Command object that can be passed to launch.  This only uses defaultSpawnOptions
makeDefCmd :: forall e.  String -> Array String -> Maybe SpawnOptions -> Eff (ref :: REF | e) Command
makeDefCmd c args opts = do
  outp <- newRef ""
  proc <- newRef Nothing
  let opt = case opts of
              (Just o) -> o
              Nothing -> defaultSpawnOptions
      cmd = Command { command: c, args: args, opts: opt, combineErr: true, output: outp, process: proc, save: true}
  pure cmd


defErr :: forall e. Error -> Eff (console :: CONSOLE | e) Unit
defErr err = log $ message err


showOutput :: forall e. Command -> Eff (ref :: REF, console :: CONSOLE | e) Unit
showOutput (Command {output}) = do
  out <- readRef output
  log out


getOutput :: forall e. Command -> Eff (ref :: REF, console :: CONSOLE | e) String
getOutput (Command {output}) = do
  out <- readRef output
  pure out


-- | Launch a child process along with an error callback and success callback
launchImpl :: forall e
            . Command
           -> (Error -> Eff (CmdEff e) Unit)
           -> (Command -> Eff (CmdEff e) Unit)
           -> Eff (CmdEff e) Unit
launchImpl cmd@(Command {command, args, opts, process, output, save}) ecb scb = do
  cp <- spawn command args opts
  -- Store the cp ChildProcess into process
  _ <- writeRef process (Just cp)
  -- The output is accumulated to cmd.output each time the data event is caught
  _ <- onData (stdout cp) if save then (onDataSave output) else (toString UTF8 >=> log)
  -- On finish, call
  onExit cp \exit -> case exit of
              Normally 0 -> (scb cmd)
              Normally x -> do
                let err = "command failed with ret code: " <> show x
                ecb $ error err
              BySignal _ -> do
                let err = "command failed due to signal: " <> (show exit)
                ecb $ error err


-- asynchronous launch.  Ok, this was super confusing.  makeAff takes a function that takes two callbacks, one for
-- error handling, and the other for success and returns Eff e Unit.  What was super confusing to me is how you actually
-- passed the callbacks.  So in the example below, if I call launch, how do I actually pass in the error and success
-- handlers?  That is what runAff is for.  What is still confusing to me though is what if my asynchronous computation
-- needs multiple handlers?  Like for example copyFile (which uses readFile that takes a handler, and writeFile that
-- also takes its own handler)?
launch :: forall e
        . Command
       -> Aff (CmdEff e) Command
launch cmd  =  makeAff $ launchImpl cmd


test :: forall e. Eff (CmdEff e) Unit
test = do
  lastOutput <- newRef ""
  lastProc <- newRef Nothing
  sndOutput <- newRef ""
  sndProc <- newRef Nothing
  firstOutput <- newRef ""
  firstProc <- newRef Nothing
  let lastCmd = Command { command: "iostat", args: ["2", "3"], opts: defaultSpawnOptions, combineErr: true
                        , output: lastOutput, process: lastProc, save: true
                        }
      -- FIXME: top has to run in the shell, which means we can't use spawn (AFAICT)
      secondCmd = Command { command: "top", args: ["-d", "1", "-n", "2"], opts: defaultSpawnOptions, combineErr: true
                          , output: sndOutput, process: sndProc, save: true
                          }
      nameCmd = Command { command: "python", args: ["-u", "/home/stoner/dummy.py"], opts: defaultSpawnOptions
                        , combineErr: true, output: firstOutput, process: firstProc, save: true
                        }
      firstCmd = Command { command: "python", args: ["-u", "/home/stoner/dumdum.py"], opts: defaultSpawnOptions
                         , combineErr: true, output: firstOutput, process: firstProc, save: true
                         }
      -- This is how we "serialize" the functions.  If we did one runAff per launch, they would still be run in
      -- parallel.  So we essentially make a little function that contains what needs to be run
      runcmds = do
        pyproc <- launch firstCmd
        ioproc <- launch lastCmd
        pyproc2 <- launch nameCmd
        pure ioproc
  _ <- runAff defErr showOutput runcmds
  liftEff $ log "done"
