-- | A module that handles a lot of the ugliness of asynchronous programming when calling a subprocess
-- | using the purescript-node-child-process module.  The problem with the purescript child process module is that it
-- | doesn't use Aff, and it doesn't handle the scenario where you have a dependency chain of calling several commands
-- | in a row or of saving the output of a command.
-- |
-- | The code here looks and is very mutable.  Part of the reason for that is because the onData function in purescript
-- | and in node has to take a callback which returns nothing (Unit).  This is a problem because how can we save any
-- | data?  That means we have to use a Ref or STRef (or maybe a Writer monad) to accumulate the data.  Moreover, this
-- | is a problem for callbacks.  The onExit function has the same problem.
module Command where

import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Control.Monad.Eff.Ref (Ref, REF, newRef, writeRef, readRef, modifyRef)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Node.Buffer (Buffer, BUFFER, toString)
import Node.ChildProcess (SpawnOptions, defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS, onExit, Exit(..), stdout)
import Node.Encoding (Encoding(..))
import Node.Stream (onData)
import Prelude (Unit, bind, pure, show, unit, ($), (<>))

-- | Represents how to run a command
newtype Command = Command  { command :: String
                           , args :: Array String
                           , opts :: SpawnOptions
                           , combineErr :: Boolean
                           , output :: Ref String -- FIXME: Make this a selectable type (eg Duplex, Socket, or other source)
                           , process :: Ref (Maybe ChildProcess)
                           }

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
  pure unit


type CmdEff e = ( cp :: CHILD_PROCESS
                , ref :: REF
                , err :: EXCEPTION
                , buffer :: BUFFER
                , console :: CONSOLE
                | e
                )


getOutput :: forall e. Command -> Eff (ref :: REF, console :: CONSOLE | e) Unit
getOutput (Command {output}) = do
  out <- readRef output
  log out


-- | Launch a child process along with an error callback and success callback
launchImpl :: forall e
            . Command
           -> (Error -> Eff (CmdEff e) Unit)
           -> (Command -> Eff (CmdEff e) Unit)
           -> Eff (CmdEff e) Unit
launchImpl cmd@(Command {command, args, opts, process, output}) ecb scb = do
  cp <- spawn command args opts
  writeRef process (Just cp)
  -- The output is accumulated to cmd.output each time the data event is caught
  onData (stdout cp) (onDataSave output)
  -- On finish, call
  onExit cp \exit -> case exit of
              Normally 0 -> (scb cmd)
              Normally x -> do
                let err = "command failed with ret code: " <> show x
                ecb $ error err
              BySignal _ -> do
                let err = "command failed due to signal: " <> (show exit)
                ecb $ error err

defErr :: forall e. Error -> Eff (console :: CONSOLE | e) Unit
defErr err = log $ message err

-- asynchronous launch
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
  let lastCmd = Command { command: "iostat"
                , args: ["2", "2"]
                , opts: defaultSpawnOptions
                , combineErr: true
                , output: lastOutput
                , process: lastProc
                }
      secondCmd = Command { command: "top"
                          , args: ["-d", "1", "-n", "2"]
                          , opts: defaultSpawnOptions
                          , combineErr: true
                          , output: sndOutput
                          , process: sndProc
                          }
      nameCmd = Command { command: "python"
                     , args: ["-u", """C:\projects\dummy.py"""]
                     , opts: defaultSpawnOptions
                     , combineErr: true
                     , output: firstOutput
                     , process: firstProc
                     }
      firstCmd = Command { command: "python"
                 , args: ["-u", """C:\projects\dumdum.py"""]
                 , opts: defaultSpawnOptions
                 , combineErr: true
                 , output: firstOutput
                 , process: firstProc
                 }
      runcmds = do
        pyproc <- launch firstCmd
        pyproc2 <- launch nameCmd
        pure pyproc2

  runAff defErr getOutput runcmds

  -- pyOut <- getOutput pyProc
  liftEff $ log "done"
