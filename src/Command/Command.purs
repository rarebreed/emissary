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

import Prelude

import Node.ChildProcess (SpawnOptions, defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS, onExit, Exit(..))
import Node.Stream (Duplex, onData)
import Node.Buffer (Buffer, BUFFER, toString)
import Node.Encoding (Encoding(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref (Ref, REF, newRef, writeRef, modifyRef)
import Control.Monad.Eff.Exception (Error, error, message)

import Data.Either
import Data.Maybe (Maybe(..))

-- | Represents how to run a command
newtype Command = Command  { command :: String
                            , args :: Array String
                            , opts :: SpawnOptions
                            , combineErr :: Boolean
                            , success :: forall a e. (a -> Eff e Unit)
                            , failure :: forall e. (Error -> Eff e Unit)
                            , output :: Ref String -- FIXME: Make this a selectable type (eg Duplex, Socket, or other source)
                            , process :: Ref ChildProcess
                            , next :: Maybe Command
                            }

-- | Function to save data from a buffer into an Ref
onDataSave :: forall e
            . Ref String
           -> Buffer
           -> Eff ( ref :: REF, buffer :: BUFFER, console :: CONSOLE | e) Unit
onDataSave ref buff = do
  bdata <- toString UTF8 buff
  modified <- modifyRef ref \current -> current <> bdata
  --log $ "Data as of now:\n" <> modified
  pure unit


defaultFailure :: forall e. Error -> Eff e Unit
defaultFailure err = do log $ message err

launch :: forall e. Command -> Eff e Unit
launch command = do
  let (Command cmd) = command
  cp <- spawn cmd.command cmd.args cmd.opts
  writeRef cmd.process cp
  -- The output is accumulated to cmd.output each time the data event is caught
  onData cp (onDataSave cmd.output)
  -- On success, call
  onExit cp \exit -> case exit of
              Normally 0 -> case cmd.next of
                              Just (Command x) -> cmd.success x
                              Nothing -> log "All done"
              Normally x -> cmd.failure $ error $ "command failed with ret code: " <> x
              BySignal _ -> cmd.failure $ error $ "command failed due to signal: " <> (show exit)

main = do
  let lastCmd = { command: "iostat"
                , args: ["2", "2"]
                , opts: defaultSpawnOptions
                , combineErr: true
                , output: newRef ""
                , success: \x -> log "Sucess"
                , failure: defaultFailure }
      firstCmd = { command: "python"
                 , args: ["-u", "/home/stoner/dumdum.py"]
                 , opts: defaultSpawnOptions
                 , combineErr: true
                 , output: newRef ""
                 , next: lastCmd
                 , success: \cmd -> launch cmd
                 , failure: defaultFailure }
  launch firstCmd
