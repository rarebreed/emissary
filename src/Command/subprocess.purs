module Command.Starter where

import Prelude
import Control.Monad.Eff (Eff)
import Node.Buffer as Buffer
import Node.Buffer (BUFFER)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, SpawnOptions, defaultSpawnOptions, Exit(..), spawn, onExit, stdout)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv, PROCESS)
import Node.Stream (onData)

-- I need to understand extensible effects better, because this should be the more generalized type of ProcEff.
type CPEffect = forall e.  Eff (cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) Unit
type ProcEff = forall e. Eff ( process :: PROCESS
                             , cp :: CHILD_PROCESS
                             , console :: CONSOLE
                             , err :: EXCEPTION
                             , buffer :: BUFFER | e
                             ) Unit

-- | First arg is default if second arg is Nothing
default' :: String -> Maybe String -> String
default' _ (Just d) = d
default' def _ = def

launch :: String -> Array String -> SpawnOptions -> CPEffect
launch cmd args opts = do
  cmd' <- spawn cmd args opts
  onExit cmd' defaultExitHdlr
  defaultDataHdlr cmd'
  log $ "done with " <> cmd


-- | a basic exit handler that simply informs how a program exited
defaultExitHdlr :: Exit -> CPEffect
defaultExitHdlr exit = case exit of
    Normally x -> log $ "Got exit code of: " <> show x
    _ -> log $ "Exited due to signal: " <> show exit

-- | A Basic data event handler for a child process.  Takes the data from the stdout and pumps to console
defaultDataHdlr :: ChildProcess -> CPEffect
defaultDataHdlr cp = onData (stdout cp) (Buffer.toString UTF8 >=> log)

-- TODO: Create function that gets the pub and pvt keys
get_auto_key :: String -> String -> CPEffect
get_auto_key keyname output = do
  let args = ["-nv", "http://auto-services.usersys.redhat.com/rhsm/keys/" <> keyname, "-O", output]
  launch "wget" args defaultSpawnOptions

-- | Does a git checkout in the current working directory
git_checkout :: Array String -> CPEffect
git_checkout args = do
  launch "git" args defaultSpawnOptions

-- | Remove reults from a prior run and avoid failed jobs due to missing result files
cleanup :: ProcEff
cleanup = do
  workspace <- lookupEnv "WORKSPACE"
  -- using bind notation here instead of <-
  launch "rm" ["-rf", "test-output/*"] defaultSpawnOptions
  -- replace this with the purescript-node-fs mkdir
  launch "mkdir" ["-p", "test-output/html"] defaultSpawnOptions
  launch "touch" ["test-output/registration_report.html", "test-output/hw_info_dump.tar"] defaultSpawnOptions
  log "end of cleanup"


-- | compile with lein
compile :: CPEffect
compile = do
  launch "lein" ["clean"] defaultSpawnOptions
  launch "lein" ["deps"] defaultSpawnOptions
  launch "lein" ["compile"] defaultSpawnOptions
  log "end of lein compile"

-- | The main script that kicks everything off
main :: ProcEff
main = do
  quick_build <- lookupEnv "QUICK_BUILD"
  auto_branch <- lookupEnv "AUTOMATION_BRANCH"
  server_branch <- lookupEnv "SERVER_BRANCH"
  rpm_urls <- lookupEnv "RPM_URLS"
  let auto_branch' = case auto_branch of
                       (Just ab) -> ab
                       _ -> ""
      server_branch' = if (default' "false" quick_build) == "false"
                         then ""
                         else default' "master" server_branch
      rpm_urls' = if (default' "" quick_build) == ""
                    then ""
                    else default' "" rpm_urls

      git_args = ["checkout", "upstream/" <> (default' "master" auto_branch)]

  log $ "server_branch = " <> server_branch'
  log $ foldl (\acc n -> acc <> n <> ",") "git_args = [" git_args <> "]"

  -- do the git command
  git_checkout git_args

  -- get the public and private keys
  get_auto_key "rhsm-qe.pub" ".ssh/rhsm-qe.pub"
  get_auto_key "rhsm-qe" ".ssh/rhsm-qe"

  log "Done"
