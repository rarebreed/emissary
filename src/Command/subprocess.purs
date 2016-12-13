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

-- | Basically a wrapper around the spawn command.  Sets a default exit and data handler
-- TODO: figure out a way to combine stdout and stderr
launch :: String -> Array String -> SpawnOptions -> CPEffect
launch cmd args opts = do
  let defaultExitHdlr exit = case exit of
        Normally x -> log $ "Got exit code of: " <> show x
        _ -> log $ "Exited due to signal: " <> show exit
      defaultDataHdlr cp = onData (stdout cp) (Buffer.toString UTF8 >=> log)
  cmd' <- spawn cmd args opts
  onExit cmd' defaultExitHdlr
  defaultDataHdlr cmd'
  log $ "done with " <> cmd

-- | gets the pub and pvt keys
-- FIXME: replace this with a regular http GET instead of shelling out to wget
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
  -- TODO: replace this with purescript-node-fs unlink command
  launch "rm" ["-rf", "test-output/*"] defaultSpawnOptions
  -- TODO: replace this with the purescript-node-fs mkdir
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

{- Make a function set_env_vars which injects these variables into the global env

POLARIZE_VERSION=0.5.4-SNAPSHOT
PROJECT_ID=RHEL6
CURRENT_XUNIT=`echo "${JENKINS_URL}view/Scratch/job/${JOB_NAME}/${BUILD_NUMBER}/artifact/test-output/testng-polarion.xml" | sed 's/http/https/g'`
PLANNEDIN_MILESTONE="6.9 Pre-testing"
TEMPLATE_ID="sean toner master template test"
TESTRUN_TITLE="Polarize testing"
PROJECT_NAME="RHSM "
NEW_XUNIT="/tmp/modified-testng-polarion.xml"    # Where to write  a temporary xunit xml based off of CURRENT_XUNIT
-}

{- Make a function create_properties which writes out all of this to the test-output/polarize.properties file
create_properties :: String -> Array String -> CPEffect
create_properties fpath lines = map (\fpath line -> writeTo fpath line) lines

create_properties "test-output/polarize.properties" lines
##########################################################
# Write out the Project and path to artifact
# Take care to wrap anything with space in it with \"${THING_WITH_SPACE}\"
##########################################################
echo "POLARIZE_VERSION=${POLARIZE_VERSION}" >> test-output/polarize.properties
echo "PROJECT_ID=${PROJECT_ID}" >> test-output/polarize.properties
echo "CURRENT_XUNIT=${CURRENT_XUNIT}" >> test-output/polarize.properties
echo "PLANNED_IN_MILESTONE=\"${PLANNEDIN_MILESTONE}\"" >> test-output/polarize.properties
echo "TEMPLATE_ID=\"${TEMPLATE_ID}\"" >> test-output/polarize.properties
echo "TESTRUN_TITLE=\"${TESTRUN_TITLE}\"" >> test-output/polarize.properties
echo "PROJECT_NAME=\"${PROJECT_NAME}" >> test-output/polarize.properties
echo "NEW_XUNIT=${NEW_XUNIT}" >> test-output/polarize.properties
echo "JENKINSJOBS=${BUILD_URL}" >> test-output/polarize.properties
echo "NOTES=${BUILD_URL}TestNG_Report" >> test-output/polarize.properties
-}




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

  -- Do the checkout, grab the keys, cleanup and compile
  git_checkout git_args
  get_auto_key "rhsm-qe.pub" ".ssh/rhsm-qe.pub"
  get_auto_key "rhsm-qe" ".ssh/rhsm-qe"
  cleanup
  compile
  -- set_env_vars
  -- create_properties

  log "Done"
