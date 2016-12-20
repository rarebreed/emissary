module Command.Starter where

import Prelude
import Node.Buffer as Buffer
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.Node.Node (lookupNamespaceURI)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foreign.Keys (keys)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS, SpawnOptions, defaultSpawnOptions, Exit(..), spawn, onExit, stdout)
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
type EffProc = forall e. Eff ( process :: PROCESS
                             , cp :: CHILD_PROCESS
                             , console :: CONSOLE
                             , err :: EXCEPTION
                             , buffer :: BUFFER | e
                             ) String

-- | First arg is default if second arg is Nothing
default' :: String -> Maybe String -> String
default' _ (Just d) = d
default' def _ = def

-- | Basically a wrapper around the spawn command.
-- | Takes a command, an array of arguments, and a record of options to pass to spawn.
-- | Sets a default exit and data handler
-- TODO: figure out a way to combine stdout and stderr
launch :: String -> Array String -> SpawnOptions -> CPEffect
launch cmd args opts = do
  let defaultExitHdlr exit = case exit of
        Normally x -> log $ "Got exit code of: " <> show x
        _ -> log $ "Exited due to signal: " <> show exit
      -- TODO: explain what this function does
      defaultDataHdlr cp = onData (stdout cp) (Buffer.toString UTF8 >=> log)
  cmd' <- spawn cmd args opts
  onExit cmd' defaultExitHdlr
  defaultDataHdlr cmd'
  log $ "done with " <> cmd

-- | gets the pub and pvt keys
-- FIXME: replace this with a regular http GET instead of shelling out to wget
getAutoKey :: String -> String -> CPEffect
getAutoKey keyname output = do
  let args = ["-nv", "http://auto-services.usersys.redhat.com/rhsm/keys/" <> keyname, "-O", output]
  launch "wget" args defaultSpawnOptions

-- | Does a git checkout in the current working directory
gitCheckout :: Array String -> CPEffect
gitCheckout args = do
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

getClasspath :: forall e. Eff (cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) String
getClasspath = do
  -- TODO: Need to redo this since launch returns Unit instead of String.  Need to change the onData handler so that
  -- it saves the stdout to a string.
  launch "lein" ["classpath"] defaultSpawnOptions
  pure "TODO"


-- | The main script that kicks everything off
main :: ProcEff
main = do
  -- This just feels really ugly.  I should be able to put all these strings into a map.
  quick_build <- lookupEnv "QUICK_BUILD"
  auto_branch <- lookupEnv "AUTOMATION_BRANCH"
  server_branch <- lookupEnv "SERVER_BRANCH"
  rpm_urls <- lookupEnv "RPM_URLS"
  jenkins_url <- lookupEnv "JENKINS_URL"
  build_number <- lookupEnv "BUILD_NUMBER"
  job_name <- lookupEnv "JOB_NAME"
  c_xunit <- lookupEnv "CURRENT_XUNIT"
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
      -- TODO: Figure out how to lift this
      default = jenkins_url <> "view/Scratch/job/" <> job_name <> "/" <>  build_number <> "/artifact/test-output/testng-polarion.xml"
      current_xunit = case c_xunit of
                        Nothing ->  default
                        (Just "") -> default
                        (Just x) -> x
      re = regex """^https"""
      current' = replace re "http" current_xunit

  log $ "server_branch = " <> server_branch'
  log $ foldl (\acc n -> acc <> n <> ",") "git_args = [" git_args <> "]"
  log $ "current' = " <> current'

  -- Do the checkout, grab the keys, cleanup and compile
  gitCheckout git_args
  getAutoKey "rhsm-qe.pub" ".ssh/rhsm-qe.pub"
  getAutoKey "rhsm-qe" ".ssh/rhsm-qe"
  cleanup
  compile

  -- Set all the JAVA_ARGS

  -- Kick off the test
  -- java -cp "`lein classpath`" "${JAVAARGS[@]}" org.testng.TestNG "${TESTNGARGS[@]}"  $TEST_SUITES || true


  {- There will be a file called polarize.properties that exists in $WORKSPACE.  If CURRENT_XUNIT from the job has no
  value, use the default.  Then write the following into the polarize.properties file.

  echo "CURRENT_XUNIT=${CURRENT_XUNIT}" >> polarize.properties
  echo "JENKINSJOBS=${BUILD_URL}" >> polarize.properties
  echo "NOTES=${BUILD_URL}TestNG_Report" >> polarize.properties
  -}

  log "Done"
