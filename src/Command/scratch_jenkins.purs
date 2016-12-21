module Command.Starter where

import Prelude
import Node.Buffer as Buffer
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (noFlags)
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

-- | If the arg is Nothing, use the 2nd arg as the default which goes into Left, otherwise use the first arg in Right
orDefault :: Maybe String -> String -> Either String String
orDefault Nothing default = Left default
orDefault (Just r) _ = Right r

-- | Basically a wrapper around the spawn command.
-- | Takes a command, an array of arguments, and a record of options to pass to spawn.
-- | Sets a default exit and data handler
-- TODO: figure out a way to combine stdout and stderr
launch :: String -> Array String -> SpawnOptions -> CPEffect
launch cmd args opts = do
  let defaultExitHdlr exit = case exit of
        Normally x -> log $ "Got exit code of: " <> show x
        _ -> log $ "Exited due to signal: " <> show exit
      -- TODO: explain what this function does.
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

envVars :: Array String
envVars = [ "QUICK_BUILD"
          , "AUTOMATION_BRANCH"
          , "SERVER_BRANCH"
          , "RPM_URLS"
          , "JENKINS_URL"
          , "BUILD_NUMBER"
          , "JOB_NAME"
          , "CURRENT_XUNIT"
          ]

-- | The main script that kicks everything off
main :: ProcEff
main = do
  -- This just feels really ugly.  I should be able to put all these strings into a map.  I'd prefer to have these
  -- stored in a map where the keys are QUICK_BUILD etc, and the values are from the env.
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

      -- All of these will be concatenated into a Maybe String
      parts = [jenkins_url, Just "view/Scratch/job/", job_name, Just "/", build_number,
               Just "/artifact/test-output/testng-polarion.xml"]
      accum = foldl (\acc n -> append <$> acc <*> n) (Just "") parts

      current_xunit :: String
      current_xunit = case c_xunit of
                        (Just "") -> either id id (orDefault accum "")
                        (Just x) -> x
                        Nothing ->  either id id (orDefault accum "")

      re :: Either String Regex
      re = regex """^https""" noFlags
      current' = replace <$> re <*> pure "http" <*> pure current_xunit

  log $ "server_branch = " <> server_branch'
  log $ foldl (\acc n -> acc <> n <> ",") "git_args = [" git_args <> "]"
  log $ "current' = " <> case current' of
                           (Left e) -> e
                           (Right r) -> r

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
