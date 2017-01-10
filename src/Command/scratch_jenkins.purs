module Command.Starter where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, newRef, modifyRef)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef, runST)

import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (noFlags)

import Node.Buffer (BUFFER, toString, Buffer)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, Exit(..), SpawnOptions, defaultSpawnOptions, onExit, spawn, stdout)
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv, PROCESS)
import Node.Stream (onData)

-- | First arg is default if second arg is Nothing
default' :: String -> Maybe String -> String
default' _ (Just d) = d
default' def _ = def

-- | If the arg is Nothing, use the 2nd arg as the default which goes into Left, otherwise use the first arg in Right
orDefault :: Maybe String -> String -> Either String String
orDefault Nothing default = Left default
orDefault (Just r) _ = Right r

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

-- | Basically a wrapper around the spawn command.
-- | Takes a command, an array of arguments, and a record of options to pass to spawn.
-- | Sets a default exit and data handler
-- TODO: figure out a way to combine stdout and stderr
--launch :: String -> Array String -> SpawnOptions ->  Eff ( cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION
--                                                         , buffer :: BUFFER | e) ChildProcess
launch :: forall e
        . String
       -> Array String
       -> SpawnOptions
       -> Eff (cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) ChildProcess
launch cmd args opts = do
  cmd' <- spawn cmd args opts
  onExit cmd' defaultExitHdlr
  -- onData takes a readable stream, and a function which takes a Buffer and returns an Eff of Unit.  But I need to save
  -- off the output somewhere AND send it to log.
  onData (stdout cmd') (toString UTF8 >=> log)
  log $ "done with " <> cmd
  pure cmd'

-- | Function to save data from a buffer into an STRef
onDataSave :: forall e
            . Ref String
           -> Buffer
           -> Eff ( ref :: REF, buffer :: BUFFER, console :: CONSOLE | e) Unit
onDataSave ref buff = do
  bdata <- toString UTF8 buff
  modified <- modifyRef ref \current -> current <> bdata
  --log $ "Data as of now:\n" <> modified
  pure unit

-- | Return type object that contains the ChildProcess value and a mutable saved state
-- newtype SubProc h = SubProc { process :: ChildProcess, saved :: STRef h String }
-- | A command which will launch a subprocess and save the stdout of the running process.
-- | Takes the command to run, an array of arguments, and a record of the options for the command to run with
run :: forall e
     . Ref String              -- The saved output of the command
    -> String                      -- The command to run
    -> Array String                -- The array of arguments to pass to the command
    -> SpawnOptions                -- An object containing the options to run the process with
    -> Eff ( console :: CONSOLE
           , cp :: CHILD_PROCESS
           , buffer :: BUFFER
           , err :: EXCEPTION
           , ref :: REF
           | e
           )
           ChildProcess
run st cmd args opts = do
  procState <- newRef ""
  let cb = onDataSave st
      eh = onExitState procState
  cmd' <- spawn cmd args opts
  onExit cmd' eh
  onData (stdout cmd') cb
  log $ "done with " <> cmd
  pure cmd'

-- TODO: Write a function that can pipe information to the stdin of the child process, for example if the child process
-- prompts in an interactive manner


-- FIXME: replace this with a regular http GET instead of shelling out to wget
-- | gets the pub and pvt keys
getAutoKey :: forall e
            . String                      -- | name of the key
           -> String                      -- | where to save file to
           -> Eff ( cp :: CHILD_PROCESS
                  , console :: CONSOLE
                  , err :: EXCEPTION
                  , buffer :: BUFFER
                  | e) ChildProcess
getAutoKey keyname output = do
  let args = ["-nv", "http://auto-services.usersys.redhat.com/rhsm/keys/" <> keyname, "-O", output]
  launch "wget" args defaultSpawnOptions

-- | Does a git checkout in the current working directory
gitCheckout :: forall e
             . Array String
            -> Eff ( cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) ChildProcess
gitCheckout args = do
  launch "git" args defaultSpawnOptions

-- | Remove reults from a prior run and avoid failed jobs due to missing result files
cleanup :: forall e
         . Eff ( process :: PROCESS
               , cp :: CHILD_PROCESS
               , console :: CONSOLE
               , err :: EXCEPTION
               , buffer :: BUFFER | e) Unit
cleanup = do
  workspace <- lookupEnv "WORKSPACE"
  -- TODO: replace this with purescript-node-fs unlink command
  launch "rm" ["-rf", "test-output/*"] defaultSpawnOptions
  -- TODO: replace this with the purescript-node-fs mkdir
  launch "mkdir" ["-p", "test-output/html"] defaultSpawnOptions
  launch "touch" ["test-output/registration_report.html", "test-output/hw_info_dump.tar"] defaultSpawnOptions
  log "end of cleanup"

-- | compile with lein
compile :: forall e. Eff (cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) Unit
compile = do
  launch "lein" ["clean"] defaultSpawnOptions
  launch "lein" ["deps"] defaultSpawnOptions
  launch "lein" ["compile"] defaultSpawnOptions
  log "end of lein compile"

-- TODO: Need to redo this since launch returns Unit instead of String.  Need to change the onData handler so that
-- it saves the stdout to a string.
getClasspath :: forall e. Eff (cp :: CHILD_PROCESS, console :: CONSOLE, err :: EXCEPTION, buffer :: BUFFER | e) String
getClasspath = do
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
main :: forall e. Eff ( process :: PROCESS
                      , cp :: CHILD_PROCESS
                      , console :: CONSOLE
                      , err :: EXCEPTION
                      , buffer :: BUFFER
                      | e) Unit
main = do
  -- This just feels really ugly.  I should be able to put all these strings into a map.  I'd prefer to have these
  -- stored in a map where the keys are QUICK_BUILD etc, and the values are from the env.
  quick_build <- lookupEnv "QUICK_BUILD"
  auto_branch <- lookupEnv "AUTOMATION_BRANCH"
  server_branch <- lookupEnv "SERVER_BRANCH"
  test_suites <- lookupEnv "TEST_SUITES"
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

  -- Kick off the test
  -- java -cp "`lein classpath`" "${JAVAARGS[@]}" org.testng.TestNG "${TESTNGARGS[@]}"  $TEST_SUITES || true
  -- TODO: Set any JAVA_ARGS needed.  Since we are using the automation.properties file, we need some way to set this
  -- TODO: Set the TESTNGARGS and get the TEST_SUITES from the test_suites Maybe String
  -- TODO: Need to create a version of spawn with an onData that captures stdout to a string.  However onData seems to
  --       return Unit.

  {- There will be a file called polarize.properties that exists in $WORKSPACE.  If CURRENT_XUNIT from the job has no
  value, use the default.  Then write the following into the polarize.properties file.

  echo "CURRENT_XUNIT=${CURRENT_XUNIT}" >> polarize.properties
  echo "JENKINSJOBS=${BUILD_URL}" >> polarize.properties
  echo "NOTES=${BUILD_URL}TestNG_Report" >> polarize.properties
  -}

  log "Done"

testing :: Eff ( console :: CONSOLE
               , buffer :: BUFFER
               , cp :: CHILD_PROCESS
               , err :: EXCEPTION
               , ref :: REF
               ) Unit
testing = do
  st <- newRef ""
  proc <- run st "iostat" ["2", "5"] defaultSpawnOptions
  -- TODO: The problem is that if I try to get the st like this:
  output <- readRef st
  log output
  -- the problem is that the proc ChildProcess hasn't finished running, but I'm already asking for the state (which has
  -- the so far saved output from the proc process).  So, I believe what I need to do here is use purescript-aff and
  -- call run via the later function.
  pure unit
