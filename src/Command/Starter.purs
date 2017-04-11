module Command.Starter where

import Prelude

import Command (CmdEff, Command(..), defErr, getOutput, launch, makeOptsWithDir, showOutput)

import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, writeRef, newRef)

import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (noFlags)

import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS, Exit(..), SpawnOptions, defaultSpawnOptions)
import Node.Process (lookupEnv, PROCESS)


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

type ProcEffect e = ( process :: PROCESS
                    , ref :: REF
                    , buffer :: BUFFER
                    , cp :: CHILD_PROCESS
                    , exception :: EXCEPTION
                    , console :: CONSOLE
                    | e
                    )

-- | Remove results from a prior run and avoid failed jobs due to missing result files
-- cleanup :: forall e. Eff (ProcEffect e) (Canceler (ProcEffect e))
cleanAndCompile :: forall e. String ->  Eff (ProcEffect e) String
cleanAndCompile dir = do
  workspace <- lookupEnv "WORKSPACE"
  case workspace of
    (Just s) -> log $ "Workspace is " <> s
    Nothing -> log "No workspace defined"
  rm <- makeDefCmd "rm" ["-rf", "test-output/*"] Nothing --(makeOptsWithDir dir)
  mkdir <- makeDefCmd "mkdir" ["-p", "test-output/html"] Nothing --(makeOptsWithDir dir)
  touch <- makeDefCmd "touch" ["test-output/registration_report.html", "test-output/hw_info_dump.tar"] Nothing --(makeOptsWithDir dir)
  leinClean <- makeDefCmd "lein" ["clean"]  (makeOptsWithDir dir)
  leinDeps <- makeDefCmd "lein" ["deps"] (makeOptsWithDir dir)
  leinCompile <- makeDefCmd "lein" ["compile"] (makeOptsWithDir dir)
  leinCP <- makeDefCmd "lein" ["classpath"] (makeOptsWithDir dir)
  let cleanup' = do
        rm' <- launch rm
        mkdir' <- launch mkdir
        touch' <- launch touch
        clean <- launch leinClean
        deps <- launch leinDeps
        lc <- launch leinCompile
        lpcmd <- launch leinCP
        pure touch
  cancelClean <- runAff defErr showOutput cleanup'
  classpath <- getOutput leinCP
  pure classpath

-- | compile with lein
compile :: forall e. Eff (ref :: REF | e) (Aff (CmdEff e) Command)
compile = do
  leinClean <- makeDefCmd "lein" ["clean"] Nothing
  leinDeps <- makeDefCmd "lein" ["deps"] Nothing
  leinCompile <- makeDefCmd "lein" ["compile"] Nothing
  let compile' = do
        clean <- launch leinClean
        deps <- launch leinDeps
        lc <- launch leinCompile
        pure leinCompile
  pure compile'

-- | Function to create a Command to retrieve a key
getAutoKey :: forall e. String -> String -> Eff (ref :: REF | e) Command
getAutoKey keyname output = do
  ak <- makeDefCmd "wget" ["-nv", "http://auto-services.usersys.redhat.com/rhsm/keys/" <> keyname, "-O", output] Nothing
  pure ak

-- | Gets the public and private keys
getAutoKeys :: forall e. Eff (ProcEffect e) (Array Command)
getAutoKeys = do
  pubCmd <- getAutoKey "rhsm-qe.pub" "/tmp/rhsm-qe.pub"
  log $ show pubCmd
  pvtCmd <- getAutoKey "rhsm-qe" "/tmp/rhsm-qe"
  let getAutoKeys' = do
        akPub <- launch pubCmd
        akPvt <- launch pvtCmd
        pure akPvt
  cancel <- runAff defErr showOutput getAutoKeys'
  pure [pubCmd, pvtCmd]

-- TODO: Need to redo this since launch returns Unit instead of String.  Need to change the onData handler so that
-- it saves the stdout to a string.
getClasspath :: forall e. Eff (ref :: REF| e) Command
getClasspath = do
  leinCP <- makeDefCmd "lein" ["classpath"] (makeOptsWithDir "/home/stoner/Projects/rhsm-qe")
  pure leinCP

envVars :: Array String
envVars = [ "QUICK_BUILD", "AUTOMATION_BRANCH", "SERVER_BRANCH", "RPM_URLS", "JENKINS_URL", "BUILD_NUMBER"
          , "JOB_NAME", "CURRENT_XUNIT"]

javaArgs :: String -> Array String
javaArgs cp = [ "-cp", cp
              , "-Xmx4096m"
              ]

listeners :: Array String
listeners = [ "com.redhat.qe.auto.bugzilla.BugzillaTestNGListener"
            , "org.uncommons.reportng.HTMLReporter,org.testng.reporters.XMLReporter"
            ]

testNGArgs :: Array String
testNGArgs = [ "-usedefaultlisteners"
             , "false"
             , "-reporter"
             , "com.github.redhatqe.polarize.junitreporter.XUnitReporter"
             , "-listener"
             , foldl (\acc n -> acc <> "," <> n) "com.redhat.qe.auto.testng.TestNGListener" listeners
             ]


-- | The main script that kicks everything off
scratch :: forall e. Eff ( process :: PROCESS
                         , cp :: CHILD_PROCESS
                         , console :: CONSOLE
                         , exception :: EXCEPTION
                         , buffer :: BUFFER
                         , ref :: REF
                         | e
                         ) Unit
scratch = do
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
      server_branch' = if (default' "false" quick_build) == "false" then "" else default' "master" server_branch
      rpm_urls' = if (default' "" quick_build) == "" then "" else default' "" rpm_urls
      git_args = ["checkout", "upstream/" <> (default' "master" auto_branch)]

      -- All of these will be concatenated into a Maybe String
      parts = [jenkins_url, Just "view/Scratch/job/", job_name, Just "/", build_number
              , Just "/artifact/test-output/testng-polarion.xml"]
      accum = foldl (\acc n -> append <$> acc <*> n) (Just "") parts
      workdir = "/home/stoner/Projects/rhsm-qe"

      current_xunit :: String
      current_xunit = case c_xunit of
                        (Just "") -> either id id (orDefault accum "")
                        (Just x) -> x
                        Nothing ->  either id id (orDefault accum "")

      re :: Either String Regex
      re = regex """^https""" noFlags
      current' = replace <$> re <*> pure "http" <*> pure current_xunit

  log $ "auto_branch = " <> auto_branch'
  log $ "server_branch = " <> server_branch'
  log $ foldl (\acc n -> acc <> n <> ",") "git_args = [" git_args <> "]"
  log $ "current' = " <> case current' of
                           (Left e) -> e
                           (Right r) -> r

  -- FIXME:  Currently, the getting of the keys will be run concurrently with cleaning, compiling and executing java
  -- however, these should be serialized.
  log "Getting the public and private keys"
  _ <- getAutoKeys
  log "Cleaning test-output"
  -- Cleaning and compiling will be run simultaneously with getting the auto keys
  wd <- cleanAndCompile workdir
  log $ show "Cleaned up" <> wd


  -- Kick off the test
  -- java -cp "`lein classpath`" "${JAVAARGS[@]}" org.testng.TestNG "${TESTNGARGS[@]}"  $TEST_SUITES || true
  -- TODO: Set any JAVA_ARGS needed.  Since we are using the automation.properties file, we need some way to set this
  -- TODO: Set the TESTNGARGS and get the TEST_SUITES from the test_suites Maybe String

  {- There will be a file called polarize.properties that exists in $WORKSPACE.  If CURRENT_XUNIT from the job has no
  value, use the default.  Then write the following into the polarize.properties file.

  echo "CURRENT_XUNIT=${CURRENT_XUNIT}" >> polarize.properties
  echo "JENKINSJOBS=${BUILD_URL}" >> polarize.properties
  echo "NOTES=${BUILD_URL}TestNG_Report" >> polarize.properties
  -}
