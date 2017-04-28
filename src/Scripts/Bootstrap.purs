-- | This module will "bootstrap" a starter project for node

module Scripts.Bootstrap where

import Prelude (bind, (<>), pure, map, ($), discard)
import Command.Command
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (runAff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Path (FilePath)
import Node.FS.Aff (mkdir)

-- | Installs packages from npm
npm :: forall e. Array String -> Maybe String -> String -> Eff (CmdEff e) Command
npm opts dir pkg = do
  cmd <- makeDefCmd "npm" (["install"] <> opts <> [pkg]) $ Just (setOptDir defaultSpawnOptions dir)
  let npm' = do
        res <- launch cmd
        pure cmd
  _ <- runAff defErr showOutput npm'
  pure cmd

runAffC = runAff defErr showOutput


-- | Creates a new package.json with defaults
npminit :: forall e. Maybe String -> Eff (CmdEff e) Command
npminit dir = do
  cmd <- makeDefCmd "npm" ["init", "-y"] $ Just (setOptDir defaultSpawnOptions dir)
  let npmcmd = do
        res <- launch cmd
        pure cmd
  _ <- runAff defErr showOutput npmcmd
  pure cmd


-- | The program
app = launchAff do
  -- Make the test directory
  dir <- (\n -> do
             _ <- mkdir n
             pure n) "/home/stoner/Projects/testpure"

  -- Run the npm init command in the test directory
  initcmd <- liftEff $ makeDefCmd "npm" ["init", "-y"]
                     $ Just (setOptDir defaultSpawnOptions (Just dir))
  initCmd' <- launch initcmd

  -- Run npm install --save for each pkg in pkgs, and npm install --save-dev for each pkg in devpkgs
  let pkgs = ["react", "react-redux", "react-router"]
      devpkgs = ["webpack", "webpack-dev-server", "babel", "babel-core"]
      pkg_opts = ["--save"]
      devpkg_opts = ["--save-dev"]
      npmC = npm ["--save"] (Just dir)
      npmCD = npm ["--save-dev"] (Just dir)

  liftEff $ log "Done"
