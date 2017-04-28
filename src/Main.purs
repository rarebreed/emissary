module Main where

import Data.Unit (Unit)
import Control.Monad.Eff (Eff)
--import Command (test)
import Command.Starter (scratch)
import Command.Command (ProcEffect)
import Scripts.Bootstrap (app)

--main :: forall e. Eff (ProcEffect e) Unit
main = do
  app
