module Command.Starter where

import Prelude
import Control.Monad.Eff
import Node.ChildProcess as CP
import Node.ChildProcess (ChildProcess, CHILD_PROCESS, spawn)
import Data.Maybe (Maybe(..))
