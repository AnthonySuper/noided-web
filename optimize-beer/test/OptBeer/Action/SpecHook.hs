module OptBeer.Action.SpecHook (hook) where

import Data.Pool (Pool)
import Hasql.Connection (Connection)
import OptBeer.Action.SpecHelper
import Test.Hspec

hook :: SpecWith TransactionRunner -> SpecWith (Pool Connection)
hook = usingTransactionRunner
