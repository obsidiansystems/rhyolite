-- | This module contains things that are convenient for use in Common.Schema modules
module Obelisk.Common.Schema where

import Data.Constraint
import GHC.Generics

--TODO: This should be moved to a more general location
-- | NOTE: If you get an unexpected error on this, ensure that `a` is an
-- instance of Generic
type AllFieldsHave c a = GAllFieldsHave c (Rep a)

type family GAllFieldsHave c a :: Constraint where
  GAllFieldsHave c V1 = ()
  GAllFieldsHave c U1 = ()
  GAllFieldsHave c (a :+: b) = (GAllFieldsHave c a, GAllFieldsHave c b)
  GAllFieldsHave c (a :*: b) = (GAllFieldsHave c a, GAllFieldsHave c b)
  GAllFieldsHave c (K1 i a) = c a
  GAllFieldsHave c (M1 i t a) = GAllFieldsHave c a
