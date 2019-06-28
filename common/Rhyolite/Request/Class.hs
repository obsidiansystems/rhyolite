{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rhyolite.Request.Class where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Constraint.Forall
import Data.Constraint.Extras
import Data.Some

type Request r = (ForallF ToJSON r, Has ToJSON r, FromJSON (Some r), Has FromJSON r)
