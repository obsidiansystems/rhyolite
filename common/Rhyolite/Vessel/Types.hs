{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rhyolite.Vessel.Types where

import Data.Vessel.Class
import Data.Functor.Identity
import Data.Functor.Const
import Data.Patch (Group)
import Reflex.Query.Class

import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.ErrorV
import Rhyolite.Vessel.AuthMapV

type RhyoliteAuthViewC v =
  ( View v
  , Group (v (Const SelectedCount))
  , Eq (v (Const SelectedCount))
  , Semigroup (v Identity)
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  , EmptyView v
  )

type HasRhyoliteAuth token publicV privateV personalV =
  ( Ord token
  , RhyoliteAuthViewC publicV
  , RhyoliteAuthViewC privateV
  , RhyoliteAuthViewC personalV
  )

-- | The full view selector which has a public, private, and personal part.
type FullV token publicV privateV personalV = AuthenticatedV publicV (AuthMapV token privateV) (AuthMapV token personalV)

-- | The full view selector from the point of view of a particular authenticated identity which
-- may or may not be valid; the result of a query can fail.
type FullAuthErrorV publicV privateV personalV = AuthenticatedV publicV (ErrorV () privateV) (ErrorV () personalV)
