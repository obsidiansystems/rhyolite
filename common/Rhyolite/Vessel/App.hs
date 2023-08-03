{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Rhyolite.Vessel.App where

import Rhyolite.Vessel.Types
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Api

type RhyoliteAuthAppC app = HasRhyoliteAuth
  (AuthCredential app)
  (PublicV app)
  (PrivateV app)
  (PersonalV app)

-- | optional class to help organise the types of a rhyolite authenticated app.
class RhyoliteAuthAppC app => RhyoliteAuthApp app where
  type AuthCredential app :: *

  type PublicApi app :: (* -> *)
  type PrivateApi app :: (* -> *)

  type PrivateV app :: ((* -> *) -> *)
  type PersonalV app :: ((* -> *) -> *)
  type PublicV app :: ((* -> *) -> *)

-- | The full view selector which has a public, private, and personal part.
type FullAppV app = FullV (AuthCredential app) (PublicV app) (PrivateV app) (PersonalV app)

-- | The full view selector from the point of view of a particular authenticated identity which
-- may or may not be valid; the result of a query can fail.
type FullAppAuthErrorV app = FullAuthErrorV (PublicV app) (PrivateV app) (PersonalV app)

-- | The full view selector from the point of view of a particular authenticated identity which
-- assumes that the identity is valid and so the query cannot fali.
type FullAppAuthV app = AuthenticatedV (PublicV app) (PrivateV app) (PersonalV app)

type FullApi app = ApiRequest (AuthCredential app) (PublicApi app) (PrivateApi app)
type FullAuthApi app = ApiRequest () (PublicApi app) (PrivateApi app)
