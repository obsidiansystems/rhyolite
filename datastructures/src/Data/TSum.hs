{-# Language DeriveGeneric #-}
{-# Language EmptyDataDecls #-}
{-# Language FlexibleInstances #-}
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Data.TSum where

import Control.Applicative
import Data.GADT.Compare
import Data.GADT.Show
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics
import Data.Typeable
import Data.Some
import Data.Constraint.Extras

data TSum t f g a = Public (f a) | Private t (g a)
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance (ArgDict c f, ArgDict c g) => ArgDict c (TSum t f g) where
  type ConstraintsFor (TSum t f g) c = (ConstraintsFor f c, ConstraintsFor g c)
  argDict (Public x) = argDict x
  argDict (Private _ x) = argDict x

instance (GEq f, GEq g) => GEq (TSum t f g) where
  geq (Public x) (Public y) = geq x y
  geq (Private _ x) (Private _ y) = geq x y
  geq _ _ = Nothing

instance (Ord t, GCompare f, GCompare g) => GCompare (TSum t f g) where
  gcompare (Public _) (Private _ _) = GLT
  gcompare (Private _ _) (Public _) = GGT
  gcompare (Public x) (Public y) = gcompare x y
  gcompare (Private t x) (Private t' y) =
    case compare t t' of
      LT -> GLT
      GT -> GGT
      EQ -> gcompare x y

instance (Show t, GShow f, GShow g) => GShow (TSum t f g) where
  gshowsPrec d (Public x) = showParen (d > 10)
    (showString "Public " . gshowsPrec 11 x)
  gshowsPrec d (Private t x) = showParen (d > 10)
    (showString "Private " . showsPrec 11 t . showString " " . gshowsPrec 11 x)

instance (ToJSON t, ToJSON (f a), ToJSON (g a)) => ToJSON (TSum t f g a) where
  toJSON (Public x) = object ["tag" .= String "Public", "contents" .= toJSON x]
  toJSON (Private t x) = object ["tag" .= String "Private", "contents" .= toJSON x, "token" .= toJSON t]

instance (FromJSON t, FromJSON (Some f), FromJSON (Some g)) => FromJSON (Some (TSum t f g)) where
  parseJSON v = do
    o <- parseJSON v
    let publicParser = do
          String "Public" <- o .: "tag"
          Some x <- o .: "contents"
          return (Some (Public x))
        privateParser = do
          String "Private" <- o .: "tag"
          Some x <- o .: "contents"
          t <- o .: "token"
          return (Some (Private t x))
    publicParser <|> privateParser <|> typeMismatch "TSum" v

data Empty a

deriving instance Eq (Empty a)
deriving instance Ord (Empty a)
deriving instance Show (Empty a)

instance GEq Empty where
  geq _ _ = Nothing

instance GCompare Empty where
  gcompare _ _ = error "gcompare applied for Empty" -- Shouldn't happen

instance GShow Empty where
  gshowsPrec = showsPrec

instance ToJSON (Empty a) where
  toJSON _ = toJSON () -- Shouldn't happen

instance FromJSON (Some Empty) where
  parseJSON v = typeMismatch "Some (Empty _)" v

instance ArgDict c Empty where
  type ConstraintsFor Empty c = ()
  argDict _ = error "argDict applied for Empty"
