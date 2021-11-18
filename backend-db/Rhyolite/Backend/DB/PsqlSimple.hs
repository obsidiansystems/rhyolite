{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-deprecations #-}

module Rhyolite.Backend.DB.PsqlSimple
  ( Psql (..)
  , In (..), Only (..), Values (..)
  , Binary (..), (:.)(..), PGArray (..)
  , ToRow (..), FromRow (..)
  , ToField (..), FromField (..)
  , Query (..), sql, traceQuery, traceExecute, traceExecute_
  , liftWithConn
  , iquery, iexecute, iexecute_, itraceQuery, itraceExecute, itraceExecute_
  ) where

import Database.PostgreSQL.Simple.Class
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.Groundhog
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action, ToField, toField)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.Types
import Language.Haskell.TH (Exp, Name, Q, appE, listE, mkName, sigE, tupE, varE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Database.PostgreSQL.Simple.SqlQQ.Interpolated

