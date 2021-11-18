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
  , queryQ, executeQ, executeQ_, sqlQ, traceQueryQ, traceExecuteQ, traceExecuteQ_
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

---------------------------------
-- PostgreSQL.Simple instances --
---------------------------------

defaultQQ :: String -> QuasiQuoter
defaultQQ name = QuasiQuoter
  { quotePat = error $ name <> ": quasiquoter used in pattern context"
  , quoteType = error $ name <> ": quasiquoter used in type context"
  , quoteExp = error $ name <> ": quasiquoter used in expression context"
  , quoteDec = error $ name <> ": quasiquoter used in declaration context"
  }

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'query'.
queryQ :: QuasiQuoter
queryQ = (defaultQQ "queryQ") { quoteExp = appE [| uncurry query |] . sqlQExp }

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'execute'.
executeQ :: QuasiQuoter
executeQ = (defaultQQ "executeQ") { quoteExp = appE [| uncurry execute |] . sqlQExp }

executeQ_ :: QuasiQuoter
executeQ_ = (defaultQQ "executeQ_") { quoteExp = appE [| uncurry execute_ |] . sqlQExp }

traceQueryQ :: QuasiQuoter
traceQueryQ = (defaultQQ "traceQueryQ") { quoteExp = appE [| uncurry traceQuery |] . sqlQExp }

traceExecuteQ :: QuasiQuoter
traceExecuteQ = (defaultQQ "traceExecuteQ") { quoteExp = appE [| uncurry traceExecute |] . sqlQExp }

traceExecuteQ_ :: QuasiQuoter
traceExecuteQ_ = (defaultQQ "traceExecuteQ_") { quoteExp = appE [| uncurry traceExecute_ |] . sqlQExp }


-- | This quasiquoter takes a SQL query with named arguments in the form "?var" and generates a pair
-- consisting of the Query string itself and a tuple of variables in corresponding order.
--
-- For example: uncurry query [sqlQ| SELECT * FROM 'Book' b WHERE b.title = ?title AND b.author = ?author |]
--
-- will be equivalent to query [sql| SELECT * FROM 'Book' b WHERE b.title = ? AND b.author = ? |] [toField title, toField author]
sqlQ :: QuasiQuoter
sqlQ = (defaultQQ "sqlQ") { quoteExp = sqlQExp }

sqlQExp :: String -> Q Exp
sqlQExp s =
  let (s',vs) = extractVars s
  in tupE [quoteExp sql s', sigE (listE $ map (appE (varE 'toField) . varE) vs) [t| [Action] |]]

extractVars :: String -> (String, [Name])
extractVars = extractVars'
  where
    extractVars' [] = ([],[])
    extractVars' ('?':s') =
      let [(var,rest)] = lex s'
          (s'',vars) = extractVars' rest
      in ('?':s'', mkName var : vars)
    extractVars' s' =
      let (pre,post) = break (=='?') s'
          (s'',vars) = extractVars' post
      in (pre ++ s'', vars)
