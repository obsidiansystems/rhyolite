{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.Backend.DB.PsqlSimple.Orphans where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t, ToField u)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t, toField u]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t, ToField u, ToField v)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t, toField u, toField v]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t, ToField u, ToField v, ToField w)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t, toField u, toField v, toField w]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t, ToField u, ToField v, ToField w, ToField x)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t, toField u, toField v, toField w, toField x]
