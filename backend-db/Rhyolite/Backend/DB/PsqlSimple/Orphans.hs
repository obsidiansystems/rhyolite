{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.Backend.DB.PsqlSimple.Orphans where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k) where
    fromRow = (,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                           <*> field <*> field <*> field <*> field <*> field
                           <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    fromRow = (,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                            <*> field <*> field <*> field <*> field <*> field
                            <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    fromRow = (,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                             <*> field <*> field <*> field <*> field <*> field
                             <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    fromRow = (,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n, FromField o) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    fromRow = (,,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                               <*> field <*> field <*> field <*> field <*> field
                               <*> field <*> field <*> field <*> field <*> field

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p)

    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t]

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
