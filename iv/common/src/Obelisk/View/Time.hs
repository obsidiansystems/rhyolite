module Obelisk.View.Time where

-- | Logical times within the system.  Each time we process a delta to the database, the time goes up by 1.  We rely on times being contiguous in some places, but not everywhere, because most nodes in the dataflow graph will not do anything for most Times.
type Time = Int
