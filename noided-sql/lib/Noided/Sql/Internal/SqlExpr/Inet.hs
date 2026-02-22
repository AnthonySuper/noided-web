{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Inet where

import Data.IP (IPRange)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infix 4 <<., <<=., >>., >>=.

-- | Operator @<<@: is contained by.
(<<.) ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
a <<. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") << (" <> unsafeGetSqlExpr b <> ")")

-- | Operator @<<=@: is contained by or equals.
(<<=.) ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
a <<=. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") <<= (" <> unsafeGetSqlExpr b <> ")")

-- | Operator @>>@: contains.
(>>.) ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
a >>. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") >> (" <> unsafeGetSqlExpr b <> ")")

-- | Operator @>>=@: contains or equals.
(>>=.) ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
a >>=. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") >>= (" <> unsafeGetSqlExpr b <> ")")

-- | Operator @&&@: contains or is contained by.
inetOverlaps_ ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
inetOverlaps_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") && (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @ABBREV@ function: abbreviated display form as text.
abbrev_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n Text)
abbrev_ a = UnsafeMkSqlExpr ("ABBREV(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @BROADCAST@ function: broadcast address for the network.
broadcast_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n IPRange)
broadcast_ a = UnsafeMkSqlExpr ("BROADCAST(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @FAMILY@ function: address family (4 for IPv4, 6 for IPv6).
family_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n Int32)
family_ a = UnsafeMkSqlExpr ("FAMILY(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @HOST@ function: host address as text.
host_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n Text)
host_ a = UnsafeMkSqlExpr ("HOST(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @HOSTMASK@ function: host mask for the address.
hostmask_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n IPRange)
hostmask_ a = UnsafeMkSqlExpr ("HOSTMASK(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @INET_MERGE@ function: smallest network containing both addresses.
inetMerge_ ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) IPRange)
inetMerge_ a b = UnsafeMkSqlExpr ("INET_MERGE(" <> unsafeGetSqlExpr a <> ", " <> unsafeGetSqlExpr b <> ")")

-- | SQL @INET_SAME_FAMILY@ function: are both addresses of the same family?
inetSameFamily_ ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
inetSameFamily_ a b = UnsafeMkSqlExpr ("INET_SAME_FAMILY(" <> unsafeGetSqlExpr a <> ", " <> unsafeGetSqlExpr b <> ")")

-- | SQL @MASKLEN@ function: subnet mask length.
masklen_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n Int32)
masklen_ a = UnsafeMkSqlExpr ("MASKLEN(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @NETMASK@ function: subnet mask for the network.
netmask_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n IPRange)
netmask_ a = UnsafeMkSqlExpr ("NETMASK(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @NETWORK@ function: network address for the address.
network_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n IPRange)
network_ a = UnsafeMkSqlExpr ("NETWORK(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @SET_MASKLEN@ function: set the subnet mask length.
setMasklen_ ::
  SqlExpr scope (SqlT n IPRange) ->
  SqlExpr scope (SqlT n' Int32) ->
  SqlExpr scope (SqlT (MostNullable n n') IPRange)
setMasklen_ a b = UnsafeMkSqlExpr ("SET_MASKLEN(" <> unsafeGetSqlExpr a <> ", " <> unsafeGetSqlExpr b <> ")")

-- | SQL @TEXT@ function: unabbreviated display form as text.
inetToText_ :: SqlExpr scope (SqlT n IPRange) -> SqlExpr scope (SqlT n Text)
inetToText_ a = UnsafeMkSqlExpr ("TEXT(" <> unsafeGetSqlExpr a <> ")")

-- | SQL @inet + bigint@: add an offset to an address.
inetAddOffset_ ::
  SqlExpr scope (SqlT n IPRange) ->
  SqlExpr scope (SqlT n' Int64) ->
  SqlExpr scope (SqlT (MostNullable n n') IPRange)
inetAddOffset_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") + (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @inet - bigint@: subtract an offset from an address.
inetSubOffset_ ::
  SqlExpr scope (SqlT n IPRange) ->
  SqlExpr scope (SqlT n' Int64) ->
  SqlExpr scope (SqlT (MostNullable n n') IPRange)
inetSubOffset_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")

-- | SQL @inet - inet@: compute the difference of two addresses as a bigint.
inetSubInet_ ::
  SqlExpr scope (SqlT lhsN IPRange) ->
  SqlExpr scope (SqlT rhsN IPRange) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Int64)
inetSubInet_ a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") - (" <> unsafeGetSqlExpr b <> ")")
