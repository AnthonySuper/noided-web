module Noided.Sql.Internal.SqlExpr.Bind where

import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

-- | Bind a param, which can occur at any scope.
bindParam ::
  forall a scope.
  (AsBindParam a) =>
  a ->
  SqlExpr scope (SqlT (BoundNullability a) (BoundType a))
bindParam a = UnsafeMkSqlExpr (Syn $ \_ -> return (BoundParam a))
