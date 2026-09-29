module Noided.Sql.Internal.SqlExpr.Bind where

import Noided.Sql.Internal.Class.AsBindParam
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

-- | Bind a param, which can occur at any scope.
bindParam_ ::
  forall a scope.
  (AsBindParam a) =>
  a ->
  SqlExpr scope (SqlT (BoundNullability a) (BoundType a))
bindParam_ a = unsafeMkAtom (Syn $ \_ -> return (BoundParam a))
