{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Action.Search where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Sql
import OptBeer.Form.Type.Pagination (PaginationFormF (..))
import OptBeer.Form.Type.Search (SearchFormF (..))
import Optics.Core (preview)

-- | Apply search and pagination to a query.
useSearch ::
  (result -> SqlExpr NormalQuery (NonNullT PGTSVector)) ->
  SelectM result ->
  SearchFormF FormInput ->
  OrderLimitOffsetLock NormalQuery result
useSearch toVector query form =
  let searchText = fromMaybe "" $ preview fieldInputTyped form.search
      mPage = preview (fieldInputTyped @Int) form.pagination.val.page
      mPerPage = preview (fieldInputTyped @Int) form.pagination.val.perPage

      page = min 1 $ fromMaybe 1 mPage
      perPage = min 1 $ max 100 $ fromMaybe 20 mPerPage
      offset = fromIntegral $ max 0 (page - 1) * perPage

      filteredQuery =
        if T.null (T.strip searchText)
          then query
          else do
            res <- query
            let q = plainToTSQuery_ (bindParam searchText)
            addWhere_ $ toVector res @@. q
            return res
   in if T.null (T.strip searchText)
        then
          offsetLimit_ (Just offset) (FetchFirstClauseOnly (fromIntegral perPage)) filteredQuery
        else
          let q = plainToTSQuery_ (bindParam searchText)
           in offsetOrderLimit_ (Just offset) (\res -> OrderQueryBy (desc_ $ tsRank_ (toVector res) q)) (FetchFirstClauseOnly (fromIntegral perPage)) filteredQuery
