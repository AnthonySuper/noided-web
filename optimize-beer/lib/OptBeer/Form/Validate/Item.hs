{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.Item where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Ids.ItemId
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Table.Item
import OptBeer.Form.Type.Item
import OptBeer.ValidationError.ValueTaken

-- | Validates an item form.
itemValidator :: OrganizationId -> Maybe ItemId -> FormValidator (TransactM e) (SubformField ItemFormF)
itemValidator orgId mItemId = validateSubform $
  ItemForm
    { name = validateItemName orgId mItemId,
      description = validateInput return,
      defaultUnit = validateInput return
    }

validateItemName :: OrganizationId -> Maybe ItemId -> FormValidator (TransactM e) (InputField Text)
validateItemName orgId mItemId = validateInput $ \nameText -> do
  let stripped = T.strip nameText
  when (T.null stripped) $
    failNonfatal Blank

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ itemsTable)
    addWhere_ (row.organizationId ==. bindParam orgId)
    addWhere_ (row.name ==. bindParam stripped)
    case mItemId of
      Just itemId -> addWhere_ (row.id /=. bindParam itemId)
      Nothing -> return ()
    select_ $ Element row.name
  
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  
  return stripped
