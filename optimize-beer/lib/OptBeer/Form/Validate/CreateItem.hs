{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.CreateItem where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Table.Item
import OptBeer.Form.Type.CreateItem
import OptBeer.ValidationError.ValueTaken

-- | Validates that an item can be created.
createItemValidator :: OrganizationId -> FormValidator (TransactM e) (SubformField CreateItemF)
createItemValidator orgId = validateSubform $
  CreateItem
    { name = validateItemName orgId,
      description = validateInput return,
      defaultUnit = validateInput return
    }

validateItemName :: OrganizationId -> FormValidator (TransactM e) (InputField Text)
validateItemName orgId = validateInput $ \nameText -> do
  when (T.null (T.strip nameText)) $
    failNonfatal Blank

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ itemsTable)
    addWhere_ (row.organizationId ==. bindParam orgId)
    addWhere_ (row.name ==. bindParam nameText)
    select_ $ Element row.name
  
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  
  return nameText
