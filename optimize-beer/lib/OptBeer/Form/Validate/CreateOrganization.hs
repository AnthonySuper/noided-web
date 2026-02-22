{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.CreateOrganization where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Table.Organization
import OptBeer.Form.Type.CreateOrganization
import OptBeer.ValidationError.ValueTaken

-- | Validates that an organization can be created.
createOrganizationValidator :: FormValidator (TransactM e) (SubformField CreateOrganizationF)
createOrganizationValidator = validateSubform $
  CreateOrganization
    { name = validateOrganizationName
    }

validateOrganizationName :: FormValidator (TransactM e) (InputField Text)
validateOrganizationName = validateInput $ \nameText -> do
  when (T.null nameText) $
    failNonfatal Blank

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ organizationsTable)
    addWhere_ (row.name ==. bindParam nameText)
    select_ $ Element row.name
  
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  
  return nameText
