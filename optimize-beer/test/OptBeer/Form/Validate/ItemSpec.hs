{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.ItemSpec (spec) where

import Data.Pool (Pool)
import Data.Text (Text)
import Hasql.Connection (Connection)
import Noided.Form.HKD
import Noided.Row
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Table.Item
import OptBeer.DB.Table.Organization
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Type.Unit
import OptBeer.Form.Type.Item
import OptBeer.Form.Validate.Item
import OptBeer.ValidationError.ValueTaken
import Test.Hspec

spec :: SpecWith (Pool Connection)
spec = describe "itemValidator" $ do
  it "validates a correct form" $ \pool -> do
    res <- runDB @String (do
      orgId <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 1") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      let input =
            ItemForm
              { name = InputInput $ FromTyped "Item 1",
                description = InputInput $ FromTyped "Description 1",
                defaultUnit = InputInput $ FromTyped Gram
              }
      validateFormDB (itemValidator orgId Nothing) input
      ) pool
    case res of
      Right _ -> return ()
      Left err -> expectationFailure $ "Expected success, got errors: " <> show err

  it "fails when name is blank" $ \pool -> do
    res <- runDB @String (do
      orgId <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 1") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      let input =
            ItemForm
              { name = InputInput $ FromTyped "  ",
                description = InputInput $ FromTyped "Description 1",
                defaultUnit = InputInput $ FromTyped Gram
              }
      validateFormDB (itemValidator orgId Nothing) input
      ) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.name.innerErrors `shouldSatisfy` (`hasError` Blank)

  it "fails when name is taken in the same organization" $ \pool -> do
    res <- runDB @String (do
      orgId <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 1") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      
      _ <- querySingleRow $ insertReturning itemsTable 
        (values_ 
          [ #organizationId :==> mutateVal_ (bindParam orgId) 
          :::%? #name :==> mutateVal_ (bindParam @Text "Item 1") 
          :::%? #defaultUnit :==> mutateVal_ (bindParam Gram)
          :::%? EmptyWrappedRow
          ]
        ) (\row -> Element $ row.id)

      let input =
            ItemForm
              { name = InputInput $ FromTyped "Item 1",
                description = InputInput $ FromTyped "Description 1",
                defaultUnit = InputInput $ FromTyped Gram
              }
      validateFormDB (itemValidator orgId Nothing) input
      ) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.name.innerErrors `shouldSatisfy` (`hasError` ValueTaken)

  it "succeeds when name is taken by the same item (update)" $ \pool -> do
    res <- runDB @String (do
      orgId <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 1") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      
      itemId <- querySingleRow $ insertReturning itemsTable 
        (values_ 
          [ #organizationId :==> mutateVal_ (bindParam orgId) 
          :::%? #name :==> mutateVal_ (bindParam @Text "Item 1") 
          :::%? #defaultUnit :==> mutateVal_ (bindParam Gram)
          :::%? EmptyWrappedRow
          ]
        ) (\row -> Element $ row.id)

      let input =
            ItemForm
              { name = InputInput $ FromTyped "Item 1",
                description = InputInput $ FromTyped "New Description",
                defaultUnit = InputInput $ FromTyped Gram
              }
      validateFormDB (itemValidator orgId (Just itemId)) input
      ) pool
    case res of
      Right _ -> return ()
      Left err -> expectationFailure $ "Expected success, got errors: " <> show err

  it "succeeds when name is taken in a different organization" $ \pool -> do
    res <- runDB @String (do
      org1Id <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 1") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      org2Id <- querySingleRow $ insertReturning organizationsTable (values_ [#name :==> mutateVal_ (bindParam @Text "Org 2") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      
      _ <- querySingleRow $ insertReturning itemsTable 
        (values_ 
          [ #organizationId :==> mutateVal_ (bindParam org1Id) 
          :::%? #name :==> mutateVal_ (bindParam @Text "Item 1") 
          :::%? #defaultUnit :==> mutateVal_ (bindParam Gram)
          :::%? EmptyWrappedRow
          ]
        ) (\row -> Element $ row.id)

      let input =
            ItemForm
              { name = InputInput $ FromTyped "Item 1",
                description = InputInput $ FromTyped "Description 1",
                defaultUnit = InputInput $ FromTyped Gram
              }
      validateFormDB (itemValidator org2Id Nothing) input
      ) pool
    case res of
      Right _ -> return ()
      Left err -> expectationFailure $ "Expected success, got errors: " <> show err
