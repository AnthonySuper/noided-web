{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Effect.CurrentActor where

import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Effectful
import Effectful.Dispatch.Static
import Noided.Sql
import Noided.Web
import OptBeer.DB.Ids.SessionId
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.Session

-- | Effect for accessing the currently authenticated actor.
data CurrentActor :: Effect

type instance DispatchOf CurrentActor = Static NoSideEffects

newtype instance StaticRep CurrentActor = CurrentActor (Maybe Actor)

-- | Effect for accessing the current session ID.
data CurrentSession :: Effect

type instance DispatchOf CurrentSession = Static NoSideEffects

newtype instance StaticRep CurrentSession = CurrentSession (Maybe SessionId)

-- | Get the current actor, if authenticated.
getCurrentActor :: (CurrentActor :> es) => Eff es (Maybe Actor)
getCurrentActor = do
  CurrentActor ma <- getStaticRep
  return ma

-- | Get the current session ID, if authenticated.
getCurrentSessionId :: (CurrentSession :> es) => Eff es (Maybe SessionId)
getCurrentSessionId = do
  CurrentSession msid <- getStaticRep
  return msid

-- | Run the 'CurrentActor' effect with a provided actor.
runWithCurrentActor :: Maybe Actor -> Eff (CurrentActor : es) a -> Eff es a
runWithCurrentActor ma = evalStaticRep (CurrentActor ma)

-- | Run the 'CurrentSession' effect with a provided session ID.
runWithCurrentSessionId :: Maybe SessionId -> Eff (CurrentSession : es) a -> Eff es a
runWithCurrentSessionId msid = evalStaticRep (CurrentSession msid)

-- | Run both 'CurrentActor' and 'CurrentSession' effects by looking up the session in the database.
runWithCurrentActorFromSession ::
  ( GetCookies :> es,
    RunTransaction :> es,
    CurrentTime :> es,
    IOE :> es
  ) =>
  Eff (CurrentActor : CurrentSession : es) a ->
  Eff es a
runWithCurrentActorFromSession act = do
  textSid <- getSecureCookie "sessionId"
  let mSid = textSid >>= decode . fromStrict . encodeUtf8
  mActor <- case mSid of
    Nothing -> return Nothing
    Just (sid :: SessionId) -> do
      now <- getCurrentTime
      runTransactionToResult (fetchActorFromSession now sid) >>= \case
        SessionErr _ -> return Nothing
        TransactErr _ -> return Nothing
        TransactOK ma -> return ma

  runWithCurrentSessionId mSid . runWithCurrentActor mActor $ act

fetchActorFromSession :: UTCTime -> SessionId -> TransactM e (Maybe Actor)
fetchActorFromSession now sid = queryMaybe $ do
  session <- addFrom_ (fromBase_ sessionsTable)
  actor <- addFrom_ (fromBase_ actorsTable)
  addWhere_ (session.id ==. bindParam sid)
  addWhere_ (session.userId ==. actor.id)
  -- Ensure session is still valid
  addWhere_ (bindParam now `isContainedBy_` session.validDuring)
  select_ actor
