{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Effect.CurrentActor where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Data.Time (UTCTime)
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.Session
import OptBeer.DB.Ids.SessionId
import Noided.Sql
import Noided.Web

-- | Effect for accessing the currently authenticated actor.
data CurrentActor :: Effect

type instance DispatchOf CurrentActor = Static NoSideEffects

newtype instance StaticRep CurrentActor = CurrentActor (Maybe Actor)

-- | Get the current actor, if authenticated.
getCurrentActor :: (CurrentActor :> es) => Eff es (Maybe Actor)
getCurrentActor = do
  CurrentActor ma <- getStaticRep
  return ma

-- | Run the 'CurrentActor' effect with a provided actor.
runWithCurrentActor :: Maybe Actor -> Eff (CurrentActor : es) a -> Eff es a
runWithCurrentActor ma = evalStaticRep (CurrentActor ma)

-- | Run the 'CurrentActor' effect by looking up the session in the database.
runWithCurrentActorFromSession ::
  ( GetCookies :> es,
    RunTransaction :> es,
    CurrentTime :> es,
    Error SessionError :> es,
    IOE :> es
  ) =>
  Eff (CurrentActor : es) a ->
  Eff es a
runWithCurrentActorFromSession act = do
  mSid <- getSecureCookie "sessionId"
  mActor <- case mSid of
    Nothing -> return Nothing
    Just (sid :: SessionId) -> do
      now <- getCurrentTime
      runTransactionEither (fetchActorFromSession now sid) >>= \case
        Left _ -> return Nothing
        Right ma -> return ma

  runWithCurrentActor mActor act

fetchActorFromSession :: UTCTime -> SessionId -> TransactM e (Maybe Actor)
fetchActorFromSession now sid = queryMaybe $ do
  session <- addFrom_ (fromBase_ sessionsTable)
  actor <- addFrom_ (fromBase_ actorsTable)
  addWhere_ (session.id ==. bindParam sid)
  addWhere_ (session.userId ==. actor.id)
  -- Ensure session is still valid
  addWhere_ (bindParam now `isContainedBy_` session.validDuring)
  select_ actor
