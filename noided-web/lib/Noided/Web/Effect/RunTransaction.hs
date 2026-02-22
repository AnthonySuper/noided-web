{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.RunTransaction
  ( RunTransaction,
    runTransaction,
    runTransactionEither,
    runTransactionToResult,
    runInfallibleTransaction,
    runTransaction',
    runTransactionEither',
    runTransactionToResult',
    runInfallibleTransaction',
    timeTransactions,
    runRunTransactionFromConnection,
  )
where

import Data.Aeson
import Data.Time.Clock (diffUTCTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Noided.Sql.Internal.Type.SqlQuery
import Noided.Sql.Internal.Type.TransactM
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.Log
import Noided.Web.Internal.Effect.RunTransaction
import Noided.Web.Internal.Effect.TimeEvent
import Noided.Web.Internal.Effect.UseConnection

timeTransactions ::
  ( TimeEvent :> es,
    RunTransaction :> es
  ) =>
  Eff es a ->
  Eff es a
timeTransactions = interpose $ \env r@(RunTransaction {}) ->
  recordEventTime "sql.transaction" (passthrough env r)

runRunTransactionFromConnection ::
  ( UseConnection :> es,
    IOE :> es,
    CurrentTime :> es,
    TimeEvent :> es,
    Log :> es
  ) =>
  Eff (RunTransaction : es) a ->
  Eff es a
runRunTransactionFromConnection act =
  recordEventTime "sql.checkout" $
    useConnection $ \connection ->
      withSeqEffToIO $ \liftE -> do
        let sc = statementCallback $ \query useQuery -> do
              beforeT <- liftIO $ liftE getCurrentTime
              res <- useQuery query
              endT <- liftIO $ liftE getCurrentTime
              let queryTime = endT `diffUTCTime` beforeT
              liftIO $ liftE $ recordStaticTime "sql.query" queryTime
              liftIO $
                liftE $
                  logMessage $
                    LoggedMessage
                      { level = Info,
                        msg = "SQL Query",
                        ctx =
                          [ ("queryText", toJSON (sqlQuerySyntax query)),
                            ("queryBinds", toJSON (sqlQueryInspectedBinds query)),
                            ("queryTime", toJSON queryTime)
                          ]
                      }
              return res
        liftE $ runRunTransaction sc connection act
