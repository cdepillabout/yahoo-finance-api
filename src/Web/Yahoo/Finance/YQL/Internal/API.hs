{-|
Module      : Web.Yahoo.Finance.YQL.Internal.API
Description : Internal API types and functions.
Copyright   : (c) James M.C. Haver II, 2016
License     : BSD3

This is an internal module. Use at your own risk.
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Yahoo.Finance.YQL.Internal.API where

import Web.Yahoo.Finance.YQL.Internal.Types

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client

#if MIN_VERSION_servant(0, 9, 0)
#elif MIN_VERSION_servant(0, 5, 0)
import Control.Monad.Except
import Network.HTTP.Client (Manager)
#else
import Control.Monad.Trans.Either
#endif

-- | Low-level Servant definition of the Yahoo Finance YQL client API.
type YahooFinanceYQLApi
  =  "v1"
  :> "public"
  :> "yql"
  :> QueryParam "q" YQLQuery
  :> QueryParam "format" Text
  :> QueryParam "env" Text
  :> QueryParam "callback" Text
  :> Get '[JSON] YQLResponse

-- | 'QueryParam' are treated as maybes but Yahoo requires that all the fields
-- be filled except for "callback". Quotes are returned in the order they are
-- queried.
--
-- format default is "json"
-- env default is "store://datatables.org/alltableswithkeys"
-- The order of arguments varies slightly with each version of Servant.
#if MIN_VERSION_servant(0, 9, 0)
getQuotesInternal :: Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM YQLResponse
#elif MIN_VERSION_servant(0, 6, 0)
getQuotesInternal :: Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> Manager -> BaseUrl -> ExceptT ServantError IO YQLResponse
#elif MIN_VERSION_servant(0, 5, 0)
getQuotesInternal :: BaseUrl -> Manager -> Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> ExceptT ServantError IO YQLResponse
#else
getQuotesInternal :: BaseUrl -> Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> EitherT ServantError IO YQLResponse
#endif
getQuotesInternal = client (Proxy :: Proxy YahooFinanceYQLApi)
