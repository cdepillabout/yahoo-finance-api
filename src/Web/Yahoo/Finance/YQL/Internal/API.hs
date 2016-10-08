{-|
Module      : Web.Yahoo.Finance.YQL.Internal.API
Description : Access methods for the Yahoo Finance YQL APIs.
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


-- | Low-level Servant definition of the Yahoo Finance webservice API.
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
-- be filled except for "callback".
--
-- format default is "json"
-- env default is "store://datatables.org/alltableswithkeys"
getQuotesInternal :: Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM YQLResponse
getQuotesInternal = client (Proxy :: Proxy YahooFinanceYQLApi)

-- | The order of arguments slightly changes for each version of servant. 
{-
#if MIN_VERSION_servant(0, 9, 0)
getQuoteLowLevel :: [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> ClientM QuoteList
#elif MIN_VERSION_servant(0, 6, 0)
getQuoteLowLevel :: [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> Manager -> BaseUrl -> ExceptT ServantError IO QuoteList
#elif MIN_VERSION_servant(0, 5, 0) 
getQuoteLowLevel :: BaseUrl -> Manager -> [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> ExceptT ServantError IO QuoteList
#else
getQuoteLowLevel :: BaseUrl -> [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> EitherT ServantError IO QuoteList
#endif
-}
