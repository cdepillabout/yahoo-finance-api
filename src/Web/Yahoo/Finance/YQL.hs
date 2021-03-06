{-|
Module      : Web.Yahoo.Finance.YQL
Description : Expose certain types and functions.
Copyright   : (c) James M.C. Haver II, 2016
License     : BSD3

This module contains methods for accessing the Yahoo Finance YQL APIs.
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

module Web.Yahoo.Finance.YQL (
  -- * Fetch Stock Quotes
    getQuotes
  , yahooFinanceJsonBaseUrl
  -- * Types
  , YQLQuery(..)
  , StockSymbol(..)
  , YQLResponse(..)
  , Quote(..)
  ) where

import Servant.Client
import Web.Yahoo.Finance.YQL.Internal.API
import Web.Yahoo.Finance.YQL.Internal.Types

#if MIN_VERSION_servant(0, 9, 0)
#elif MIN_VERSION_servant(0, 5, 0)
import Control.Monad.Except
import Network.HTTP.Client (Manager)
#else
import Control.Monad.Trans.Either
#endif


-- | General client API to access Yahoo financial data. Quotes are returned
-- in the order they are queried.
#if MIN_VERSION_servant(0, 9, 0)
getQuotes :: YQLQuery -> ClientM YQLResponse
getQuotes qs = getQuotesInternal (Just qs) (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "")
#elif MIN_VERSION_servant(0, 6, 0)
getQuotes :: YQLQuery -> Manager -> BaseUrl -> ExceptT ServantError IO YQLResponse
getQuotes qs bs m  = getQuotesInternal (Just qs) (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "") bs m
#elif MIN_VERSION_servant(0, 5, 0)
getQuotes :: BaseUrl -> Manager -> YQLQuery -> ExceptT ServantError IO YQLResponse
getQuotes bs m qs = getQuotesInternal bs m (Just qs) (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "")
#else
getQuotes :: BaseUrl -> YQLQuery -> EitherT ServantError IO YQLResponse
getQuotes bs qs = getQuotesInternal bs (Just qs) (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "")
#endif


-- | 'BaseUrl' for the Yahoo Finance YQL API.  This represents
-- @https://finance.yahoo.com@.
yahooFinanceJsonBaseUrl :: BaseUrl
yahooFinanceJsonBaseUrl = BaseUrl {
  baseUrlScheme = Https
, baseUrlHost = "query.yahooapis.com"
, baseUrlPort = 443
#if MIN_VERSION_servant(0, 5, 0)
, baseUrlPath = "/"
#endif
}

