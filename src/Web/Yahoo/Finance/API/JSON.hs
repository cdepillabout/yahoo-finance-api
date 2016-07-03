{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Web.Yahoo.Finance.API.JSON
Description : Access methods for the Yahoo Finance JSON APIs.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module contians methods for accessing the Yahoo Finance webservice APIs.

The 'getQuote' method is mainly used to obtain stock quotes for specific stocks.

Here is an example
-}

module Web.Yahoo.Finance.API.JSON
    ( Quote(..)
    , QuoteList(..)
    , getQuote
    , getQuoteTrans
    ) where

import Control.Monad.Except (ExceptT(..), MonadError(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Network.HTTP.Client (HasHttpManager(..), Manager)
import Servant.Client (ServantError)

import Web.Yahoo.Finance.API.JSON.Internal
    ( Quote(..), QuoteList(..), getQuoteLowLevel, yahooFinanceJsonBaseUrl )
import Web.Yahoo.Finance.Types (StockSymbol)

-- | Get stock quotes from Yahoo Finance webservice APIs.
--
-- Here is a short example of the usage:
--
-- @
--   (manager :: 'Manager') <- 'getGlobalManager'
--   (eitherRes :: 'Either' 'ServantError' 'QuoteList') <-
--       'runExceptT' $ 'runReaderT' ('getQuote' ["VGTSX", "TRRNX"]) manager
--   let (res :: 'QuoteList') = 'either' 'undefined' 'id' eitherRes -- Warning: this is unsafe...
--   'show' $ 'unQuoteList' res
-- @
--
-- > [ Quote
-- >     { quoteChange = "0.050000"
-- >     , quoteChangePercent = "0.350141"
-- >     , quoteDayHigh = "0.000000"
-- >     , quoteDayLow = "0.000000"
-- >     , quoteIssuerName = "Vanguard Total Intl Stock Index Inv"
-- >     , quoteIssuerNameLang = "Vanguard Total Intl Stock Index Inv"
-- >     , quoteName = "Vanguard Total International St"
-- >     , quotePrice = "14.330000"
-- >     , quoteSymbol = "VGTSX"
-- >     , quoteTS = "1467413100"
-- >     , quoteType = "mutualfund"
-- >     , quoteUTCTime = 2016-07-01 22:45:00 UTC
-- >     , quoteVolume = "0"
-- >     , quoteYearHigh = "16.330000"
-- >     , quoteYearLow = "12.760000"
-- >     }
-- > , Quote
-- >     { quoteChange = "0.140000"
-- >     , quoteChangePercent = "1.098904"
-- >     , quoteDayHigh = "0.000000"
-- >     , quoteDayLow = "0.000000"
-- >     , quoteIssuerName = "T. Rowe Price Retirement 2055"
-- >     , quoteIssuerNameLang = "T. Rowe Price Retirement 2055"
-- >     , quoteName = "T. Rowe Price 2055 Retirement F"
-- >     , quotePrice = "12.880000"
-- >     , quoteSymbol = "TRRNX"
-- >     , quoteTS = "1467326700"
-- >     , quoteType = "mutualfund"
-- >     , quoteUTCTime = 2016-06-30 22:45:00 UTC
-- >     , quoteVolume = "0"
-- >     , quoteYearHigh = "14.110000"
-- >     , quoteYearLow = "11.270000"
-- >     }
-- > ]
getQuote
    :: ( HasHttpManager r
       , MonadError ServantError m
       , MonadIO m
       , MonadReader r m
       )
    => [StockSymbol] -> m QuoteList
getQuote stockSymbols = do
    manager <- reader getHttpManager
    eitherRes <- liftIO . runExceptT $
        getQuoteLowLevel
            stockSymbols
            (Just "json")
            (Just "detail")
            manager
            yahooFinanceJsonBaseUrl
    either throwError pure eitherRes

-- | Similar to 'getQuotes' but using transformers intead of mtl.
getQuoteTrans
    :: [StockSymbol] -> ReaderT Manager (ExceptT ServantError IO) QuoteList
getQuoteTrans = getQuote
