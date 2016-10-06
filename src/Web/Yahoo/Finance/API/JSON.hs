{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module      : Web.Yahoo.Finance.API.JSON
Description : Access methods for the Yahoo Finance JSON APIs.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module contians methods for accessing the Yahoo Finance webservice APIs.

The 'getQuote' method is mainly used to obtain stock quotes for specific stocks.
-}

module Web.Yahoo.Finance.API.JSON
    ( getQuote
    , getQuoteTrans
    -- * Types
    , Quote(..)
    , QuoteList(..)
    ) where

import Control.Monad.Except
-- import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Network.HTTP.Client (HasHttpManager(..), Manager)
import Servant.Client

import Web.Yahoo.Finance.API.JSON.Internal
    ( Quote(..), QuoteList(..), getQuoteLowLevel, yahooFinanceJsonBaseUrl )
import Web.Yahoo.Finance.Types (StockSymbol)

#if MIN_VERSION_servant(0,5,0)
  
#else
import Control.Monad.Trans.Either
#endif

-- | Get stock quotes from Yahoo Finance webservice APIs.
--
-- Here is a short example of the usage:
--
-- @
--   (manager :: 'Manager') <- 'getGlobalManager'
--   (eitherRes :: 'Either' 'ServantError' 'QuoteList') <-
--       'runExceptT' $ 'runReaderT' ('getQuote' [\"VGTSX\", \"GOOG\"]) manager
--   let (res :: 'QuoteList') =
--           'either' 'undefined' 'id' eitherRes -- Warning: this is unsafe...
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
-- >     { quoteChange = "7.110046"
-- >     , quoteChangePercent = "1.027315"
-- >     , quoteDayHigh = "700.650024"
-- >     , quoteDayLow = "692.130127"
-- >     , quoteIssuerName = "Alphabet Inc."
-- >     , quoteIssuerNameLang = "Alphabet Inc."
-- >     , quoteName = "Alphabet Inc."
-- >     , quotePrice = "699.210022"
-- >     , quoteSymbol = "GOOG"
-- >     , quoteTS = "1467403200"
-- >     , quoteType = "equity"
-- >     , quoteUTCTime = 2016-07-01 20:00:00 UTC
-- >     , quoteVolume = "1344710"
-- >     , quoteYearHigh = "789.870000"
-- >     , quoteYearLow = "515.180000"
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
#if MIN_VERSION_servant(0, 9, 0)
    manager <- reader getHttpManager  
    eitherRes <- liftIO $ runClientM (getQuoteLowLevel stockSymbols (Just "json") (Just "detail")) (ClientEnv manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0, 6, 0)
    manager <- reader getHttpManager
    eitherRes <- liftIO . runExceptT $
        getQuoteLowLevel
            stockSymbols
            (Just "json")
            (Just "detail")
            manager
            yahooFinanceJsonBaseUrl
#elif MIN_VERSION_servant(0, 5, 0)
    manager <- reader getHttpManager
    eitherRes <- liftIO . runExceptT $
        getQuoteLowLevel
            yahooFinanceJsonBaseUrl
            manager
            stockSymbols
            (Just "json")
            (Just "detail")
#else
    eitherRes <- liftIO . runEitherT $
        getQuoteLowLevel
            yahooFinanceJsonBaseUrl
            stockSymbols
            (Just "json")
            (Just "detail")
#endif
    either throwError pure eitherRes

-- | Similar to 'getQuotes' but using transformers intead of mtl.
getQuoteTrans :: [StockSymbol] -> ReaderT Manager (ExceptT ServantError IO) QuoteList
getQuoteTrans = getQuote
