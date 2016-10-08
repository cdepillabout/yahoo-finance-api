{-|
Module      : Web.Yahoo.Finance.YQL.Types
Description : Access methods for the Yahoo Finance YQL APIs.
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

module Web.Yahoo.Finance.YQL.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Servant.API
import Servant.Client

-- import Network.HTTP.Client (HasHttpManager(..), Manager)

data YQLQuery = YQLQuery {
  yqlQuery :: [StockSymbol]
} deriving (Eq, Show, Generic)

instance ToHttpApiData YQLQuery where
  toUrlPiece :: YQLQuery -> Text
  toUrlPiece (YQLQuery {..}) = "select * from yahoo.finance.quotes where symbol in (" <> toUrlPiece yqlQuery <> ")"

-- https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22YHOO%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=
newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
#if MIN_VERSION_servant(0, 5, 0)
    deriving (Eq, Generic, Ord, Show) --, Typeable)
#else
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable, ToText)
#endif 

instance ToHttpApiData StockSymbol where
  toUrlPiece :: StockSymbol -> Text
  toUrlPiece (StockSymbol {..}) = "\"" <> unStockSymbol <> "\""

-- | Connect separate 'StockSymbol's with a comma.
--
-- >>> toUrlPiece (["GOOG", "YHOO", "^GSPC"] :: [StockSymbol])
-- "GOOG,YHOO,^GSPC"
instance ToHttpApiData [StockSymbol] where
    toUrlPiece :: [StockSymbol] -> Text
    toUrlPiece = fold . intersperse "," . fmap toUrlPiece

{-
instance ToText [StockSymbol] where
  toText :: [StockSymbol] -> Text  
  toText = fold . intersperse "," . fmap toText
-}


data YQLResponse = YQLResponse {
  responseCount   :: Int
, responseCreated :: UTCTime
, responseLang    :: Text
, responseQuotes  :: [Quote]
} deriving (Eq, Read, Show, Generic)

instance FromJSON YQLResponse where
  parseJSON = withObject "YQLResponse" $ \o -> do
    innerO <- o .: "query"
    results <- innerO .: "results"
    quotes <- ((results .: "quote" :: (Parser Quote)) >>= \q -> return [q]) <|> (results .: "quote" :: Parser [Quote])
    
    YQLResponse <$> innerO .: "count"
                <*> innerO .: "created"
                <*> innerO .: "lang"
                <*> pure quotes

data Quote = Quote {
  quoteAverageDailyVolume   :: Text
, quoteChange               :: Text
, quoteDaysLow              :: Text
, quoteDaysHigh             :: Text
, quoteYearLow             :: Text
, quoteYearHigh            :: Text
, quoteMarketCapitalization :: Text
, quoteLastTradePriceOnly   :: Text
, quoteDaysRange            :: Text
, quoteName                 :: Text
, quoteSymbol               :: Text
, quoteVolume               :: Text
, quoteStockExchange        :: Text
} deriving (Eq, Read, Show, Generic)

instance FromJSON Quote where
  parseJSON = withObject "Quote" $ \o ->
    Quote <$> o .: "AverageDailyVolume"
          <*> o .: "Change"
          <*> o .: "DaysLow"
          <*> o .: "DaysHigh"
          <*> o .: "YearLow"
          <*> o .: "YearHigh"
          <*> o .: "MarketCapitalization"
          <*> o .: "LastTradePriceOnly"
          <*> o .: "DaysRange"
          <*> o .: "Name"
          <*> o .: "Symbol"
          <*> o .: "Volume"
          <*> o .: "StockExchange"

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
-- https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22YHOO%22,%22GOOG%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=

-- &format=json&env=store://datatables.org/alltableswithkeys&callback=

-- | 'BaseUrl' for the Yahoo Finance webservice API.  This represents
-- @https://finance.yahoo.com@.
yahooFinanceJsonBaseUrl :: BaseUrl
yahooFinanceJsonBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "query.yahooapis.com"
    , baseUrlPort = 443
#if MIN_VERSION_servant(0, 5, 0)    
    , baseUrlPath = "/"
#endif    
    }


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
getQuotesInternal :: Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM YQLResponse
getQuotesInternal = client (Proxy :: Proxy YahooFinanceYQLApi)

getQuotes :: YQLQuery -> ClientM YQLResponse
getQuotes qs = getQuotesInternal (Just qs) (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "")
