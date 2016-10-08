{-|
Module      : Web.Yahoo.Finance.API.YQL
Description : Access methods for the Yahoo Finance YQL APIs.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module contians methods for accessing the Yahoo Finance YQL APIs.

Currently not implemented.
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

module Web.Yahoo.Finance.API.YQL where

import Data.Aeson
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Time
import GHC.Generics


import Control.Lens (Traversal', (^..))
import Data.Aeson.Lens (key, values)
import Data.Proxy
import Data.Typeable
import Servant.API
import Network.HTTP.Client (HasHttpManager(..), Manager)
import Servant.Client
import Data.Foldable (fold)
import Web.HttpApiData (ToHttpApiData(..))  


import Web.Yahoo.Finance.Types


import Data.Data (Data)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Data.Aeson.Types (Parser)

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
    mSingleQuote <- results .:? "quote" :: Parser (Maybe Quote)
    quotes <- case mSingleQuote of
      Just singleQuote -> return [singleQuote]
      Nothing -> innerO .: "results" :: Parser [Quote]
    YQLResponse <$> innerO .: "count"
                <*> innerO .: "created"
                <*> innerO .: "lang"
                <*> pure quotes

data Quote = Quote {
  quoteSymbol :: Text
} deriving (Eq, Read, Show, Generic)

{-
instance ToJSON Quote where
  toJSON (Quote {..}) = object [
      "Symbol" .= quoteSymbol
    ]
-}  
instance FromJSON Quote where
  parseJSON = withObject "Quote" $ \o -> do
    -- innerO <- o .: "quote"
    --Quote <$> innerO .: "Symbol"
    Quote <$> o .: "Symbol"

{-
newtype QuoteList = QuoteList { unQuoteList :: [Quote] }
    deriving (Eq, Show)

instance FromJSON QuoteList where
    -- parseJSON :: Value -> Parser QuoteList
    parseJSON val = do
        let rawQuotes = val ^.. fields
        QuoteList <$> traverse parseJSON rawQuotes
      where
        fields :: Traversal' Value Value
        fields = key "list" .
            key "resources" .
            values .
            key "resource" .
            key "fields"
-}

{-
{
	"query": {
		"count": 2,
		"created": "2016-10-07T16:03:37Z",
		"lang": "en-US",
		"results": {
			"quote": [{
-}
--data YQLInLineQuery = YQLInLineQuery {
--  stockSymbols :: [StockSymbol]
--}
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

{-
type YahooFinanceJsonApi
    = "webservice"
    :> "v1"
    :> "symbols"
    :> Capture "symbol_list" [StockSymbol]
    :> "quote"
    :> QueryParam "format" QueryFormat
    :> QueryParam "view" ViewType
    :> Get '[JSON] QuoteList
-}

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
getQuoteInternal :: Maybe YQLQuery -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM YQLResponse
getQuoteInternal = client (Proxy :: Proxy YahooFinanceYQLApi)

getQuote mq = getQuoteInternal mq (Just "json") (Just "store://datatables.org/alltableswithkeys") (Just "")

data YQLQuery = YQLQuery {
  yqlQuery :: [StockSymbol]
} deriving (Eq, Show, Generic)

instance ToHttpApiData YQLQuery where
  toUrlPiece :: YQLQuery -> Text
  -- toUrlPiece (YQLQuery {..}) = "select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(" <> toUrlPiece yqlQuery <> ")&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="
  toUrlPiece (YQLQuery {..}) = "select * from yahoo.finance.quotes where symbol in (" <> toUrlPiece yqlQuery <> ")"

-- &format=json&env=store://datatables.org/alltableswithkeys&callback=
-- select * from yahoo.finance.quotes where symbol in ("YHOO")&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=
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
