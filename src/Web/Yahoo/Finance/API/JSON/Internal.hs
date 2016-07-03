{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-}

module Web.Yahoo.Finance.API.JSON.Internal where

import Control.Monad.Except (ExceptT(..))
import Control.Lens (Traversal', (^..))
import Data.Aeson (FromJSON(..), Value, (.:), withObject)
import Data.Aeson.Lens (key, values)
import Data.Aeson.Types (Parser)
import Data.Data (Data)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant.API (Capture, Get, JSON, QueryParam, (:>))
import Servant.Client (BaseUrl(..), ServantError, Scheme(..), client)
import Web.HttpApiData (ToHttpApiData(..))

import Web.Yahoo.Finance.Types (StockSymbol)

-- | Query format query param for the Yahoo finance webservice APIs.
-- Normally should be the string @json@.
newtype QueryFormat = QueryFormat { unQueryFormat :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData QueryFormat where
    toQueryParam :: QueryFormat -> Text
    toQueryParam = unQueryFormat

-- | View type query param for the Yahoo finance webservice APIs.
-- Normally should be the string @detail@.
newtype ViewType = ViewType { unViewType :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData ViewType where
    toQueryParam :: ViewType -> Text
    toQueryParam = unViewType

-- | Real-time stock quote.
data Quote =
    Quote
        { quoteChange :: Text
        , quoteChangePercent :: Text
        , quoteDayHigh :: Text
        , quoteDayLow :: Text
        , quoteIssuerName :: Text
        , quoteIssuerNameLang :: Text
        , quoteName :: Text
        , quotePrice :: Text
        , quoteSymbol :: Text
        , quoteTS :: Text
        , quoteType :: Text
        , quoteUTCTime :: UTCTime
        , quoteVolume :: Text
        , quoteYearHigh :: Text
        , quoteYearLow :: Text
        }
    deriving (Eq, Show)

instance FromJSON Quote where
    parseJSON :: Value -> Parser Quote
    parseJSON = withObject "Quote" $ \obj ->
        Quote
            <$> obj .: "change"
            <*> obj .: "chg_percent"
            <*> obj .: "day_high"
            <*> obj .: "day_low"
            <*> obj .: "issuer_name"
            <*> obj .: "issuer_name_lang"
            <*> obj .: "name"
            <*> obj .: "price"
            <*> obj .: "symbol"
            <*> obj .: "ts"
            <*> obj .: "type"
            <*> obj .: "utctime"
            <*> obj .: "volume"
            <*> obj .: "year_high"
            <*> obj .: "year_low"

newtype QuoteList = QuoteList { unQuoteList :: [Quote] }
    deriving (Eq, Show)

instance FromJSON QuoteList where
    parseJSON :: Value -> Parser QuoteList
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

type YahooFinanceJsonApi
    = "webservice"
    :> "v1"
    :> "symbols"
    :> Capture "symbol_list" [StockSymbol]
    :> "quote"
    :> QueryParam "format" QueryFormat
    :> QueryParam "view" ViewType
    :> Get '[JSON] QuoteList

yahooFinanceJsonBaseUrl :: BaseUrl
yahooFinanceJsonBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "finance.yahoo.com"
    , baseUrlPort = 443
    , baseUrlPath = "/"
    }

getQuoteLowLevel
    :: [StockSymbol]
    -> Maybe QueryFormat
    -> Maybe ViewType
    -> Manager
    -> BaseUrl
    -> ExceptT ServantError IO QuoteList
getQuoteLowLevel = client (Proxy :: Proxy YahooFinanceJsonApi)
