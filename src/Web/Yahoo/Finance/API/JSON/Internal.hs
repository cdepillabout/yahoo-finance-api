{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Module      : Web.Yahoo.Finance.API.JSON.Internal
Description : Internal types and modules for the Yahoo Finance JSON APIs.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module contains internal types and methods for accessing the Yahoo Finance
webservice APIs.
-}

module Web.Yahoo.Finance.API.JSON.Internal where

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
import Servant.API
import Servant.Client

import Web.Yahoo.Finance.Types (StockSymbol)

#if MIN_VERSION_servant(0, 9, 0)
#elif MIN_VERSION_servant(0, 5, 0)
import Control.Monad.Except
import Network.HTTP.Client (Manager)
#else
import Control.Monad.Trans.Either
import Web.HttpApiData (ToHttpApiData(..))  
#endif

-- | Query format query param for the Yahoo finance webservice APIs.
-- Normally should be the string @json@.
newtype QueryFormat = QueryFormat { unQueryFormat :: Text }
#if MIN_VERSION_servant(0, 5, 0)
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)
#else
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable, ToText)
#endif 

instance ToHttpApiData QueryFormat where
    toQueryParam :: QueryFormat -> Text
    toQueryParam = unQueryFormat

-- | View type query param for the Yahoo finance webservice APIs.
-- Normally should be the string @detail@.
newtype ViewType = ViewType { unViewType :: Text }
#if MIN_VERSION_servant(0, 5, 0)
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)
#else
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable, ToText)
#endif 

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

-- | Newtype wrapper around a list of 'Quote's.
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

-- | Low-level Servant definition of the Yahoo Finance webservice API.
type YahooFinanceJsonApi
    = "webservice"
    :> "v1"
    :> "symbols"
    :> Capture "symbol_list" [StockSymbol]
    :> "quote"
    :> QueryParam "format" QueryFormat
    :> QueryParam "view" ViewType
    :> Get '[JSON] QuoteList

-- | 'BaseUrl' for the Yahoo Finance webservice API.  This represents
-- @https://finance.yahoo.com@.
yahooFinanceJsonBaseUrl :: BaseUrl
yahooFinanceJsonBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "finance.yahoo.com"
    , baseUrlPort = 443
#if MIN_VERSION_servant(0, 5, 0)    
    , baseUrlPath = "/"
#endif    
    }

-- | The order of arguments slightly changes for each version of servant. 
#if MIN_VERSION_servant(0, 9, 0)
getQuoteLowLevel :: [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> ClientM QuoteList
#elif MIN_VERSION_servant(0, 6, 0)
getQuoteLowLevel :: [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> Manager -> BaseUrl -> ExceptT ServantError IO QuoteList
#elif MIN_VERSION_servant(0, 5, 0) 
getQuoteLowLevel :: BaseUrl -> Manager -> [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> ExceptT ServantError IO QuoteList
#else
getQuoteLowLevel :: BaseUrl -> [StockSymbol] -> Maybe QueryFormat -> Maybe ViewType -> EitherT ServantError IO QuoteList
#endif
getQuoteLowLevel = client (Proxy :: Proxy YahooFinanceJsonApi)
