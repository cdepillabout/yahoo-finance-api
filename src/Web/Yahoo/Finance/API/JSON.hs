{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Web.Yahoo.Finance.API.JSON where

import Control.Monad.Except (ExceptT(..), MonadError(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Data.Data (Data)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client (HasHttpManager(..), Manager)
import Servant.API (Capture, Get, JSON, QueryParam, (:>))
import Servant.Client (BaseUrl(..), ServantError, Scheme(..), client)
import Web.HttpApiData (ToHttpApiData(..))

newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData StockSymbol where
    toUrlPiece :: StockSymbol -> Text
    toUrlPiece = unStockSymbol

instance ToHttpApiData [StockSymbol] where
    toUrlPiece :: [StockSymbol] -> Text
    toUrlPiece = fold . intersperse "," . fmap toUrlPiece

newtype QueryFormat = QueryFormat { unQueryFormat :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData QueryFormat where
    toQueryParam :: QueryFormat -> Text
    toQueryParam = unQueryFormat

newtype ViewType = ViewType { unViewType :: Text }
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

instance ToHttpApiData ViewType where
    toQueryParam :: ViewType -> Text
    toQueryParam = unViewType

type YahooFinanceJsonApi
    = "webservice"
    :> "v1"
    :> "symbols"
    :> Capture "symbol_list" [StockSymbol]
    :> "quote"
    :> QueryParam "format" QueryFormat
    :> QueryParam "view" ViewType
    :> Get '[JSON] ()

yahooFinanceJsonBaseUrl :: BaseUrl
yahooFinanceJsonBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "finance.yahoo.com"
    , baseUrlPort = 443
    , baseUrlPath = "/"
    }

getQuote
    :: ( HasHttpManager r
       , MonadError ServantError m
       , MonadIO m
       , MonadReader r m
       )
    => [StockSymbol] -> m ()
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

getQuoteTrans
    :: [StockSymbol] -> ReaderT Manager (ExceptT ServantError IO) ()
getQuoteTrans = getQuote

getQuoteLowLevel
    :: [StockSymbol]
    -> Maybe QueryFormat
    -> Maybe ViewType
    -> Manager
    -> BaseUrl
    -> ExceptT ServantError IO ()
getQuoteLowLevel = client (Proxy :: Proxy YahooFinanceJsonApi)
