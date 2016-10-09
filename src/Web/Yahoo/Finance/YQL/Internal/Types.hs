{-|
Module      : Web.Yahoo.Finance.YQL.Internal.Types
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

module Web.Yahoo.Finance.YQL.Internal.Types (
    Quote(..)
  , StockSymbol(..)    
  , YQLQuery(..)
  , YQLResponse(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Web.HttpApiData

#if !MIN_VERSION_servant(0, 5, 0)
import Servant.Common.Text
#endif

import qualified Data.Vector as V

-- | Query for yahoo finance api.   
data YQLQuery = YQLQuery {
  yqlQuery :: [StockSymbol]
} deriving (Eq, Show, Generic)

-- | Automate a simple YQL query.
instance ToHttpApiData YQLQuery where
  toUrlPiece :: YQLQuery -> Text
  toUrlPiece (YQLQuery {..}) = "select * from yahoo.finance.quotes where symbol in (" <> toUrlPiece yqlQuery <> ")"

#if !MIN_VERSION_servant(0, 5, 0)
instance ToText YQLQuery where
  toText (YQLQuery {..}) = "select * from yahoo.finance.quotes where symbol in (" <> toText yqlQuery <> ")"
#endif

newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
  deriving (Eq, Generic, Ord, Show)

-- | Surround 'StockSymbol' with double quotes.
instance ToHttpApiData StockSymbol where
  toUrlPiece :: StockSymbol -> Text
  toUrlPiece (StockSymbol {..}) = "\"" <> unStockSymbol <> "\""

#if !MIN_VERSION_servant(0, 5, 0)
instance ToText StockSymbol where
  toText (StockSymbol {..}) = "\"" <> unStockSymbol <> "\""
#endif

-- | Connect separate 'StockSymbol's with a comma.
--
-- >>> toUrlPiece (["GOOG", "YHOO", "^GSPC"] :: [StockSymbol])
-- "GOOG,YHOO,^GSPC"
instance ToHttpApiData [StockSymbol] where
  toUrlPiece :: [StockSymbol] -> Text
  toUrlPiece = fold . intersperse "," . fmap toUrlPiece

#if !MIN_VERSION_servant(0, 5, 0)
instance ToText [StockSymbol] where
  toText = fold . intersperse "," . fmap toUrlPiece
#endif


{-
instance ToText [StockSymbol] where
  toText :: [StockSymbol] -> Text  
  toText = fold . intersperse "," . fmap toText
-}


data YQLResponse = YQLResponse {
  responseCount   :: Int
, responseCreated :: UTCTime
, responseLang    :: Text
, responseQuotes  :: [Maybe Quote]
} deriving (Eq, Read, Show, Generic)

instance FromJSON YQLResponse where
  parseJSON = withObject "YQLResponse" $ \o -> do
    innerO <- o .: "query"
    results <- innerO .: "results"
    
    innerQuotes  <- results .: "quote"
    quotes <- case innerQuotes of 
      (Object o) -> (:[]) <$> (parseJSON innerQuotes <|> pure Nothing) :: Parser [Maybe Quote]
      (Array  a) -> sequence $ (\x -> parseJSON x <|> pure Nothing) <$> (V.toList a)
      
      _ -> return []
    
    --quotes <- ((results .:? "quote" :: (Parser (Maybe Quote))) >>= \q -> return [q]) <|> (results .: "quote" :: Parser [Maybe Quote])
    
    YQLResponse <$> innerO .: "count"
                <*> innerO .: "created"
                <*> innerO .: "lang"
                <*> pure quotes

data Quote = Quote {
  quoteAverageDailyVolume   :: Text
, quoteChange               :: Text
, quoteDaysLow              :: Text
, quoteDaysHigh             :: Text
, quoteYearLow              :: Text
, quoteYearHigh             :: Text
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
