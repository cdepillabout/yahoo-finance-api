{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

{-|
Module      : Web.Yahoo.Finance.Types
Description : General types for all Yahoo Finance APIs.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module contains general types for working with all Yahoo Finance APIs.
-}

module Web.Yahoo.Finance.Types where

import Data.Data (Data)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData(..))

import Servant.API
import Servant.Client

-- | This type is used to represent a stock symbol.
--
-- It can easily be used with the @OverloadedStrings@ extension.
--
-- >>> :set -XOverloadedStrings
-- >>> "GOOG" :: StockSymbol
-- StockSymbol {unStockSymbol = "GOOG"}
newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
#if MIN_VERSION_servant(0, 5, 0)
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)
#else
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable, ToText)
#endif 

instance ToHttpApiData StockSymbol where
    toUrlPiece :: StockSymbol -> Text
    toUrlPiece = unStockSymbol

-- | Connect separate 'StockSymbol's with a comma.
--
-- >>> toUrlPiece (["GOOG", "YHOO", "^GSPC"] :: [StockSymbol])
-- "GOOG,YHOO,^GSPC"
instance ToHttpApiData [StockSymbol] where
    toUrlPiece :: [StockSymbol] -> Text
    toUrlPiece = fold . intersperse "," . fmap toUrlPiece

--instance ToText StockSymbol where
--  toText (StockSymbol x) = x

#if !MIN_VERSION_servant(0, 5, 0)
instance ToText [StockSymbol] where
  toText :: [StockSymbol] -> Text  
  toText = fold . intersperse "," . fmap toText
#endif
