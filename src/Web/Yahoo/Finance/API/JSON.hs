{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

getQuoteTrans
    :: [StockSymbol] -> ReaderT Manager (ExceptT ServantError IO) QuoteList
getQuoteTrans = getQuote
