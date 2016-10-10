{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yahoo.FinanceSpec (
   main
 , spec
 ) where

import Data.Maybe (isJust)
import Network.HTTP.Client.TLS (getGlobalManager)
import Servant.Client
import Test.Hspec  
import Web.Yahoo.Finance.YQL

#if MIN_VERSION_servant(0,9,0)
#elif MIN_VERSION_servant(0,5,0)
import Control.Monad.Except
#else
import Control.Monad.Trans.Either
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
#if MIN_VERSION_servant(0,5,0)
  manager <- runIO getGlobalManager
#endif
  describe "getQuotes" $ do
    it "should retrieve 'GOOG' stock information (assuming the stock with symbol 'GOOG' exists)" $ do
#if MIN_VERSION_servant(0,9,0)
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "GOOG"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,6,0)
      res <- runExceptT (getQuotes (YQLQuery [StockSymbol "GOOG"]) manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,5,0)
      res <- runExceptT (getQuotes yahooFinanceJsonBaseUrl manager (YQLQuery [StockSymbol "GOOG"]))
#else
      res <- runEitherT (getQuotes yahooFinanceJsonBaseUrl (YQLQuery [StockSymbol "GOOG"]))
#endif
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (isJust <$> responseQuotes qs) `shouldBe` [True]

    it "should return [Nothing] for a single non-existent StockSymbol" $ do
#if MIN_VERSION_servant(0,9,0)
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "FOOBAR"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,6,0)
      res <- runExceptT (getQuotes (YQLQuery [StockSymbol "FOOBAR"]) manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,5,0)
      res <- runExceptT (getQuotes yahooFinanceJsonBaseUrl manager (YQLQuery [StockSymbol "FOOBAR"]))
#else
      res <- runEitherT (getQuotes yahooFinanceJsonBaseUrl (YQLQuery [StockSymbol "FOOBAR"]))
#endif
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (responseQuotes qs) `shouldBe` [Nothing]


    it "should retrieve multiples quotes" $ do
#if MIN_VERSION_servant(0,9,0)
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,6,0)
      res <- runExceptT (getQuotes (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]) manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,5,0)
      res <- runExceptT (getQuotes yahooFinanceJsonBaseUrl manager (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]))
#else
      res <- runEitherT (getQuotes yahooFinanceJsonBaseUrl (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]))
#endif
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (length . responseQuotes $ qs) `shouldBe` 2

    it "should return nothing for StockSymbols that do not exist" $ do
#if MIN_VERSION_servant(0,9,0)
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "FOOBAR", StockSymbol "BARFOO"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,6,0)
      res <- runExceptT (getQuotes (YQLQuery [StockSymbol "FOOBAR", StockSymbol "BARFOO"]) manager yahooFinanceJsonBaseUrl)
#elif MIN_VERSION_servant(0,5,0)
      res <- runExceptT (getQuotes yahooFinanceJsonBaseUrl manager (YQLQuery [StockSymbol "FOOBAR", StockSymbol "BARFOO"]))
#else
      res <- runEitherT (getQuotes yahooFinanceJsonBaseUrl (YQLQuery [StockSymbol "FOOBAR", StockSymbol "BARFOO"]))
#endif
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (responseQuotes qs) `shouldBe` [Nothing,Nothing]
