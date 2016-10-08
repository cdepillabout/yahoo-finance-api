{-# LANGUAGE OverloadedStrings #-}

module Web.Yahoo.FinanceSpec (
   main
 , spec
 ) where

import Network.HTTP.Client.TLS (getGlobalManager)
import Web.Yahoo.Finance.YQL.Types
import Test.Hspec  
import Servant.Client

main :: IO ()
main = hspec spec

spec :: Spec 
spec = do 
  describe "getQuotes" $ do 
    it "should retrieve 'GOOG' stock information (assuming the stock with symbol 'GOOG' exists)" $ do
      manager <- getGlobalManager
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "GOOG"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (quoteSymbol . head . responseQuotes $ qs) `shouldBe` "GOOG"

    it "should retrieve multiples quotes" $ do
      manager <- getGlobalManager      
      res <- runClientM (getQuotes (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
      case res of
        Left  err -> fail $ "Query failed: " ++ show err
        Right qs -> (length . responseQuotes $ qs) `shouldBe` 2
      
