{-# LANGUAGE OverloadedStrings #-}

module Web.Yahoo.FinanceSpec (
   main
 , spec
 ) where

import Test.Hspec  
-- import Web.Yahoo.Finance
-- import Web.Yahoo.Finance.API.JSON
-- import Web.Yahoo.Finance.API.JSON.Internal

import Network.HTTP.Client
import Servant.API
import Servant.Client
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Network.HTTP.Client.TLS (getGlobalManager)

import Web.Yahoo.Finance.API.YQL


import Safe (headMay)

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "getQuote" $
  it "should retrieve 'GOOG' stock information (assuming 'GOOG' exists and internet connection is working)" $ do
    -- manager <- newManager defaultManagerSettings
    -- manager <- reader getHttpManager :: IO Manager
    manager <- getGlobalManager
    res     <- runClientM (getQuote (Just $ YQLQuery [StockSymbol "GOOG"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
    case res of
      Left  err -> fail $ "Query failed: " ++ show err
      Right qs -> (quoteSymbol . head . responseQuotes $ qs) `shouldBe` "GOOG"
      -- Right qs -> (length . responseQuotes $ qs) `shouldBe` 1
      {-
      Right quotes -> 
        case headMay (unQuoteList quotes) of
          Nothing -> fail $ "Unable to find 'GOOG' stock."
          Just q  -> -- quoteSymbol q `shouldBe` "GOOG"
            True `shouldBe` True
      -}
