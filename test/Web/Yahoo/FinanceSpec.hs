{-# LANGUAGE OverloadedStrings #-}

module Web.Yahoo.FinanceSpec (
   main
 , spec
 ) where

import Test.Hspec  
import Web.Yahoo.Finance
import Web.Yahoo.Finance.API.JSON
import Web.Yahoo.Finance.API.JSON.Internal

import Network.HTTP.Client
import Servant.API
import Servant.Client
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Network.HTTP.Client.TLS (getGlobalManager)


import Safe (headMay)

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "getQuote" $
  it "should retrieve 'GOOG' stock information (assuming 'GOOG' exists and internet connection is working)" $ do
    -- manager <- newManager defaultManagerSettings
    -- manager <- reader getHttpManager :: IO Manager
    manager <- getGlobalManager
    res     <- runClientM (getQuoteLowLevel [StockSymbol "GOOG"] (Just "json") (Just "detail")) (ClientEnv manager yahooFinanceJsonBaseUrl)
    case res of
      Left  err -> fail $ "Query failed: " ++ show err
      Right quotes -> 
        case headMay (unQuoteList quotes) of
          Nothing -> fail $ "Unable to find 'GOOG' stock."
          Just q  -> quoteSymbol q `shouldBe` "GOOG"
