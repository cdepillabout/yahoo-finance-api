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

import Servant.API

#if !MIN_VERSION_servant(0,5,0)
import Web.HttpApiData (ToHttpApiData(..))  
#endif

-- | This type is used to represent a stock symbol.
--
-- It can easily be used with the @OverloadedStrings@ extension.
--
-- >>> :set -XOverloadedStrings
-- >>> "GOOG" :: StockSymbol
-- StockSymbol {unStockSymbol = "GOOG"}
{-
newtype StockSymbol = StockSymbol { unStockSymbol :: Text }
#if MIN_VERSION_servant(0, 5, 0)
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)
#else
    deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable, ToText)
#endif 

instance ToHttpApiData StockSymbol where
    toUrlPiece :: StockSymbol -> Text
    toUrlPiece = unStockSymbol
-}
-- | Connect separate 'StockSymbol's with a comma.
--
-- >>> toUrlPiece (["GOOG", "YHOO", "^GSPC"] :: [StockSymbol])
-- "GOOG,YHOO,^GSPC"
{-
instance ToHttpApiData [StockSymbol] where
    toUrlPiece :: [StockSymbol] -> Text
    toUrlPiece = fold . intersperse "," . fmap toUrlPiece


#if !MIN_VERSION_servant(0, 5, 0)
-- | Connect separate 'StockSymbol's with a comma.
instance ToText [StockSymbol] where
  toText :: [StockSymbol] -> Text  
  toText = fold . intersperse "," . fmap toText
#endif
-}

{-
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
-}
{-
data Quote = Quote {
  qAsk
, qAverageDailyValue
, qBid
, qAskRealtime 
} deriving (Eq, Read, Show, Generic)

-}
{-
https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20(%22YHOO%22,%22GOOG%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=


{"query":{"count":2,"created":"2016-10-07T16:03:37Z","lang":"en-US","results":{"quote":[{"symbol":"YHOO","Ask":"43.25","AverageDailyVolume":"11754200","Bid":"43.24","AskRealtime":null,"BidRealtime":null,"BookValue":"29.83","Change_PercentChange":"-0.43 - -0.98%","Change":"-0.43","Commission":null,"Currency":"USD","ChangeRealtime":null,"AfterHoursChangeRealtime":null,"DividendShare":null,"LastTradeDate":"10/7/2016","TradeDate":null,"EarningsShare":"-5.18","ErrorIndicationreturnedforsymbolchangedinvalid":null,"EPSEstimateCurrentYear":"0.49","EPSEstimateNextYear":"0.57","EPSEstimateNextQuarter":"0.17","DaysLow":"43.19","DaysHigh":"43.68","YearLow":"26.15","YearHigh":"44.92","HoldingsGainPercent":null,"AnnualizedGain":null,"HoldingsGain":null,"HoldingsGainPercentRealtime":null,"HoldingsGainRealtime":null,"MoreInfo":null,"OrderBookRealtime":null,"MarketCapitalization":"41.16B","MarketCapRealtime":null,"EBITDA":"151.08M","ChangeFromYearLow":"17.10","PercentChangeFromYearLow":"+65.39%","LastTradeRealtimeWithTime":null,"ChangePercentRealtime":null,"ChangeFromYearHigh":"-1.67","PercebtChangeFromYearHigh":"-3.72%","LastTradeWithTime":"11:48am - <b>43.25</b>","LastTradePriceOnly":"43.25","HighLimit":null,"LowLimit":null,"DaysRange":"43.19 - 43.68","DaysRangeRealtime":null,"FiftydayMovingAverage":"43.17","TwoHundreddayMovingAverage":"38.90","ChangeFromTwoHundreddayMovingAverage":"4.35","PercentChangeFromTwoHundreddayMovingAverage":"+11.19%","ChangeFromFiftydayMovingAverage":"0.08","PercentChangeFromFiftydayMovingAverage":"+0.20%","Name":"Yahoo! Inc.","Notes":null,"Open":"43.26","PreviousClose":"43.68","PricePaid":null,"ChangeinPercent":"-0.98%","PriceSales":"8.50","PriceBook":"1.46","ExDividendDate":null,"PERatio":null,"DividendPayDate":null,"PERatioRealtime":null,"PEGRatio":"-24.57","PriceEPSEstimateCurrentYear":"88.27","PriceEPSEstimateNextYear":"75.88","Symbol":"YHOO","SharesOwned":null,"ShortRatio":"5.05","LastTradeTime":"11:48am","TickerTrend":null,"OneyrTargetPrice":"43.08","Volume":"4072551","HoldingsValue":null,"HoldingsValueRealtime":null,"YearRange":"26.15 - 44.92","DaysValueChange":null,"DaysValueChangeRealtime":null,"StockExchange":"NMS","DividendYield":null,"PercentChange":"-0.98%"},{"symbol":"GOOG","Ask":"771.5500","AverageDailyVolume":"1302160","Bid":"771.2000","AskRealtime":null,"BidRealtime":null,"BookValue":"186.2010","Change_PercentChange":"-5.5496 - -0.7144%","Change":"-5.5496","Commission":null,"Currency":"USD","ChangeRealtime":null,"AfterHoursChangeRealtime":null,"DividendShare":null,"LastTradeDate":"10/7/2016","TradeDate":null,"EarningsShare":"25.8090","ErrorIndicationreturnedforsymbolchangedinvalid":null,"EPSEstimateCurrentYear":"34.1400","EPSEstimateNextYear":"40.4800","EPSEstimateNextQuarter":"9.6700","DaysLow":"770.9700","DaysHigh":"779.6600","YearLow":"635.3180","YearHigh":"789.8700","HoldingsGainPercent":null,"AnnualizedGain":null,"HoldingsGain":null,"HoldingsGainPercentRealtime":null,"HoldingsGainRealtime":null,"MoreInfo":null,"OrderBookRealtime":null,"MarketCapitalization":"530.10B","MarketCapRealtime":null,"EBITDA":"26.90B","ChangeFromYearLow":"135.9924","PercentChangeFromYearLow":"+21.4054%","LastTradeRealtimeWithTime":null,"ChangePercentRealtime":null,"ChangeFromYearHigh":"-18.5596","PercebtChangeFromYearHigh":"-2.3497%","LastTradeWithTime":"11:48am - <b>771.3104</b>","LastTradePriceOnly":"771.3104","HighLimit":null,"LowLimit":null,"DaysRange":"770.9700 - 779.6600","DaysRangeRealtime":null,"FiftydayMovingAverage":"773.2100","TwoHundreddayMovingAverage":"740.0570","ChangeFromTwoHundreddayMovingAverage":"31.2534","PercentChangeFromTwoHundreddayMovingAverage":"+4.2231%","ChangeFromFiftydayMovingAverage":"-1.8996","PercentChangeFromFiftydayMovingAverage":"-0.2457%","Name":"Alphabet Inc.","Notes":null,"Open":"779.6600","PreviousClose":"776.8600","PricePaid":null,"ChangeinPercent":"-0.7144%","PriceSales":"6.5302","PriceBook":"4.1722","ExDividendDate":null,"PERatio":"29.8853","DividendPayDate":null,"PERatioRealtime":null,"PEGRatio":"1.2600","PriceEPSEstimateCurrentYear":"22.5926","PriceEPSEstimateNextYear":"19.0541","Symbol":"GOOG","SharesOwned":null,"ShortRatio":"1.8200","LastTradeTime":"11:48am","TickerTrend":null,"OneyrTargetPrice":"929.0800","Volume":"340589","HoldingsValue":null,"HoldingsValueRealtime":null,"YearRange":"635.3180 - 789.8700","DaysValueChange":null,"DaysValueChangeRealtime":null,"StockExchange":"NMS","DividendYield":null,"PercentChange":"-0.7144%"}]}}}



"symbol": "GOOG",
"Ask": "771.5500",
"AverageDailyVolume": "1302160",
"Bid": "771.2000",
"AskRealtime": null,
"BidRealtime": null,
"BookValue": "186.2010",
"Change_PercentChange": "-5.5496 - -0.7144%",
"Change": "-5.5496",
"Commission": null,
"Currency": "USD",
"ChangeRealtime": null,
"AfterHoursChangeRealtime": null,
"DividendShare": null,
"LastTradeDate": "10/7/2016",
"TradeDate": null,
"EarningsShare": "25.8090",
"ErrorIndicationreturnedforsymbolchangedinvalid": null,
"EPSEstimateCurrentYear": "34.1400",
"EPSEstimateNextYear": "40.4800",
"EPSEstimateNextQuarter": "9.6700",
"DaysLow": "770.9700",
"DaysHigh": "779.6600",
"YearLow": "635.3180",
"YearHigh": "789.8700",
"HoldingsGainPercent": null,
"AnnualizedGain": null,
"HoldingsGain": null,
"HoldingsGainPercentRealtime": null,
"HoldingsGainRealtime": null,
"MoreInfo": null,
"OrderBookRealtime": null,
"MarketCapitalization": "530.10B",
"MarketCapRealtime": null,
"EBITDA": "26.90B",
"ChangeFromYearLow": "135.9924",
"PercentChangeFromYearLow": "+21.4054%",
"LastTradeRealtimeWithTime": null,
"ChangePercentRealtime": null,
"ChangeFromYearHigh": "-18.5596",
"PercebtChangeFromYearHigh": "-2.3497%",
"LastTradeWithTime": "11:48am - <b>771.3104</b>",
"LastTradePriceOnly": "771.3104",
"HighLimit": null,
"LowLimit": null,
"DaysRange": "770.9700 - 779.6600",
"DaysRangeRealtime": null,
"FiftydayMovingAverage": "773.2100",
"TwoHundreddayMovingAverage": "740.0570",
"ChangeFromTwoHundreddayMovingAverage": "31.2534",
"PercentChangeFromTwoHundreddayMovingAverage": "+4.2231%",
"ChangeFromFiftydayMovingAverage": "-1.8996",
"PercentChangeFromFiftydayMovingAverage": "-0.2457%",
"Name": "Alphabet Inc.",
"Notes": null,
"Open": "779.6600",
"PreviousClose": "776.8600",
"PricePaid": null,
"ChangeinPercent": "-0.7144%",
"PriceSales": "6.5302",
"PriceBook": "4.1722",
"ExDividendDate": null,
"PERatio": "29.8853",
"DividendPayDate": null,
"PERatioRealtime": null,
"PEGRatio": "1.2600",
"PriceEPSEstimateCurrentYear": "22.5926",
"PriceEPSEstimateNextYear": "19.0541",
"Symbol": "GOOG",
"SharesOwned": null,
"ShortRatio": "1.8200",
"LastTradeTime": "11:48am",
"TickerTrend": null,
"OneyrTargetPrice": "929.0800",
"Volume": "340589",
"HoldingsValue": null,
"HoldingsValueRealtime": null,
"YearRange": "635.3180 - 789.8700",
"DaysValueChange": null,
"DaysValueChangeRealtime": null,
"StockExchange": "NMS",
"DividendYield": null,
"PercentChange": "-0.7144%"
-}
