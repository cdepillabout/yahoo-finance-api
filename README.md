
Web.Yahoo.Finance
=================

[![Hackage](https://img.shields.io/hackage/v/yahoo-finance-api.svg)](https://hackage.haskell.org/package/yahoo-finance-api) [![Build Status](https://secure.travis-ci.org/cdepillabout/yahoo-finance-api.svg)](http://travis-ci.org/cdepillabout/yahoo-finance-api)

This Haskell module exports functions for reading stock quotes from the Yahoo Finance APIs.

## Usage

Currently, only the Yahoo Finance YQL API is implemented.  They can be
accessed with methods in
[Web.Yahoo.Finance.API.JSON](https://hackage.haskell.org/package/yahoo-finance-api/docs/Web-Yahoo-Finance-API-JSON.html).
The Yahoo no longer supports the Yahoo Finance webservice so we have removed the
related querying code.

```haskell
λ> manager <- getGlobalManager
λ> res <- runClientM (getQuotes (YQLQuery [StockSymbol "GOOG", StockSymbol "AA"]) ) (ClientEnv manager yahooFinanceJsonBaseUrl)
λ> print res
```

This produces output like the following:

```
Right
  ( YQLResponse
    { responseCount = 2
    , responseCreated = 2016-10-09 13:44:49 UTC
    , responseLang = "en-US"
    , responseQuotes =
      [ Just
        ( Quote
          { quoteAverageDailyVolume = "1296480"
          , quoteChange = "-1.78"
          , quoteDaysLow = "770.75"
          , quoteDaysHigh = "779.66"
          , quoteYearLow = "639.01"
          , quoteYearHigh = "789.87"
          , quoteMarketCapitalization = "532.69B"
          , quoteLastTradePriceOnly = "775.08"
          , quoteDaysRange = "770.75 - 779.66"
          , quoteName = "Alphabet Inc."
          , quoteSymbol = "GOOG"
          , quoteVolume = "933158"
          , quoteStockExchange = "NMS"
          }
        )
      , Just
        (Quote
          { quoteAverageDailyVolume = "5999180"
          , quoteChange = "-0.41"
          , quoteDaysLow = "31.03"
          , quoteDaysHigh = "32.10"
          , quoteYearLow = "18.42"
          , quoteYearHigh = "34.50"
          , quoteMarketCapitalization = "41.26B"
          , quoteLastTradePriceOnly = "31.37"
          , quoteDaysRange = "31.03 - 32.10"
          , quoteName = "Alcoa Inc. Common Stock"
          , quoteSymbol = "AA"
          , quoteVolume = "7858603"
          , quoteStockExchange = "NYQ"
          }
        )
      ]
    }
  )
```

For other examples refer to `test/Web/Yahoo/FinanceSpec.hs`.
