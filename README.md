
Web.Yahoo.Finance
=================

[![Hackage](https://img.shields.io/hackage/v/yahoo-finance-api.svg)](https://hackage.haskell.org/package/yahoo-finance-api) [![Build Status](https://secure.travis-ci.org/cdepillabout/yahoo-finance-api.svg)](http://travis-ci.org/cdepillabout/yahoo-finance-api)

This Haskell module exports functions for reading stock quotes from the Yahoo Finance APIs.

## Usage

Currently, only the Yahoo Finance webservice APIs are implemented.  They can be
accessed with methods in
[Web.Yahoo.Finance.API.JSON](https://hackage.haskell.org/package/yahoo-finance-api/docs/Web-Yahoo-Finance-API-JSON.html).

```haskell
(manager :: Manager) <- getGlobalManager
(eitherRes :: Either ServantError QuoteList) <-
    runExceptT $ runReaderT (getQuote ["VGTSX", "GOOG"]) manager
let (res :: QuoteList) =
        either undefined id eitherRes -- Warning: this is unsafe...
show $ unQuoteList res
```

This produces output like the following:

```
[ Quote
    { quoteChange = "0.050000"
    , quoteChangePercent = "0.350141"
    , quoteDayHigh = "0.000000"
    , quoteDayLow = "0.000000"
    , quoteIssuerName = "Vanguard Total Intl Stock Index Inv"
    , quoteIssuerNameLang = "Vanguard Total Intl Stock Index Inv"
    , quoteName = "Vanguard Total International St"
    , quotePrice = "14.330000"
    , quoteSymbol = "VGTSX"
    , quoteTS = "1467413100"
    , quoteType = "mutualfund"
    , quoteUTCTime = 2016-07-01 22:45:00 UTC
    , quoteVolume = "0"
    , quoteYearHigh = "16.330000"
    , quoteYearLow = "12.760000"
    }
, Quote
    { quoteChange = "7.110046"
    , quoteChangePercent = "1.027315"
    , quoteDayHigh = "700.650024"
    , quoteDayLow = "692.130127"
    , quoteIssuerName = "Alphabet Inc."
    , quoteIssuerNameLang = "Alphabet Inc."
    , quoteName = "Alphabet Inc."
    , quotePrice = "699.210022"
    , quoteSymbol = "GOOG"
    , quoteTS = "1467403200"
    , quoteType = "equity"
    , quoteUTCTime = 2016-07-01 20:00:00 UTC
    , quoteVolume = "1344710"
    , quoteYearHigh = "789.870000"
    , quoteYearLow = "515.180000"
    }
]
```
