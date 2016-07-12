
### Example Usage

The example is a Servant application, but this package is compatible with any Wai application.

The `logEntriesLogger` produces a chainable `Middleware` type
which can be used in conjunction with any other `Middleware` type.

The relevant code in the example is found in `src/Lib.hs`.

```Haskell
-- The Middleware is chained to Servant's Application
-- Other Middlewares can be attached, as well.
app :: Application
app = requestLogger $ serve api server

-- Configures and creates the LogEntries request logger Middleware
requestLogger :: Middleware
requestLogger =
  let token = fromJust . fromString $ "00000000-0000-0000-0000-000000000000"
      logentriesConfig = Config "data.logentries.com" 80 token
  in logEntriesLogger logentriesConfig
```
