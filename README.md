# logentries

### Example Usage

The `logEntriesLogger` produces a chainable `Middleware` type
which can be used in conjunction with any other `Middleware` type.

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

### Example Application

A [Servant example](https://github.com/toddmohney/master/tree/add-example-app/example)
can be found in the repo.
