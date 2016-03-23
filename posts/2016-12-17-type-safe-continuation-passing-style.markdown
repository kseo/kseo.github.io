---
title: Type safe continuation passing style
tags: continuation passing style, type safe
author: Kwang Yul Seo
---
One common mistake in JavaScript programming is to forget to invoke callback in continuation passing style code. For example, the code below may never complete:

```javascript
async.series([
       function (callback) {
           if (..)
              console.log("invalid input"); // BUG: NO callback!
           else
              callback(null, 'ok');
       },
       function (callback) {
           ...
       }
  ],
  function (err, result) { handleErrorOrResult(err, result);} // Might not be reached
);
```

Unfortunately, there is no systematic way to prevent this kind of bug in JavaScript. We can write tests, but it is not practical to write tests which cover all control paths.

But in Haskell, thanks to the powerful type system, we can turn these bugs into type errors! Let's take a look at the definition of `Application` from [Web Application Interface][wai].

```haskell
type Application =
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
```

This signature of `Application` looks similar to `bracket` function. Wai uses continuation passing style to handle resource management in an exception-safe manner.

There is a bonus here. A valid function of `Application` must return a `ResponseReceived`, but we can't create one by ourselves because there is no constructor available. The only way to acquire an `ResponseReceived` value is to invoke the callback. Thus if you accidentally forget to invoke callback, it automatically becomes a type error.

The code snippet below returns `responseReceived` returned from `respond` to make `application` type-check. Otherwise, GHC will complain about the type mismatch.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (fromByteString)
import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)

application _ respond = do
  msg = fromByteString "Hello world!"
  responseReceived <- respond $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
    msg
  return responseReceived

main = run 3000 application
```

NOTE: We could define `Application` using `RankNTypes` GHC extension instead of `ResponseReceived` type. An old version of `Wai` actually used this definition of `Application`.

```haskell
Application = Request -> (forall b. (Response -> IO b) -> IO b)
```

[wai]: http://www.yesodweb.com/book/web-application-interface
