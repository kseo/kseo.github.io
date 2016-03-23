---
title: Custom connection manager for http-client
tags: http-client, Network.HTTP.Client, Manager
author: Kwang Yul Seo
---
[http-client][http-client] provides the low-level API for HTTP client. In this post, I will explain how to create custom connection managers. If you want to know the basics of the library, read [Making HTTP requests][http-client-post] first.

<!--more-->

Every HTTP request is made via a `Manager`. It handles the details of creating connections to the server such as managing a connection pool. It also allows us to configure various settings and setup secure connections (HTTPS).

The easiest way to create one is to use `newManager defaultManagerSettings` as follows.

```haskell
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://httpbin.org/post"
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
```

But the default connection manager is not enough for some cases. One such case is the [docker remote API][docker-remote-api]. Because Docker listens on the unix domain socket by default for security reasons, we can't access to the API with the default connection manager which uses tcp.

But we are not out of luck. We can configure the connection manager to create a *unix domain socket* instead of a *tcp socket* by setting a custom `managerRawConnection` field.

```haskell
managerRawConnection :: ManagerSettings -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
```

It is used by `Manager` to create a new `Connection` from the host and port. So we can make the connection manager to create a unix socket by replacing the default implementation with `openUnixSocket`.

```haskell
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Network.HTTP.Client
import Network.HTTP.Client.Internal (makeConnection)
import Network.HTTP.Types.Status (statusCode)

newUnixDomainSocketManager :: FilePath -> IO Manager
newUnixDomainSocketManager path = do
  let mSettings = defaultManagerSettings { managerRawConnection = return $ openUnixSocket path }
  newManager mSettings
  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection (SBS.recv s 8096)
                     (SBS.sendAll s)
                     (S.close s)
```

By creating a connection manager with "/var/run/docker.sock", we can make a request to the docker. The code below returns the version of the docker.

```haskell
main :: IO ()
main = do
  manager <- newUnixDomainSocketManager
   "/var/run/docker.sock"
  request <- parseRequest "http://192.168.99.100:2376/v1.25/version"
  response <- httpLbs request manager

  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
```

[docker-remote-api]: https://docs.docker.com/engine/api/
[http-client]: https://www.stackage.org/haddock/lts-7.12/http-client-0.4.31.2/Network-HTTP-Client.html
[http-client-post]: https://haskell-lang.org/library/http-client