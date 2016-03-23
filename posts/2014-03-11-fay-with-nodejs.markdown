---
title: Fay + Node.js
tags: Fay, node.js
author: Kwang Yul Seo
---

[Fay][fay] is a proper subset of Haskell that compiles to JavaScript. Thus it
is by definition a statically typed lazy pure functional language. If you want
a more thorough introduction to Fay, please read Paul Callaghan’s [Web
Programming in Haskell][callaghan] and [Oliver Charles’s 24 Days of Hackage:
fay][24-fay].

The original intention of Fay is to use Haskell on the client side. If you use a
Haskell web framework such as Yesod or Snap, using Fay you can use the same
language on both client and server sides and some code can actually be shared.

However, because Fay is simply a subset of Haskell that compiles to JavaScript
with no dependencies on the client side, you can use it on the server side too
in combination with Node.js. I am not saying it is actually a good idea to write
server code in Fay, but it is at least fun to investigate the feasibility. Here
is [a web server example][example] written in Fay.

```haskell
{-# LANGUAGE EmptyDataDecls #-}
module Hello where
```

`EmptyDataDecls` is required because JavaScript types are represented by empty
data declarations in Fay.

```haskell
import FFI
```

FFI module provides a foreign function interface.

```haskell
data Http
data HttpServer
data Request
data Response
```

`Http`, `HttpServer`, `Request` and `Response` are JavaScript types we use in
this example. They are represented by empty data declarations.

```haskell
requireHttp :: Fay Http
requireHttp = ffi "require('http')"
```

This is a simple example of a FFI declaration. It returns the result of
`require('http')` as a `Http` instance. Fay is a monad which is similar to IO
monad.  Because a FFI function often has side effects, Fay monad is used to
represent this.

```haskell
createServer :: Http -> (Request -> Response -> Fay ()) -> Fay HttpServer
createServer = ffi "%1.createServer(%2)"
 
consoleLog :: String -> Fay ()
consoleLog = ffi "console.log(%1)"
 
listen :: HttpServer -> Int -> String -> Fay ()
listen = ffi "%1.listen(%2, %3)"
  
writeHead :: Response -> Int -> String -> Fay ()
writeHead = ffi "%1.writeHead(%2, %3)"
  
end :: Response -> String -> Fay ()
end = ffi "%1.end(%2)"
```

These FFI declarations use `%1`, `%2` that corresponds to the arguments we
specify in the type. Most Fay types are automatically serialized and
deserialized. Note that we can only use point free style in FFI functions.

```haskell
main :: Fay ()
main = do
  http <- requireHttp
  server <- createServer http (\req res -> do
    writeHead res 200 "{ 'Content-Type': 'text/plain' }"
    end res "Hello World\n"
    )
  listen server 1337 "127.0.0.1"
  consoleLog "Server running at http://127.0.0.1:1337/"
```

`main` is the entry point to our web server example. Its return type is `Fay ()`
because a Fay program can't do anything without interacting with the world
outside. Because we already wrapped all the Node.js APIs we use, we can program
as if we write a normal Haskell program.

Compare our Fay web server program with the original Node.js program. Except
for the FFI bindings, the main code is almost the same as before. However, our
version is much more type-safe!

```haskell
var http = require('http');
http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(1337, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1337/');
```

[fay]: https://github.com/faylang/fay
[callaghan]: https://pragprog.com/magazines/2012-12/web-programming-in-haskell
[24-fay]: https://ocharles.org.uk/blog/posts/2013-12-23-24-days-of-hackage-fay.html
[example]: https://gist.github.com/kseo/9477930
