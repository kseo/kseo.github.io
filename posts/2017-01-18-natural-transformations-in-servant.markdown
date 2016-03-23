---
title: Natural transformations in Servant
tags: servant, natural transformation, monad morphism
author: Kwang Yul Seo
---
I've recently started using [servant][servant] at work. Servant lets us declare web APIs at the type-level once and use those APIs to write servers, obtains client functions and generate documentation. It's a real world example which shows the power of Haskell type system.

The most interesting part of Servant is its *extensible type-level DSL* for describing web APIs. However, I found another interesting application of theory into practice in [servant-server][servant-server] library. It is the use of *natural transformation* to convert one handler type into another handler type.

In Servant, `Handler` is a type alias for `ExceptT ServantErr IO`.

```haskell
type Handler = ExceptT ServantErr IO
```

Thus `Handler` monad allows us to do:

* Perform IO operations such as database query through the base monad `IO`.
* Throw a `ServantErr` if something went wrong.

Here's an example of a Servant handler.

```haskell
type ItemApi =
    "item" :> Capture "itemId" Integer :> Get '[JSON] Item

queryItemFirst :: Integer -> IO (Maybe Item)
queryItemFirst itemId = ...

getItemById :: Integer -> Handler Item
getItemById itemId = do
  mItem <- liftIO $ queryItemFirst itemId
  case mItem of
    Just item -> return item
    Nothing   -> throwError err404
```

So far so good, but what if `queryItemFirst` needs a database connection to retrieve the item? Ideally, we would like to create a custom monad for our application such as

```haskell
data AppEnv = AppEnv { db :: ConnectionPool }
type MyHandler = ReaderT AppEnv (ExceptT ServantErr IO)

queryItemFirst :: ConnectionPool -> Integer -> IO (Maybe Item)
queryItemFirst cp itemId = ...

getItemById :: Integer -> MyHandler Item
getItemById itemId = do
  cp <- db <$> ask
  mItem <- liftIO $ queryItemFirst cp itemId
  case mItem of
    Just item -> return item
    Nothing   -> throwError err404
```

Unfortunately, this does not work because [serve][serve] wants `Handler` type. We need a way to transform `MyHandler` into `Handler` so that Servant can happily serve our handlers. Because both `MyHandler` and `Handler` are monads, we need a monad morphism. Or more generally, we need a *natural transformation* from `MyHandler` to `Handler`.

Servant provides a newtype wrapper `Nat` which represents a natural transformation from `m a` to `n a`.

```haskell
newtype m :~> n = Nat { unNat :: forall a. m a -> n a}
```

So what we want is `MyHandler :~> Handler`.

```haskell
myHandlerToHandler :: AppEnv -> MyHandler :~> Handler
myHandlerToHandler env = Nat myHandlerToHandler'
  where
  myHandlerToHandler' :: MyHandler a -> Handler a
  myHandlerToHandler' h = runReaderT h env
```

Okay, now we can get a natural transformation `MyHandler :~> Handler` by applying an `AppEnv` to `myHandlerToHandler`. How can I tell the Servant to use this natural transformation to serve our handlers? That's what [enter][enter] does!

```haskell
server :: AppEnv -> Server ItemApi
server env =
  enter (myHandlerToHandler env) getItemById
```

Wrapping `Handler` with `ReaderT` is a common idiom, so Servant provides a convenient function `runReaderTNat` which is exactly the same to `myHandlerToHandler`. So we can rewrite `server` as follows:

```haskell
server :: AppEnv -> Server ItemApi
server env =
  enter (runReaderTNat env) getItemById
```

Servant also provides a lot of monad morphisms such as `hoistNat`, `embedNat`, `squashNat` and `generalizeNat`. Sounds familiar? These are just wrappers around [mmorph][mmorph] library functions. Interested readers are referred to Gabriel Gonzalez's article [mmorph-1.0.0: Monad morphisms][mmorph-monad-morphisms].

In object-oriented programming, we use *Adapter pattern* to allow the interface of an existing class to be used as another interface. In functional programming, we use *natural transformations* (or more generally, *functors*) to do so!

[servant]: http://haskell-servant.readthedocs.io/en/stable/
[servant-server]: https://hackage.haskell.org/package/servant-server
[serve]: https://www.stackage.org/haddock/lts-7.12/servant-server-0.8.1/Servant-Server.html#v:serve
[enter]: https://www.stackage.org/haddock/lts-7.12/servant-server-0.8.1/Servant-Server.html#v:enter
[mmorph]: https://hackage.haskell.org/package/mmorph
[mmorph-monad-morphisms]: http://www.haskellforall.com/2013/03/mmorph-100-monad-morphisms.html
