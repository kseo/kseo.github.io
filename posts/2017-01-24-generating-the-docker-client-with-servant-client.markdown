---
title: Generating the Docker client with servant-client
tags: docker, API, servant, servant-client
author: Kwang Yul Seo
---
[Servant][servant] provides a type-level DSL for declaring web APIs. Once we write the specification with the DSL, we can do various things including:

* Write servers (this part of servant can be considered a web framework),
* Obtain client functions (in Haskell),
* Generate client functions for other programming languages,
* Generate documentation for your web applications

The primary use case of Servant is to write servers, but we can use [servant-client][servant-client] to generate client functions for the pre-existing web servers too! In this post, I will show you how we can generate client functions for the [Docket remote API][docker-api] automatically with servant-client.

<!--more-->

# API specification

To make the exposition simple, we will specify only three APIs: `ping`, `version` and `containerList`.

The simplest API is [Ping][Ping] which tests if the server is accessible. Its path is `/v1.25/_ping` and it returns `OK` as a plain text with status code 200. We can succinctly describe this endpoint with Servant's type-level DSL.

```haskell
type Ping = "_ping" :> Get '[PlainText] Text
```

[Version][Version] is a slightly more complex API which returns the version information as JSON. `Version` data type has the required fields and it declares an instance of `FromJSON` for unmarshalling JSON data into `Version`. `fieldLabelModifier` is used to bridge JSON field names to `Version` field names.

```haskell
type Version = "version" :> Get '[JSON] Version

data Version = Version
  { versionVersion       :: Text
  , versionApiVersion    :: Text
  , versionMinAPIVersion :: Text
  , versionGitCommit     :: Text
  , versionGoVersion     :: Text
  , versionOs            :: Text
  , versionArch          :: Text
  , versionKernelVersion :: Text
  , versionExperimental  :: Bool
  , versionBuildTime     :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = stripPrefix "version" }

stripPrefix :: String -> String -> String
stripPrefix prefix = fromJust . DL.stripPrefix prefix
```

Finally, [ContainerList][ContainerList] returns the list of containers. The API takes optional query parameters such as `all`, `limit`, `size` and `filters` as specified follows. We created a newtype wrapper for `ContainerID` and declared `FromJSON` instances for `ContainerID` and `Container`. Some fields are omitted for brevity.

```haskell
type ContainerList = "containers" :> "json" :> QueryParam "all" Bool
                                            :> QueryParam "limit" Int
                                            :> QueryParam "size" Bool
                                            :> QueryParam "filters" Text
                                            :> Get '[JSON] [Container]

newtype ContainerID = ContainerID Text
  deriving (Eq, Show, Generic)

instance FromJSON ContainerID

data Container = Container
  { containerId               :: ContainerID
  , containerNames            :: [Text]
  , containerImage            :: Text
  , containerImageID          :: ImageID
  , containerCommand          :: Text
  , containerCreated          :: Int
  -- FIXME: Add Ports
  , containerSizeRw           :: Maybe Int
  , containerSizeRootFs       :: Maybe Int
  -- FIXME: Add Labels
  , containerState            :: Text
  , containerStatus           :: Text
  -- FIXME: Add HostConfig
  -- FIXME: Add NetworkSettings
  -- FIXME: Add Mounts
  } deriving (Show, Eq, Generic)
```

Our API is just the combination of these endpoints.

```haskell
type Api = Ping :<|> Version :<|> ContainerList
```

# API Versioning

Because the Docker remote API has many versions, it adds a version prefix in the path. Servant allows us to expression this version scheme by declaring a new Api with the version prefix.

```haskell
type ApiV1_25 = "v1.25" :> Api
```

We can also mix-and-match many endpoints as the Docker remote API changes. Let'a assume that the docker API version v1.26 changed the specification of the Version endpoint. We can reuse unchanged endpoints by replacing only the changed endpoints with new ones.

```haskell
type Version1_26 = ...
type ApiV1_26 = "v1.26" :> (Ping :<|> Version1_26 :<|> ContainerList)
```

# Generating Client Functions

Now it's time to generate client functions from the specification. It's super easy! We can simply pass our API to `client` function.

```haskell
ping :: ClientM Text
version :: ClientM Version
containerList' :: Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Text -> ClientM [Container]

ping
  :<|> version
  :<|> containerList' = client apiV1_25
```

`ping` and `version` functions are okay, but the signature `containerList'` is a bit confusing. We have to pass four `Maybe` values and two of them have the `Bool` type and it is not easy to remember the order of the arguments. We can improve the function by declaring a wrapper function `containerList`. It takes a `ContainerListOptions`, and the users of the function can pass `defaultContainerListOptions` as the default value.

```haskell
data ContainerListOptions = ContainerListOptions
  { containerListOptionAll     :: Maybe Bool
  , containerListOptionLimit   :: Maybe Int
  , containerListOptionSize    :: Maybe Bool
  , containerListOptionFilters :: Maybe Text
  } deriving (Eq, Show)

defaultContainerListOptions :: ContainerListOptions
defaultContainerListOptions = ContainerListOptions
  { containerListOptionAll     = Just False
  , containerListOptionLimit   = Nothing
  , containerListOptionSize    = Just False
  , containerListOptionFilters = Nothing
  }

containerList :: ContainerListOptions -> ClientM [Container]
containerList opt = containerList' (containerListOptionAll opt)
                                   (containerListOptionLimit opt)
                                   (containerListOptionSize opt)
                                   (containerListOptionFilters opt)
```

Because the expressiveness of Haskell is much more powerful than that of the REST API specification, these wrappings are somewhat unavoidable to make our client functions more Haskell-friendly.

# Using Client Functions

Now our client functions for the Docker API is ready. We need to prepare a `ClientEnv` by passing the host, port and url prefix of the server. We also created a custom connection manager which uses the domain socket for communication because the Docker server listens on the domain socket by default. Interested readers are referred to my previous article [Custom connection manager for http-client][custom-connection-manager] for the implementation details of `newUnixSocketManager`.

```haskell
query :: ClientM [Container]
query = do
  ok <- ping
  liftIO $  print ok
  version <- version
  liftIO $ print (versionVersion version)
  containerList defaultContainerListOptions

app :: String -> Int -> IO ()
app host port = do
  manager <- newUnixSocketManager "/var/run/docker.sock"
  res <- runClientM query (ClientEnv manager (BaseUrl Http host port ""))
  case res of
    Left err          -> putStrLn $ "Error: " ++ show err
    Right containers  -> mapM_ print containers
```

Because `ClientM` is a monad, we can combine multiple monadic actions into one. `query` function pings the server, queries the version information and then request the list of containers.

# Swagger

So far I manually specified the API with Servant's DSL, but if the server has the [Swagger][swagger] specification we can even generate the Servant DSL from the Swagger specification. [swagger-codegen][swagger-codegen] has the [HaskellServantCodegen][HaskellServantCodegen], so we can use it! (I haven't tried it yet.)

# Wrap-up

Writing client functions for existing servers are boring and repetitive. With *servant-client*, we no longer need to write these functions. We just specify the API and Servant writes the client functions for us. Have fun with Servant!

[servant]: http://haskell-servant.readthedocs.io/en/stable/
[servant-client]: https://hackage.haskell.org/package/servant-client
[docker-api]: https://docs.docker.com/engine/api/
[Ping]: https://docs.docker.com/engine/api/v1.25/#operation/SystemPing
[Version]: https://docs.docker.com/engine/api/v1.25/#operation/SystemVersion
[ContainerList]: https://docs.docker.com/engine/api/v1.25/#operation/ContainerList
[custom-connection-manager]: https://kseo.github.io/posts/2017-01-23-custom-connection-manager-for-http-client.html
[swagger]: http://swagger.io/
[swagger-codegen]: https://github.com/swagger-api/swagger-codegen
[HaskellServantCodegen]: https://github.com/swagger-api/swagger-codegen/blob/master/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/HaskellServantCodegen.java