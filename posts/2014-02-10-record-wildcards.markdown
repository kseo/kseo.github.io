---
title: Record wildcards
tags: Haskell, record wildcards, language extension
author: Kwang Yul Seo
---

Haskell record syntax is a bit verbose. For records with many fields, it is
tiresome to write each field individually in a record pattern, as in

```haskell
data C = C {a :: Int, b :: Int, c :: Int, d :: Int}

f (C {a = 1, b = b, c = c, d = d}) = b + c + d
```

[Record wildcard syntax][record-wildcards] lets us use `..` in a record pattern,
which simplifies pattern `f=f` to `f`. The above pattern can be rewritten with
record wildcards syntax

```haskell
f (C {a = 1, ..}) = b + c + d
```

This simple example does not show the merit of record wildcards vividly. Let’s
see a real world example. [hs-java][hs-java] is a package written by Ilya V.
Portnov, which provides data types for Java .class files format and functions to
assemble/disassemble Java bytecode.

The datatype for a JVM class file is `Class`, which has many fields as in

```haskell
data Class stage = Class {
  magic :: Word32,                         -- ^ Magic value: 0xCAFEBABE
  minorVersion :: Word16,
  majorVersion :: Word16,
  constsPoolSize :: Word16,                -- ^ Number of items in constants pool
  constsPool :: Pool stage,                -- ^ Constants pool itself
  accessFlags :: AccessFlags stage,        -- ^ See @JVM.Types.AccessFlag@
  thisClass :: Link stage B.ByteString,    -- ^ Constants pool item index for this class
  superClass :: Link stage B.ByteString,   -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount :: Word16,               -- ^ Number of implemented interfaces
  interfaces :: [Link stage B.ByteString], -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount :: Word16,              -- ^ Number of class fileds
  classFields :: [Field stage],            -- ^ Class fields
  classMethodsCount :: Word16,             -- ^ Number of class methods
  classMethods :: [Method stage],          -- ^ Class methods
  classAttributesCount :: Word16,          -- ^ Number of class attributes
  classAttributes :: Attributes stage      -- ^ Class attributes
  }
```

It is declared as an instance of [Binary][binary] class for serialization. Its
put method uses the record wildcards syntax not to repeat field names as in the
following:

```haskell
instance Binary (Class File) where
  put (Class {..}) = do
    put magic
    put minorVersion
    put majorVersion
    putPool constsPool
    put accessFlags
    put thisClass
    put superClass
    put interfacesCount
    forM_ interfaces put
    put classFieldsCount
    forM_ classFields put
    put classMethodsCount
    forM_ classMethods put
    put classAttributesCount
    forM_ (attributesList classAttributes) put
```

You can see the real difference by comparing this with a more verbose version
which does not use record wildcards.

```haskell
instance Binary (Class File) where
  put (Class {magic=magic, minorVersion=minorVersion, majorVersion=majorVersion, constsPool=constsPool, accessFlags=accessFlags, thisCla    ss=thisClass, superClass=superClass, interfacesCount=interfacesCount, interfaces=interfaces, classFieldsCount=classFieldsCount, classFie    lds=classFields, classMethodsCount=classMethodsCount, classMethods=classMethods, classAttributesCount=classAttributesCount, classAttributes=classAttributes}) = do
 ...
```

[record-wildcards]: https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
[hs-java]: http://hackage.haskell.org/package/hs-java
[binary]: http://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary.html
