---
title: Calling C library functions dynamically in Haskell
tags: Haskell, FFI, C
author: Kwang Yul Seo
---

Haskell FFI is used to call functions from C and for C to call Haskell
functions. A classic example from Haskell Wiki page [FFI
Introduction][ffi-intro] is to call a C library function, `sin` from Haskell. In
this method, you have to declare the type of C function you want to call using
`foreign import ccall`.

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C
 
foreign import ccall "sin" c_sin :: CDouble -> CDouble
  
sin1 :: Double -> Double
sin1 d = realToFrac (c_sin (realToFrac d))
```

What if you don’t know the type of a C function you want to call until you run
your program (e.g., JIT compilation). You can’t use `foreign import ccall`
because you don’t know the function type statically. In this case, you can use
the [Haskell binding for libffi][libffi], a Portable Foreign Function Interface
Library.

```haskell
import System.Posix.DynamicLinker
import Foreign.C
import Foreign.Ptr
import Foreign.LibFFI
 
sin2 :: Double -> IO Double
sin2 d = do
    sin <- dlsym Default "sin"
    ret <- callFFI sin retCDouble [argCDouble (realToFrac d)]
    return $ realToFrac ret
```

Now you don’t need to declare it using `foreign import ccall`. You can just
pass the return type and the list of argument types to `callFFI` function.

So far so good, but there is one problem with this approach. `sin2` is no
longer a pure function. The return type must be `IO Double` because calling
`callFFI` function requires IO monad. If you call a impure C function with side
effects, this is okay though.

```haskell
callFFI :: FunPtr a -> RetType b -> [Arg] -> IO b
```

[ffi-intro]: https://wiki.haskell.org/FFI_Introduction
[libffi]: http://hackage.haskell.org/package/libffi-0.1/docs/Foreign-LibFFI.html

