---
title: Please use Generically instead of DefaultSignatures!
author: Jan van Brügge
date: 2025-06-08
tags: [haskell]
description: "For a long time, the Haskell library author's tool of choice to provide default type class implementations based on `Generic` was `DefaultSignatures`. Since GHC 9.4.1 however, there is a better way: the `Generically` newtype that is in `base`."
---

For a long time, the Haskell library author's tool of choice to provide default type class implementations based on `Generic` was `DefaultSignatures`. Since GHC 9.4.1 however, there is a better way: the `Generically` newtype that is in `base`.


## What are `DefaultSignatures`?

Let's use the example of `aeson` to encode a type in JSON. We have a type class `ToJSON` as well as a function `genericToJSON` that can be used to encode an arbitrary type as long as it derives `GHC.Generics`:

```haskell
class ToJSON a where
    toJSON :: a -> Value

genericToJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
genericToJSON = -- redacted
```

However, to use `genericToJSON` the user has to manually write an instance of the `ToJSON` class:

```haskell
data MyData = MkMyData { foo :: Int }
    deriving Generic

instance ToJSON MyData where
    toJSON = genericToJSON
```

`DefaultSignatures` allow the library author to provide a default implementations that is only available if the type fulfils additional constraints. In our case this addition constraint is that the type implements `Generic`. Together with `DeriveAnyClass` which will create an empty type class instance, this makes it possible for the user to directly derive an instance of `ToJSON`:

```haskell
-- Library.hs
{-# LANGUAGE DefaultSignatures #-}

class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON

-- User.hs
{-# LANGUAGE DeriveAnyClass #-}

data MyData = MkMyData { foo :: Int }
    deriving (Generic, ToJSON)
```

## The problems with `DefaultSignatures`

In my opinion there are two issues here: 1) the reliance on `DeriveAnyClass` which introduces a footgun for the user and 2) `DefaultSignatures` force the author to provide only one way to derive instances

The `DeriveAnyClass` extension allows the user to put any type class in the deriving clause of any datatype, not just the stock derivable classes like `Show` or `Read`. For non-stock classes this deriving statement will create an empty instance:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

data MyData = MkMyData { foo :: Int }
    deriving (Generic, ToJSON)

-- equivalent to

data MyData = MkMyData { foo :: Int }
    deriving Generic

instance ToJSON MyData
```

If you derive a class with no default implementation this will result in a missing method. `-Wmissing-methods` (part of `-Wall`) will warn about this case, but in my opinion it still feels wrong. And I am not alone with this opinion, see for example [Richard Eisenberg's video on the topic](https://www.youtube.com/watch?v=Zdne-Ch2000).

The other big issue with `DefaultSignatures` is that there can only be a single default implementation. In Haskell this is usually an implementation based on `GHC.Generics`. But even then there might be several possible implementations to choose from. Maybe the default `genericToJSON` works for any type, but is not as fast as it could be while a `genericFlatToJSON` is faster, but requires the type to be non-recursive. With `DefaultSignatures` choosing the second implementation will require writing an instance manually again.

Another use case is if you are the author of another library that provides encodings for several structured data formats. Your library requires the user to provide an instance of an `Encoding` class and they can use that to convert their datatypes to YAML, JSON and more. Because `aeson` is so prevalent in the ecosystem you might want to allow the user of your library to derive an instance of `ToJSON` if they already implement `Encoding`. But because `DefaultSignatures` are tied to the class declaration (which sits outside your library, in `aeson`) you are out of luck.

## DerivingVia to the rescue!

Instead of `DeriveAnyClass` and `DefaultSignatures` your library can encourage the use of `DerivingVia`. The `base` library provides the `Generically` newtype since GHC 9.4.1:

```haskell
newtype Generically a = Generically a
```
While this type looks rather useless, it is meant for library authors to provide type class instances based on `GHC.Generics`:

```haskell
class ToJSON a where
    toJSON :: a -> Value

instance (Generic a, GToJSON (Rep a)) => ToJSON (Generically a) where
    toJSON (Generically x) = genericToJSON x
```

The user can then use `DerivingVia` to use this instance for their type:

```haskell
import GHC.Generics (Generic, Generically(..))

data MyData = MkMyData { foo :: Int }
    deriving Generic
    deriving ToJSON via (Generically MyData)
```
This deriving clause makes it clear that we are using `MyData`'s `Generic` instance to derive `ToJSON`. We could also have newtypes with custom `Generic` instance that e.g. convert fields to camel case, and use them together with the `Generically` newtype to derive `ToJSON`.

Going to the example from earlier with the `Encoding` typeclass, we as author of that library could provide an `Encoded` newtype that then has the `ToJSON` instance:

```haskell
data DataRep = -- redacted

class Encoding a where
    encode :: a -> DataRep

encodeJSON :: Encoding a => a -> Value
encodeJSON = -- redacted

newtype Encoded a = MkEncoded a

instance Encoding a => ToJSON (Encoded a) where
    toJSON = encodeJSON
```

A user can then use the same `DerivingVia` clause to derive ToJSON from the `Encoding` instance.

```haskell
data MyData = MkMyData { foo :: Int }
    deriving Generic
    deriving Encoding via (Generically MyData)
    deriving ToJSON via (Encoded MyData)
```

With `StandaloneDeriving` this could also be used to derive `ToJSON` without `Generic` at all:

```haskell
{-# LANGUAGE StandaloneDeriving #-}

data MyData = MkMyData { foo :: Int }

instance Encoding MyData where
    encode = -- custom implementation


deriving via (Encoded MyData) instance ToJSON MyData
```

## Conclusion

If you did not know about `Generically` I hope this article was able to sufficiently explain the benefits of composability of newtypes and `DerivingVia`, especially across libraries. If you are a library author yourself, see this article as a plea to ditch `DefaultSignatures` and provide instances for `Generically` and potentially other newtypes only. Then we might be able to deprecate `DeriveAnyClass` for good.
