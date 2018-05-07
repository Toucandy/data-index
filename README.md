# What is it?

This is an attempt to rethink the concept of list indexing, implemented in 
Haskell. Here are some examples (do `cabal install` before it):

```haskell
Prelude> import Data.Index
Prelude Data.Index> import Data.List.Index as LI
Prelude Data.Index LI> [1..7] LI.!! 1
2
Prelude Data.Index LI> [1..7] LI.!! end
7
Prelude Data.Index LI> [1..7] LI.!! (end-2)
5
Prelude Data.Index LI> [1..7] LI.!! mid
4
Prelude Data.Index LI> LI.drop 13 ['a'..'z']
"nopqrstuvwxyz"
Prelude Data.Index LI> LI.drop mid ['a'..'z']
"nopqrstuvwxyz"
Prelude Data.Index LI> LI.splitAt mid "Hello World!"
("Hello ","World!")
Prelude Data.Index LI> LI.splitAt (mid - 1) "Hello World!"
("Hello"," World!")
```

# Why would someone need this?

Well, there already are a few of programming languages in which one can access 
lists from their end, rather than from the beginning.

* Some of them use a negative index (e.g. Python, Perl) like `a[-2]`. It's 
not safe way though, because this approach may disguise an error of violating 
lists' range.
* The others use a special keyword or a symbol (e.g. Tcl, Matlab, Julia, D) 
like `a[end-1]`. It's a good option, although it requires support from the 
language syntax.

Other programming languages do not have such a feature. This package 
**data-index** tries to get it done for Haskell in a rather peculiar way.

1. The required functionality is added as a library, in contrast with patching 
the syntax
2. Not only an access from the end of a list is added, but also an access from 
any user-defined place, i.e. from the middle.

# How is this done?

The concept of a list index was invented anew. What is an access to a list by 
index anyway? Well, this is a function of two arguments that takes a list and 
a some abstract *place*, and returns a value from the *place*. This *place* 
must be defined in a list-independent way. These are examples of such 
*places*: the third element from the beginning, the second element from the 
end, the middle. That's how it can be done:

```haskell
newtype Index i = Index (forall a . [a] -> i)
idx   = Index (\t -> 2)                -- third element from the beginning
idx'  = Index (\t -> length t - 2)     -- second element from the end
idx'' = Index (\t -> length t `div` 2) -- the middle
```

One can notice that with such a definition, `Index` turns out to be something 
very similar to `Reader` monad, saving `forall`. Furthermore, "classical" 
indices from the beginning become `pure` indices, in virtue of
    
```haskell
pure i = Index (const i)
```

New `Index` is `Num` instance as well, allowing using not only `t !!  end`, 
but also `t !! (end-2)` or `t !! 1`, due to

```haskell
(-) = liftA2 (-)
fromInteger = pure . fromInteger
```

# Functions redefining

The only problem is to make functions to work with new `Index` instead of 
`Int`. At present this is getting done by manual redefining each needed 
function via do-notation, because I don't know how to do it better. Container 
polymorphism is also done, so the actual definition of `Index` is:

```haskell
newtype Index t i = Index {runIndex :: forall a . t a -> i}
```

Therefore, for each necessary container `t` corresponding functions working 
with indices as `Int` have to be redefined to functions working with `Index t 
Int`. Until now this is done for `Data.List` and `Data.Sequence`, but it's 
easy to implement `Index` for any linear container, e.g. `Vector`.
