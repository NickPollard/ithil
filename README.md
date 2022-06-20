# Ithil

_A statically typed, referentially transparent functional programming language that compiles to LUA bytecode_

## Example Syntax

```
  factorial : Int -> Int
  factorial n = if n < 2 then 1 else n * factorial (n - 1)

  data Maybe a = Just a
               | Nothing

  mapMaybe : (a -> b) -> Maybe a -> Maybe b
  mapMaybe f (Just a) = f a
  mapMaybe _ Nothing  = Nothing

  main : IO ()
  main = do
    println "Hello world"
    lua_call "print" "Hello world"
```

## Motivation

Ithil is intended as am embeddable typed functional programming language. It is primarily designed for use by myself for game development.

Games are commonly built with performance in mind, and so usually in low level systems languages like C++ or Rust. However, many parts of a game do not have such significant performance requirements, and benefit from a higher level approach. It is common for such parts to be written in a _scripting_ language, of which Lua is the most common - primarily for its ease of embedding, whilst also retaining reasonable performance.

Lua (and other similar languages) have two particular disadvantages:
1. They are dynamically typed, and so lack the fast feedback and reliability of statically typed languages.
2. Partly related to the above, they do not have the levels of abstraction provided by modern functional languages such as Haskell. Writing generic libraries is tough, and many powerful patterns are frustratingly verbose.

Ithil aims to solve these needs by providing a language in which performance-uncritical parts of games can be written, using the powerful, terse abstractions, safely typechecked, and compiled to Lua bytecode which can be efficiently executed by an easily embedded Lua VM.

In short: it lets you write large pieces of games in (pseudo) Haskell.

## Features

The features intended for Ithil include:

* Static types, including
  * Algebraic data types
  * Parametric polymorphism
  * Higher-kinded types
* Pattern Matching
* Typeclasses
* Easy inter-operation with Lua

## Etymology

Following in the footsteps of many LUA derivative languages, Ithil is named after the moon. In Tolkien's legendarium, _Ithil_ is the Sindarin word for the moon.
