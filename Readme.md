# HLog - Haskell Logic Language

This repository contains an implementation of a simple logic language interpreter similar to prolog written in Haskell.

---

Programs are stated in terms of __facts__
```
sum(X, z, X).
# X plus Zero is X
```
and __rules__
```
sum(X, s(Y), Z) :-
       sum(s(X), Y, Z).
# X plus the successor of Y is the sum of the successor of X plus Y
```
which enable deriving a possible substitution of free
variables in this system.
E.g:
```
:- sum(s(z), s(z), X)
# What is the sum of 1 and 1
# -> { X -> s(s(z)) } I.e: 2
```

For another example see [example.hlog](example.hlog)

## Building

```
stack build
```

## Running

```
stack run example.hlog
```

## Note

This interpreter was created as a toy example to test how to implement a logic resolution engine in Haskell. There might still be issues with the implementation which is why I do not recommend using it as a reference.