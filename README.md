enclosed-exceptions-except
==========================

Run an enclosed exception raising computation and return and ExceptT result.

```enclosed-exception-except``` is a complement to Michael Snoyman' excelent
[enclosed-exceptions](https://hackage.haskell.org/package/enclosed-exceptions) package, 
allowing you to run an IO computation that may raise exceptions and convert it to an 'ExceptT e IO a' value, 
while still remaining responsive to external asynchronous exceptions.

This module is fully strict, in the sense that it forces full evaluation on its arguments.


