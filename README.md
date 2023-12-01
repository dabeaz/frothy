# Frothy Example

Author : David Beazley (https://www.dabeaz.com)

This is the Frothy example from Brown's CSCI1730, Fall 2023. Contains
different implementations of a language reader should you decide to
make your own Racket language.

Here are the notable files:

- `frothy/frothy.rky` (Core semantics of the language, macros)
- `frothy/sexp/lang/reader.rkt` (Defining an s-exp based language)
- `frothy/line/lang/reader.rkt` (A simple line-based text parser)
- `frothy/tokens/lang/reader.rkt` (A hand written parser using tokens)
- `frothy/yacc/lang/reader.rkt` (A parser written using lex/yacc tools)

There are some example programs that use each of the different
readers.  You can find these in the various `frothy-example-*.rkt`
files.

Note:  If you want to be able to use a language line like
`#lang frothy`, you need to install via raco.

```
shell % raco pkg install frothy/
```
