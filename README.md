# macroparser

This is a library for "parsing" clojure data structures, with the
intention of making it easier to describe and extract the components
of complex macros (though it of course need not be used for macro
definitions). It uses the
[parsatron](https://github.com/youngnh/parsatron) parsing library.

## Usage

Several combinators useful for parsing generally, and specifically for
parsing data structures, can be found in macroparser.parsers. Other
namespaces contain pre-defined parsers for binding forms, pairs, and
vectors and for parsing `defn`, `fn`, and `fn*` forms. These can be
used as-as (in the case of binding vectors) or consulted as models for
defining derived function definition forms. The macroparser.monads
namespace contains a macro for an alternative monad syntax, though it
does not, presently, interoperate with algo.monads. (Such
interoperation is, however, trivially accomplished.)

Note that error messages at the moment are both deceptive (since
parsatron currently has no facility for anything other than row &
column-based character-oriented error messages), extremely verbose,
and unhelpful. Until some kind of error-message-overriding combinator
is developed for parsatron, it will be advisable to catch exceptions
and provide your own, more informative, error messages to users of any
macros (or anything else) you might parse with these combinators.

## License

Copyright (C) 2012 Ben Wolfson

Distributed under the Eclipse Public License, the same as Clojure.
