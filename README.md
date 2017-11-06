typed-list-struct-refine [![Build Status](https://travis-ci.org/AlexKnauth/typed-list-struct-refine.png?branch=master)](https://travis-ci.org/AlexKnauth/typed-list-struct-refine)
===
Using lists as struct-like values that can cooperate with Typed Racket's `Refine` types.

```racket
(require typed/list-struct)
```

```racket
(list-struct name ([field : sptype] ...) #:type-name Name)
  sptype  = Integer
          | (List sptype ...)
          | x                 ; where x is a type for another list-struct
```

Defines:
 - `Name`
 - `name`
 - `name-field` ...

`Name` is a type for instances of the list-struct.

`name` is a constructor and a type-expander. The constructor constructs
  instances from the field values, just like a struct constructor.
  The type-expander takes symbolic-objects (see the [grammar for Refine](http://docs.racket-lang.org/ts-reference/Experimental_Features.html#(form._((lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt)._.Refine))))
  and expands to a type that refines the fields to be equal to the
  symbolic-objects.

`name-field` is an accessor and a type-expander. The accessor gets that
  field from the instance, just like a struct accessor. The
  type-expander takes a symbolic-path (see the [grammar for Refine](http://docs.racket-lang.org/ts-reference/Experimental_Features.html#(form._((lib._typed-racket%2Fbase-env%2Fbase-types-extra..rkt)._.Refine))))
  for an instance, and expands to a symbolic-path for the field within
  the instance.
  
To use `name` and `name-field` as type expanders, you must use either
`(require type-expander)` or `#lang type-expander` from jsmaniac's
[Type Expander](http://docs.racket-lang.org/type-expander/)
library.

