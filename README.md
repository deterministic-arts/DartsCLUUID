# UUID Library

This is a simple library to work with UUID values. The library primarily
uses the Leach-Salz variant of UUIDs, but can represent all kinds of UUIDs.

## Documentation

### Types

- **Type** `uuid`

  Represents an UUID value. Basically, a 128 bit number (more or less)
  with added structure.

- **Function** `uuidp` _object_ &rarr; _boolean_

  Tests, whether _object_ is a value of type `uuid`. This is equivalent
  with `(typep object 'uuid)`.

### Comparing and Hashing

Values of type UUID can be compared for equality as well as order. The
ordering on UUIDs is total, i.e., every two UUID instances _v1_ and _v2_
can be compared, and exactly one of

 - `uuid< v1 v2`
 - `uuid= v1 v2`
 - `uuid> v1 v2`
 
will answer true. The ordering carefully reflects the lexicographic
ordering of the string representations of UUIDs, i.e., it is always the
case that

 - `(uuidXX v1 v2)` if (and only if) `(stringXX (uuid-string v1) (uuid-string v2))`
 
where `XX` is one of `<`, `<=`, `=`, `>=`, `>`, `/=`. Note, that the actual
implementation of the predicates is more efficient than that.
  
The hash function can be used in Common Lisp implementations, which
support custom hash functions

For example, in [CCL](https://ccl.clozure.com/)

```
(make-hash-table :test 'uuid= :hash-function 'uuid-hash)
```

In [SBCL](http://www.sbcl.org/manual/#Hash-Table-Extensions) you can

```
(sb-ext:define-hash-table-test uuid= #'uuid-hash)
```

and then use this as

```
(make-hash-table :test 'uuid=)
```

(but 

```
(make-hash-table :test #'uuid= :hash-function #'uuid-hash)
```

will work there, too)

The ordering predicates can be used with data structures like [FSet](http://quickdocs.org/fset/)
and [WBTree](http://quickdocs.org/dartsclhashtree/):

```
(define-wbtree uuid-wbtree 
  (:key uuid)
  (:test uuid<)
  (:documentation "A weight-balanced binary tree, whose keys are
    UUID values."))
    
(defvar *mapping* (make-uuid-wbtree (list "F664863A-B3D9-4EF4-BB22-E1061F0010D6" 'stuff)))

*mapping* ;; => #<UUID-WBTREE #<UUID F664863A-B3D9-4EF4-BB22-E1061F0010D6> STUFF 1>
```

- **Function** `uuid=` _object1_ _object2_ &rarr; _boolean_
- **Function** `uuid/=` _object1_ _object2_ &rarr; _boolean_
- **Function** `uuid<=` _object1_ _object2_ &rarr; _boolean_
- **Function** `uuid<` _object1_ _object2_ &rarr; _boolean_
- **Function** `uuid>=` _object1_ _object2_ &rarr; _boolean_
- **Function** `uuid>` _object1_ _object2_ &rarr; _boolean_

  Each of these functions compares the argument values, both of which must
  be instances of type `uuid`, and returns true, if the values have the 
  ordering/equality relationship hinted at by the suffix (`=`, `<`, ...)

- **Function** `uuid-hash` _object_ &rarr; _fixnum_

  Computes a hash code for the UUID value _object_. The algorithm used is
  compatible with the type's notion of equality, i.e., for two UUID values
  _v1_ and _v2_, the following property always holds: `(or (uuid/= v1 v2) (eql (uuid-hash v1) (uuid-hash v2)))`

### Reading and Writing

- **Function** `parse-uuid` string `&key` _start_ _end_ &rarr; _object_

  Parses the string representation of a UUID, returning the result as instance
  of type `uuid`. The general format understood by this function is `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`
  where each `X` is a hexadecimal digit. Optionally, the value may be enclosed
  in curly braces, i.e., `{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}`; note, that
  if the opening brace is present, the closing must be, too. The number of `X`es 
  in the pattern is the maximum length; the function also accepts strings,
  where each group of `X`es is shorter. Note, that at least one digit is
  required per group. So, the following values result in the same UUID being
  produced by this function: 
  
   - `00000001-0002-0003-0004-000000000005`
   - `1-2-3-4-5`
  
  If the value can be parsed, the function returns the UUID value. If an
  error is detected, the function returns `nil`.
  
  This function will only inspect the portion of `string` in the region
  represented by _start_ (inclusive) and _end_ (exclusive). If omitted,
  _start_ defaults to 0 (the beginning of _string_) and _end_ to 
  `(length string)`.

- **Function** `print-uuid` _object_ `&key` _stream_ _braces_ _downcase_ &rarr; _answer_

  Writes a string representation of _object_ in the "usual" format into the
  given character stream _stream_ (which defaults to `*standard-output*`). The
  representation written has the general format `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`, 
  where each `X` is a hexadecimal digit. If _braces_ is true, the value is additionally 
  enclosed in curly braces `{`/`}`. The default is to not use braces. If _downcase_, 
  the function  uses lower case letters in the hex representation, otherwise it uses
  upper case letters (the default).
  
  This function returns its first argument _object_
  
- **Function** `uuid-string-p` _object_ `&key` _lenient_ _braces_ &rarr; _boolean_

  Tests, whether _object_ is a string, that looks like the supported representation
  of a UUID value. Note, that this function is slightly more strict than `parse-uuid`,
  in that
  
  - if _braces_ is true, then braces are required, if it is false (the default),
    then they are *not* accepted.
    
  - if _lenient_ is true, the "short" representation is accepted, otherwise (the default)
    each group of hexadecimal digits must have the "nominal" length (i.e., be of
    8, 4, 4, and finally 12 hex digits, respectively).

### Conversions

- **Function** `uuid` _object_ &rarr; _uuid_

  Coerces _object_ to a UUID value. The argument may be one of 
  
  - an instance of type `uuid`, in which case it is returned unchanged
  - a string designator (string, symbol), whose string content is the 
    textual representation of a UUID in the format accepted by `parse-uuid`.
  - a byte array (i.e., `(array (unsigned-byte 8) (16))`)
  - an 128 bit unsigned integer number (i.e., `(unsigned-byte 128)`)
  
  If _object_ is neither of the above, the function signals a fatal
  condition of type `type-error`.

- **Function** `uuid-bytes` _object_ &rarr; _array_

  Answers an `(array (unsigned-byte 8) (16))`, which holds the contents
  of the UUID _object_. The value _array_ can later be passed to function 
  `uuid` to reconstruct the UUID value. For all UUID values _object_, it 
  is always the case that `(uuid= object (uuid (uuid-bytes object)))`.

- **Function** `uuid-string` _object_ `&key` _braces_ _downcase_ &rarr; _string_

  Answers a string representation of _object_ in the "usual" format, i.e.,
  `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`, where each `X` is a hexadecimal
  digit. If _braces_ is true, the value is additionally enclosed in curly
  braces `{`/`}`. The default is to not use braces. If _downcase_, the function 
  uses lower case letters in the hex representation, otherwise it uses
  upper case letters (the default).
  
  For any UUID value _object_ and any combination of argument values for
  _braces_ and _downcase_, this function guarantees, that `(uuid= object (uuid (uuid-string object :braces braces :downcase downcase)))`

- **Function** `uuid-number` _object_ &rarr; _value_

  Answers an integer of type `(unsigned-byte 128)`, which holds the contents
  of the UUID _object_. The value _value_ can later be passed to function 
  `uuid` to reconstruct the UUID value. For all UUID values _object_, it 
  is always the case that `(uuid= object (uuid (uuid-number object)))`.

### Special Constructors

- **Function** `random-uuid` `&key` _generator_ _random-state_ &rarr; _object_

  Generates a fresh UUID of the "random" variant. If _generator_ is given it
  must be a function of a single argument, a positive integer _N_, which returns
  an `(array (unsigned-byte) (N))` of _N_ random bytes. If _random-state_ is 
  given, it must be a [random state](http://www.lispworks.com/documentation/lw70/CLHS/Body/26_glo_r.htm#random_state).
  If neither is given, the function uses `random` with the value of `*random-state*`
  to generate the bytes. Only one of _generator_ or _random-state_ can be
  used.

- **Function** `uuid-for-name` _string_ `&key` _start_ _end_ _digest_ _namespace_ &rarr; _object_

  FIXME

### Format Information

The following functions extract information about variant and version of a
UUID value. Certain extractor functions (like `uuid-clock-sequence`, `uuid-timestamp`, etc.)
are meaningful only for certain variants, if at all. These functions are
provided for completeness, but I have yet to find a reason for using any
of them...

- **Function** `uuid-version` _object_ &rarr; _value_
- **Function** `uuid-variant` _object_ &rarr; _value_
- **Function** `uuid-node` _object_ &rarr; _variant_
- **Function** `uuid-clock-sequence` _object_ &rarr; _variant_
- **Function** `uuid-timestamp` _object_ &rarr; _variant_

## Compatibility with System `UUID`

The UUID provided by this library is comparable to the one provided by
the [UUID](http://quickdocs.org/uuid/) library with the following 
differences

 - this library's UUID representation is more compact
 - this library provides a complete set of comparison operators for 
   UUIDs as well as a hash function
 - there is no support for the generation of "v1" style UUIDs in 
   this library as of now

You can convert between `uuid:uuid` and `darts.lib.uuid:uuid` values,
for example, by

```
(defvar *uuid-uuid* (uuid:make-v1-uuid))
(defvar *darts-uuid* (darts.lib.uuid:uuid (uuid:uuid-to-byte-array *uuid-uuid*)))
```

and

```
(defvar *darts-uuid-2* (darts.lib.uuid:random-uuid))
(defvar *uuid-uuid-2* (uuid:byte-array-to-uuid (darts.lib.uuid:uuid-bytes *darts-uuid-2*)))
```
