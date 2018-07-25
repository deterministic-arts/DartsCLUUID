# UUID Library

This is a simple library to work with UUID values. The library primarily
uses the Leach-Salz variant of UUIDs, but can cope with essentially all
kinds of UUIDs.

## Types

- **Type** `uuid`

  Represents an UUID value. Basically, a 128 bit number (more or less)
  with added structure.

- **Function** `uuidp` _object_ &rarr; _boolean_

  Tests, whether _object_ is a value of type `uuid`. This is equivalent
  with `(typep object 'uuid)`.

## Comparing and Hashing

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

## Reading and Writing

- **Function** `parse-uuid` string `&key` _start_ _end_ &rarr; _object_

  Parses the string representation of a UUID, returning the result as instance
  of type `uuid`. The general format understood by this function is `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`
  where each `X` is a hexadecimal digit. Optionally, the value may be enclosed
  in curly braces, i.e., `{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}`; note, that
  if the opening brace is present, the closing must be, too.
  
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

## Conversions

- **Function** `uuid` _object_ &rarr; _uuid_

  Coerces _object_ to a UUID value. The argument may be one of 
  
  - an instance of type `uuid`, in which case it is returned unchanged
  - a string designator (string, symbol), whose string content is the 
    textual representation of a UUID in the format accepted by `parse-uuid`.
  - a byte array (`(array (unsigned-byte) (16))`)
  - an 128 bit unsigned integer number (`(unsigned-byte 128)`)
  
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

## Format Information

- **Function** `uuid-version` _object_ &rarr; _value_
- **Function** `uuid-variant` _object_ &rarr; _value_
- **Function** `uuid-node` _object_ &rarr; _variant_
- **Function** `uuid-clock-sequence` _object_ &rarr; _variant_
- **Function** `uuid-timestamp` _object_ &rarr; _variant_

## Special Constructors

- **Function** `random-uuid` `&key` _generator_ _random-state_ &rarr; _object_
- **Function** `uuid-for-name` _string_ `&key` _start_ _end_ _digest_ _namespace_ &rarr; _object_
