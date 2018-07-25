# UUID Library

This is a simple library to work with UUID values. The library primarily
uses the Leach-Salz variant of UUIDs, but can cope with essentially all
kinds of UUIDs.

## Types

- Type `uuid`

  Represents an UUID value. Basically, a 128 bit number (more or less)
  with added structure.

- Function `uuid` _object_ &rarr; _uuid_

  Coerces _object_ to a UUID value. The argument may be one of 
  
  - an instance of type `uuid`, in which case it is returned unchanged
  - a string designator (string, symbol), whose string content is the 
    textual representation of a UUID in the format accepted by `parse-uuid`.
  - a byte array (`(array (unsigned-byte) (16))`)
  - an 128 bit unsigned integer number (`(unsigned-byte 128)`)
  
  If _object_ is neither of the above, the function signals a fatal
  condition of type `type-error`.

- Function `uuidp` _object_ &rarr; _boolean_

  Tests, whether _object_ is a value of type `uuid`. This is equivalent
  with `(typep object 'uuid)`.

## Comparing and Hashing

- Function `uuid=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid/=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid<=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid<` _object1_ _object2_ &rarr; _boolean_
- Function `uuid>=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid>` _object1_ _object2_ &rarr; _boolean_

- Function `uuid-hash` _object_ &rarr; _fixnum_

  Computes a hash code for the UUID value _object_. The algorithm used is
  compatible with the type's notion of equality, i.e., for two UUID values
  _v1_ and _v2_, the following property always holds: `(or (uuid/= v1 v2) (eql (uuid-hash v1) (uuid-hash v2)))`

## Reading and Writing

- Function `parse-uuid` string `&key` _start_ _end_ &rarr; _object_
- Function `print-uuid` _object_ `&key` _stream_ _braces_ _downcase_ &rarr; _answer_

## Conversions

- Function `uuid-bytes` _object_ &rarr; _array_
- Function `uuid-string` _object_ `&key` _braces_ _downcase_ &rarr; _string_
- Function `uuid-number` _object_ &rarr; _value_

## Format Information

- Function `uuid-version` _object_ &rarr; _value_
- Function `uuid-variant` _object_ &rarr; _value_
- Function `uuid-node` _object_ &rarr; _variant_
- Function `uuid-clock-sequence` _object_ &rarr; _variant_
- Function `uuid-timestamp` _object_ &rarr; _variant_

## Special Constructors

- Function `random-uuid` `&key` _generator_ _random-state_ &rarr; _object_
- Function `uuid-for-name` _string_ `&key` _start_ _end_ _digest_ _namespace_ &rarr; _object_
