# UUID Library

This is a simple library to work with UUID values. The library primarily
uses the Leach-Salz variant of UUIDs, but can cope with essentially all
kinds of UUIDs.

- Type `uuid`
- Function `uuid` _object_ &rarr; _uuid_
- Function `uuidp` _object_ &rarr; _boolean_
- Function `uuid=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid/=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid<=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid<` _object1_ _object2_ &rarr; _boolean_
- Function `uuid>=` _object1_ _object2_ &rarr; _boolean_
- Function `uuid>` _object1_ _object2_ &rarr; _boolean_
- Function `uuid-hash` _object_ &rarr; _fixnum_
- Function `print-uuid` _object_ `&key` _stream_ _braces_ _downcase_ &rarr; _answer_
- Function `uuid-bytes` _object_ &rarr; _array_
- Function `uuid-string` _object_ `&key` _braces_ _downcase_ &rarr; _string_
- Function `uuid-number` _object_ &rarr; _value_
- Function `uuid-version` _object_ &rarr; _value_
- Function `uuid-variant` _object_ &rarr; _value_
- Function `uuid-node` _object_ &rarr; _variant_
- Function `uuid-clock-sequence` _object_ &rarr; _variant_
- Function `uuid-timestamp` _object_ &rarr; _variant_
- Function `parse-uuid` string `&key` _start_ _end_ &rarr; _object_
- Function `random-uuid` `&key` _generator_ _random-state_ &rarr; _object_
- Function `uuid-for-name` _string_ `&key` _start_ _end_ _digest_ _namespace_ &rarr; _object_
