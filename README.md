# ocamlJson

A toy JSON parser for Ocaml using dune, ocamlyacc and ocamllex.

### Usage

Json.parse takes a string and returns a JSON value. JSON values are a variant of Number, Bool, String, Null, Array, or Object, covering all possible JSON values.

_this is a toy for education, and probably shouldn't be used in production._

### Tests

`$ dune runtest`
