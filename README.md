api-tools
=========

The `api-tools` library provides a compact DSL for describing an API.
It uses Template Haskell to generate the corresponding data types and
assorted tools for working with it, including code for converting
between JSON and the generated types and writing unit tests.  It
supports maintaining a log of changes to the API and migrating data
between different versions.

See `Data.API.Tutorial` for a brief overview of the library.
