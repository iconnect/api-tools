
#The api-tools API

Here we make the DSP specs available in JSON so that clients
working in non-Haskell frameworks can integrate API
specs into their own environments.
###APISpec

an API specification consists of a list of nodes,
each describing a named JSON thing which can be
referenced in other (object) nodes.


JSON Type : **[APINode]** [Haskell prefix is `ans`] 

version: 0


###APINode

Each node is named (with a name that conforms to Haskell's
Type identifier rules, starting with a capital letter) and some
comment text (like this comment). The JSON spec is all contained
in the {Spec}, the other fields being used behind the scenes
to manage the Haskell name space and migrations.


JSON Type : **record object** [Haskell prefix is `an`] 

Field   | Type         | Comment
------- | ------------ | -------
name    | string       | the name must start with a big letter  and conform to Haskell rules for type identifiers
comment | string       | natural language description of the node
prefix  | string       | (Haskell side only) this prefix used to structure the Haskell name space (which is uncomfortably flat where record field names are concerned); it has no effect on the JSON representation
spec    | Spec         | here we specify the JSON representation
convert | ? Conversion | (Haskell side only) sometimes we may choose to not use the default internal representation for the JSON but instead supply a couple of functions for injecting the default representation into the actual type we will use and the and project it back again; here we can check some properties (which must be explained in the comments) and reject a request with a 400 error; has no effect on the JSON representation
version | integer      | (Haskell side mostly) the version number for handling server migrations
log     | string       | a log explaining the changes that have been made through the migrations
version: 0


###Spec

here we specify the JSON representation:


JSON Type : **union object** [Haskell prefix is `sp`] 

Alternative | Type      | Comment
----------- | --------- | -------
_newtype_   | BasicType | here we have a basic string or number type
_record_    | [Field]   | an object representing a record
_union_     | [Field]   | an object representing a number of alternatives
_enum_      | [string]  | a string type which must contain a number of discrete values
_synonym_   | APIType   | is just an alias for another type
version: 0


###Conversion

Conversions are just used on the Haskell side to map the concrete JSON
representation into an internal type and back again


JSON Type : **record object** [Haskell prefix is `cv`] 

Field      | Type   | Comment
---------- | ------ | -------
injection  | string | the injection function for injecting representations into the internal representation; may return a failure indicating that some API precondition was not met
projection | string | the projection function converts the internal type back into the JSON representation for communication back to the client
version: 0


###Field

a field represent both a field in a record object and an alternative in
a union object (in which exactly one of the 'fields' is ever present in
the object)


JSON Type : **record object** [Haskell prefix is `fd`] 

Field   | Type    | Comment
------- | ------- | -------
name    | string  | the name of the method
type    | APIType | the JSON type of the field
comment | string  | a comment describing the field (like this one)
version: 0


###APIType

this is used to represent JSON types in fields (or synonyms) and is one
one of the following:


JSON Type : **union object** [Haskell prefix is `ty`] 

Alternative | Type      | Comment
----------- | --------- | -------
_list_      | APIType   | a JSON list of the given type
_maybe_     | APIType   | either the given type or the null value
_name_      | string    | a named type (node) from the API
_basic_     | BasicType | a basic JSON type
version: 0


###BasicType

finally we get down to the basic JSON types ('binary' is a string
in which the byte string has been encoded with base-64, safe for
embedding in a UTF-8-encoded JSON string


JSON Type : **string (string|binary|boolean|integer)** [Haskell prefix is `bt`] 

version: 0


