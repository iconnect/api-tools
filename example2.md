###Ssn



JSON Type : **integer** [Haskell prefix is `ssn`]

version: 0


###Ide

here is a block comment
and a simple comment


JSON Type : **string** [Haskell prefix is `ide`]

version: 0

###Log

need to add Migrate instances 
before we can bump the version 



###Flag



JSON Type : **boolean** [Haskell prefix is `flg`]

version: 0


###Cert



JSON Type : **base64 string** [Haskell prefix is `crt`]

version: 0


###Poo

A simple test example


JSON Type : **record object** [Haskell prefix is `poo`]

Field | Type
----- | -------
x     | integer
y     | Foo
version: 0


###Coord

A simple test example


JSON Type : **record object** [Haskell prefix is `c`]

Field | Type    | Comment
----- | ------- | -------
x     | integer | the x coordinate with a multi-line comment
y     | integer | and here we have a multi-line                        block comment                         and this
version: 0


###Twiddle

A simple test example


JSON Type : **record object** [Haskell prefix is `twi`]

Field | Type              | Comment
----- | ----------------- | -------
x     | [integer]         | a field
y     | ? [base64 string] | another field
version: 0


###CHOICE



JSON Type : **union object** [Haskell prefix is `chc`]

Alternative | Type
----------- | -------
_a_         | integer
_b_         | string
version: 0


###ENUM



JSON Type : **string (e1|e2)** [Haskell prefix is `enm`]

version: 0


