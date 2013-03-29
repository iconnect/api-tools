###IsoS

simple String newtype defn

JSON Type : **string** [Haskell prefix is ``]

version: 0


###IsoB

simple Bool   newtype defn

JSON Type : **boolean** [Haskell prefix is ``]

version: 0


###IsoI

simple Int    newtype defn

JSON Type : **integer** [Haskell prefix is ``]

version: 0


###Foo

a test defn

JSON Type : **record object** [Haskell prefix is `bAr_`]

Field | Type    | Comment
----- | ------- | -------
Baz   | boolean | just a bool
Qux   | integer | just an int
version: 0


###Wibble

another test defn

JSON Type : **union object** [Haskell prefix is `dro`]

Alternative | Type   | Comment
----------- | ------ | -------
_wubble_    | [Foo]  | list of Foo
_flubble_   | string | a string
version: 0


###Enumer

enum test defn

JSON Type : **string (wubble|flubble)** [Haskell prefix is `enm`]

version: 0


