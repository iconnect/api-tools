#IsoS

simple String newtype defn

    JSON Type : string


#IsoB

simple Bool   newtype defn

    JSON Type : boolean


#IsoI

simple Int    newtype defn

    JSON Type : integer


#Foo

a test defn

    prefix    : bAr_
    JSON Type : object (record)
        Baz                  : boolean              // just a bool
        Qux                  : integer              // just an int


#Wibble

another test defn

    prefix    : dro
    JSON Type : object (union)
        wubble               : [Foo]                // list of Foo
        flubble              : string               // a string


#Enumer

enum test defn

    prefix    : enm
    JSON Type : string (wubble|flubble)


