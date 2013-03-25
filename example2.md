#Ssn



    prefix    : ssn
    JSON Type : integer


#Ide

here is a block comment
and a simple comment


    prefix    : ide
    JSON Type : string

###Log

need to add Migrate instances 
before we can bump the version 



#Flag



    prefix    : flg
    JSON Type : boolean


#Cert



    prefix    : crt
    JSON Type : base64 string


#Poo

A simple test example


    prefix    : poo
    JSON Type : object (record)
        x                    : integer              
        y                    : Foo                  


#Coord

A simple test example


    prefix    : c
    JSON Type : object (record)
        x                    : integer              // the x coordinate
                                                    // with a multi-line comment
        y                    : integer              // and here we have a multi-line
                                                    //                        block comment 
                                                    //                        and this


#Twiddle

A simple test example


    prefix    : twi
    JSON Type : object (record)
        x                    : [integer]            // a field
        y                    : ? [base64 string]    // another field


#CHOICE



    prefix    : chc
    JSON Type : object (union)
        a                    : integer              
        b                    : string               


#ENUM



    prefix    : enm
    JSON Type : string (e1|e2)


