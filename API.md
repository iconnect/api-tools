#APINode



    prefix    : an
    JSON Type : object (record)
        comment              : string               
        convert              : ? Conversion         
        log                  : string               
        name                 : string               
        prefix               : string               
        spec                 : Spec                 
        version              : integer              


#Spec



    prefix    : sp
    JSON Type : object (union)
        enum                 : [string]             
        newtype              : BasicType            
        record               : [Field]              
        synonym              : APIType              
        union                : [Field]              


#Conversion



    prefix    : cv
    JSON Type : object (record)
        injection            : string               
        projection           : string               


#Field



    prefix    : fd
    JSON Type : object (record)
        comment              : string               
        name                 : string               
        type                 : APIType              


#APIType



    prefix    : ty
    JSON Type : object (union)
        basic                : BasicType            
        list                 : APIType              
        maybe                : APIType              
        name                 : string               


#BasicType



    prefix    : bt
    JSON Type : string (binary|boolean|integer|string|union)


