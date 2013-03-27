#APINode



    prefix    : an
    JSON Type : object (record)
        name                 : string               
        comment              : string               
        prefix               : string               
        spec                 : Spec                 
        convert              : ? Conversion         
        version              : integer              
        log                  : string               


#Spec



    prefix    : sp
    JSON Type : object (union)
        newtype              : BasicType            
        record               : [Field]              
        union                : [Field]              
        enum                 : [string]             
        synonym              : APIType              


#Conversion



    prefix    : cv
    JSON Type : object (record)
        injection            : string               
        projection           : string               


#Field



    prefix    : fd
    JSON Type : object (record)
        name                 : string               
        type                 : APIType              
        comment              : string               


#APIType



    prefix    : ty
    JSON Type : object (union)
        list                 : APIType              
        maybe                : APIType              
        name                 : string               
        basic                : BasicType            


#BasicType



    prefix    : bt
    JSON Type : string (union|string|binary|boolean|integer)


