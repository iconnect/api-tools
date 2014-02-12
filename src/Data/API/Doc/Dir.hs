{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.API.Doc.Dir
    ( dirHtml
    ) where

import           Data.API.Doc.Types
import           Data.API.Doc.Subst
import           Data.List
import           Data.Ord
import           Data.Char


-- | Generate a web page documenting all the 'Call's in a web
-- application
dirHtml :: DocInfo -> Dict -> [Call] -> String
dirHtml di dct cls = concat
    [ container_open                 dct
    , concat $ map (resourceHtml di  dct) $ aggregate cls
    ]

aggregate :: [Call] -> [(String,[Call])]
aggregate = map f . groupBy eq . sortBy (comparing fst) . map x_r
  where
    x_r cl@Call{..} =
        case call_path of
          []     -> ("/" ,cl)
          rsrc:_ -> (rsrc,cl)

    eq (x,_) (y,_)   = x==y

    f []             = error "Data.API.Doc.Dir.aggregate"
    f ((rsrc,cl):ps) = (rsrc,cl:map snd ps)


resourceHtml :: DocInfo -> Dict -> (String,[Call]) -> String
resourceHtml di dct (rsc,cls) = concat
    [ resource_heading      r_dct
    , ul_open               r_dct
    , concat [ callHtml di  r_dct cl | cl<-cls ]
    , ul_close              r_dct
    ]
  where
    r_dct = resource_dict dct rsc

resource_dict :: Dict -> String -> Dict
resource_dict dct rsc = flip extDict dct
    [ (,) "RESOURCE-HEADING"    rsc
    ]

callHtml :: DocInfo -> Dict -> Call -> String
callHtml di dct cl = call $ call_dict di dct cl

call_dict :: DocInfo -> Dict -> Call -> Dict
call_dict di dct Call{..} = flip extDict dct
    -- calls
    [ (,) "CALL-URL"          $ doc_info_call_url di call_http_method call_path
    , (,) "METHOD-CLASS"      $ map toLower mth_s
    , (,) "METHOD"            $ mth_s
    , (,) "PATH"              $ '/' : concat (intersperse "/" call_path)
    , (,) "BODY-TYPE"         $ maybe "&mdash;" (renderAPIType di . fst) call_body
    ]
  where
    mth_s = call_http_method
--    cvt   = T.unpack . _APINodeName

container_open :: Dict -> String
container_open dct = prep dct
    [ "<h2>"
    , "   <<TITLE>>"
    , "   <span class='tagline'><<TAGLINE>></span>"
    , "</h2>"
    , "<strong>Endpoint: </strong><<ENDPOINT>>"
    , "<br>"
    , "<div id='toc'>"
    , "   <div id='app-description'>"
    , "      <p><<SUMMARY>></p>"
    , "   </div>"
    , "   <hr/>"
    , "   <br>"
    , "   <h3 class='section-title'>"
    , "      API Resources for This Application"
    , "   </h3>"
    ]

resource_heading :: Dict -> String
resource_heading dct = prep dct
    [ "   <h5 class='tag'><<RESOURCE-HEADING>></h5>"
    ]

ul_open :: Dict -> String
ul_open dct = prep dct
    [ "   <ul class='list resource-list'>"
    ]

call :: Dict -> String
call dct = prep dct
    [ "      <li >"
    , "         <a class='reflink' href='<<CALL-URL>>'>"
    , "         <span class='<<METHOD-CLASS>>'><<METHOD>></span>"
    , "         <<PATH>>"
    , "         </a>"
    , "         <div class='post-data' ><<BODY-TYPE>></div>"
    , "      </li>"
    ]

ul_close :: Dict -> String
ul_close dct = prep dct
    [ "   </ul>"
    ]
