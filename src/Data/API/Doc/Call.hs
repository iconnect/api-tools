{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.API.Doc.Call
    ( callHtml
    ) where

import           Data.API.Doc.Subst
import           Data.API.Doc.Types

import           Data.List
import           Data.Ord


-- | Generate a web page documenting a 'Call'
callHtml :: DocInfo -> Dict -> Call -> String
callHtml di dct0 call@Call{..} = concat
    [ container_open            dct
    , headersHtml       di call dct
    , paramsHtml        di call dct
    , bodyHtml          di call dct
    , viewsHtml         di call dct
    , samplesHtml       di call dct
    ]
  where
    dct = flip extDict dct0
            [ (,) "HTTP-METHOD"    $ call_http_method
            , (,) "PATH"           $ ('/' :) $ concat $ intersperse "/" call_path
            , (,) "CALL-DESCRIPTION" call_description
            , (,) "AUTH-REQUIRED"  $ if call_auth_required then "yes" else "no"
            ]

headersHtml :: DocInfo -> Call -> Dict -> String
headersHtml di Call{..} c_dct =
    case call_headers of
      [] -> ""
      _  -> concat
                [ headers_head
                , concatMap mk_header $ sortBy (comparing header_name) call_headers
                , headers_foot
                ]
  where
    mk_header hdr = header_content $ call_dict_header di c_dct hdr


paramsHtml :: DocInfo -> Call -> Dict -> String
paramsHtml di Call{..} c_dct =
    case call_params of
      [] -> no_params c_dct
      _  -> concat
                [ params_head c_dct
                , paramsRows di c_dct call_params
                , params_foot c_dct
                ]

paramsRows :: DocInfo -> Dict -> [Param] -> String
paramsRows di c_dct = concatMap mk_param . sortBy (comparing param_name)
  where
    mk_param param = parameter_row $ call_dict_param di c_dct param

bodyHtml :: DocInfo -> Call -> Dict -> String
bodyHtml di Call{..} c_dct =
    case call_body of
      Nothing        -> ""
      Just (typ,spl) ->
            body_sample $
                flip extDict c_dct
                    [ (,) "BODY-TYPE"   (renderAPIType di typ)
                    , (,) "BODY-SAMPLE" spl
                    ]

viewsHtml :: DocInfo -> Call -> Dict -> String
viewsHtml di Call{..} c_dct
  | null call_views = ""
  | otherwise = concat [ views_head
                       , concatMap (view_content . view_dict) call_views
                       , views_foot
                       , concatMap view_detailed call_views
                       ]
  where
    view_dict vw = flip extDict c_dct
                            [ (,) "VIEW-ID"   $ view_id vw
                            , (,) "VIEW-TYPE" $ renderAPIType di $ view_type vw
                            , (,) "VIEW-DOC"  $ view_doc vw
                            ]

    view_detailed vw
      | null (view_params vw) = ""
      | otherwise = concat [ view_detail_head v_dct
                           , paramsRows di v_dct $ view_params vw
                           , view_detail_foot
                           ]
      where v_dct = view_dict vw

samplesHtml :: DocInfo -> Call -> Dict -> String
samplesHtml di Call{..} c_dct = concat
    [ sample_heading    c_dct
    , concat $ map mk_sample call_samples
    ]
  where
    mk_sample spl = sample s_dct
      where
        s_dct = call_dict_sample di c_dct spl

call_dict_header :: DocInfo -> Dict -> Header -> Dict
call_dict_header di dct Header{..} = flip extDict dct
    [ (,) "HEADER-NAME"        header_name
    , (,) "HEADER-OR"        $ if header_required then "Required" else "Optional"
    , (,) "HEADER-EXAMPLE"     header_expl
    , (,) "HEADER-DESC"        header_desc
    , (,) "HEADER-TYPE"      $ renderAPIType di header_type
    ]

call_dict_param :: DocInfo -> Dict -> Param -> Dict
call_dict_param di dct Param{..} = flip extDict dct
    [ (,) "PARAMETER-NAME"        param_name
    , (,) "PARAMETER-OR"        $ if param_required then "Required" else "Optional"
    , (,) "PARAMETER-EXAMPLE"     param_expl
    , (,) "PARAMETER-DESC"        param_desc
    , (,) "PARAMETER-TYPE"      $ either id (renderAPIType di) param_type
    ]

call_dict_sample :: DocInfo -> Dict -> Sample -> Dict
call_dict_sample di dct Sample{..} = flip extDict dct
    [ (,) "HTTP-STATUS"         $ show sample_status
    , (,) "SAMPLE-TYPE"         $ renderBodyType di sample_type
    , (,) "SAMPLE-RESPONSE"     $ maybe "" id sample_response
    ]


container_open :: Dict -> String
container_open dct = prep dct
    [ "    <nav class='breadcrumbs'>"
    , "        <a href='<<BC-HOME-URL>>'><<BC-HOME-TEXT>></a> &raquo; <<HTTP-METHOD>> <<PATH>>"
    , "    </nav>"
    , "    <h2>"
    , "        <<HTTP-METHOD>> <<PATH>>"
    , "    </h2>"
    , "    <br>"
    , "    <div class='description'>"
    , "        <p><<CALL-DESCRIPTION>></p>"
    , "    </div>"
    , "    <table border='0' cellspacing='3' cellpadding='0' class='details-table'>"
    , "        <tr>"
    , "            <td width='180'><strong>Request Method</strong></td>"
    , "            <td><code><<HTTP-METHOD>></code>&nbsp;</td>"
    , "        </tr>"
    , "        <tr>"
    , "            <td width='180'><strong>Resource URI</strong></td>"
    , "            <td><code><<ENDPOINT>><<PATH>></code>&nbsp;</td>"
    , "        </tr>"
    , "        <tr>"
    , "            <td width='180'><strong>Authentication Required</strong></td>"
    , "            <td><code>"
    , "                <<AUTH-REQUIRED>>"
    , "                </code>&nbsp;"
    , "            </td>"
    , "        </tr>"
--  , "        <tr>"
--  , "            <td width='180'><strong>Request Body</strong></td>"
--  , "            <td><b>" ++ mkLink dct "POST-NODE-URL"  "POST-NODE" ++ "</b></td>"
--  , "        </tr>"
    , "    </table>"
    , "    <br>"
    , "    <hr/>"
    ]

headers_head :: String
headers_head = unlines
    [ "    <br>"
    , "    <h3>Headers</h3>"
    , "    <br>"
    , "    <table border='0' cellspacing='0' cellpadding='0' width='100%' id='headers'>"
    ]

header_content :: Dict -> String
header_content dct = prep dct
    [ "        <tr>"
    , "            <td><code><<HEADER-NAME>></code></td>"
    , "            <td><em><<HEADER-OR>></em></td>"
    , "            <td class='details'><p><<HEADER-TYPE>></p></td>"
    , "            <td class='details'><p><tt><<HEADER-EXAMPLE>></tt></p></td>"
    , "            <td class='details'><p><<HEADER-DESC>></p></td>"
    , "        </tr>"
    ]

headers_foot :: String
headers_foot = "</table><br>"


no_params :: Dict -> String
no_params dct = prep dct
    [ "    <br>"
    , "    <h3>Parameters</h3>"
    , "    <br>"
    , "    <em>There are no parameters for this resource.</em>"
    , "    <br>"
    ]

params_head :: Dict -> String
params_head dct = prep dct
    [ "    <br>"
    , "    <h3>Parameters</h3>"
    , "    <br>"
    , "    <table border='0' cellspacing='0' cellpadding='0' width='100%' id='params' class='params'>"
    ]



parameter_row :: Dict -> String
parameter_row dct = prep dct
    [ "        <tr>"
    , "            <td width='130'><code><<PARAMETER-NAME>></code></td>"
    , "            <td width='75'>"
    , "                <em>"
    , "                <<PARAMETER-OR>>"
    , "                </em>"
    , "            </td>"
    , "            <td class='details'>"
    , "                <p><<PARAMETER-TYPE>></p>"
    , "            </td>"
    , "            <td class='details'>"
    , "                <p><tt><<PARAMETER-EXAMPLE>></tt></p>"
    , "            </td>"
    , "            <td class='details'>"
    , "                <p><<PARAMETER-DESC>></p>"
    , "            </td>"
    , "        </tr>"
    ]


params_foot :: Dict -> String
params_foot dct = prep dct
    [ "    </table>"
    ]

body_sample :: Dict -> String
body_sample dct = prep dct
    [ "    <br>"
    , "    <h3>Sample Body</h3>"
    , "    <br>"
    , "    <div class='response-format'>"
    , "        <<BODY-TYPE>>"
    , "    </div>"
    , "    <pre><<BODY-SAMPLE>></pre>"
    ]


views_head :: String
views_head = unlines
    [ "    <br>"
    , "    <h3>Views</h3>"
    , "    <br>"
    , "    <table border='0' cellspacing='0' cellpadding='0' width='100%' id='views'>"
    ]

view_content :: Dict -> String
view_content dct = prep dct
    [ "        <tr>"
    , "            <td width='130'><code><<VIEW-ID>></code></td>"
    , "            <td class='details'><p><<VIEW-TYPE>></p></td>"
    , "            <td class='details'><p><<VIEW-DOC>></p></td>"
    , "        </tr>"
    ]

views_foot :: String
views_foot = "</table><br>"


view_detail_head :: Dict -> String
view_detail_head dct = prep dct
    [ "    <br>"
    , "    <h3>Parameters for <code><<VIEW-ID>></code> view :: <<VIEW-TYPE>></h3>"
    , "    <br>"
    , "    <table border='0' cellspacing='0' cellpadding='0' width='100%' class='params view-detail' id='view-<<VIEW-ID>>'>"
    ]

view_detail_foot :: String
view_detail_foot = "</table>"



sample_heading :: Dict -> String
sample_heading dct = prep dct
    [ "    <br>"
    , "    <h3>Sample Responses</h3>"
    , "    <br>"
    ]

sample :: Dict -> String
sample dct = prep dct
    [ "    <div class='response-format'>"
    , "        <<SAMPLE-TYPE>>"
    , "        <span>HTTP Status: <<HTTP-STATUS>></span>"
    , "    </div>"
    , "    <pre><<SAMPLE-RESPONSE>></pre>"
    ]
