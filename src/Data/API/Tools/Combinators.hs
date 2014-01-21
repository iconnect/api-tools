{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.Combinators
    ( Tool
    , APITool
    , APINodeTool
    , emptyTool
    , appendTool
    , apiNodeTool
    , apiDataTypeTool
    , apiSpecTool
    ) where

import           Data.API.Types

import           Control.Applicative
import           Language.Haskell.TH


type Tool a      = a -> Q [Dec]
type APITool     = Tool API
type APINodeTool = Tool APINode

emptyTool :: Tool a
emptyTool _ = return []

appendTool :: Tool a -> Tool a -> Tool a
appendTool t1 t2 x = (++) <$> t1 x <*> t2 x

-- | Apply a tool that acts on nodes to an entire API
apiNodeTool :: Tool APINode -> Tool API
apiNodeTool tool api' = concat <$> mapM (tool $) [an | ThNode an <- api' ]

-- | Apply a tool that acts on datatype nodes (i.e. those that are not
-- synonyms) to an entire API
apiDataTypeTool :: Tool APINode -> Tool API
apiDataTypeTool tool api' = concat <$> mapM (tool $) [an | ThNode an <- api'
                                                         , hasDataType $ anSpec an ]
  where
    hasDataType (SpSynonym _) = False
    hasDataType _             = True

-- | Create a tool that acts on nodes from its action on individual
-- specs.
apiSpecTool :: (APINode -> Tool SpecNewtype)
            -> (APINode -> Tool SpecRecord )
            -> (APINode -> Tool SpecUnion  )
            -> (APINode -> Tool SpecEnum   )
            -> (APINode -> Tool APIType    )
            -> Tool APINode
apiSpecTool n r u e s an = case anSpec an of
              SpNewtype sn -> n an sn
              SpRecord  sr -> r an sr
              SpUnion   su -> u an su
              SpEnum    se -> e an se
              SpSynonym ss -> s an ss
