{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.Combinators
    ( Tool
    , APITool
    , APINodeTool
    , runTool

      -- * Smart constructors and combinators
    , simpleTool
    , mkTool
    , contramapTool
    , readTool
    , subTools
    , apiNodeTool
    , apiDataTypeTool
    , apiSpecTool

      -- * Tool settings
    , ToolSettings
    , warnOnOmittedInstance
    , newtypeSmartConstructors
    , defaultToolSettings
    ) where

import           Data.API.Types

import           Control.Applicative
import           Data.Monoid
import           Language.Haskell.TH
import           Prelude


-- | Settings to control the behaviour of API tools.  This record may
-- be extended in the future, so you should construct a value by
-- overriding individual fields of 'defaultToolSettings'.
data ToolSettings = ToolSettings
    { warnOnOmittedInstance :: Bool
      -- ^ Generate a warning when an instance declaration is omitted
      -- because it already exists
    , newtypeSmartConstructors :: Bool
      -- ^ Rename the constructors of filtered newtypes and generate
      -- smart constructors that enforce the invariants
    }

-- | Default settings designed to be overridden.
defaultToolSettings :: ToolSettings
defaultToolSettings = ToolSettings
    { warnOnOmittedInstance = False
    , newtypeSmartConstructors = False
    }

-- | A @'Tool' a@ is something that can generate TH declarations from
-- a value of type @a@.  Tools can be combined using the 'Monoid'
-- instance.
newtype Tool a   = Tool
    { runTool :: ToolSettings -> a -> Q [Dec]
      -- ^ Execute a tool to generate some TH declarations.
    }

type APITool     = Tool API
type APINodeTool = Tool APINode

instance Monoid (Tool a) where
  mempty                    = Tool $ \ _ _ -> return []
  Tool t1 `mappend` Tool t2 = Tool $ \ ts x -> (++) <$> t1 ts x <*> t2 ts x

-- | Construct a tool that does not depend on any settings
simpleTool :: (a -> Q [Dec]) -> Tool a
simpleTool f = Tool $ const f

-- | Construct a tool that may depend on the settings
mkTool :: (ToolSettings -> a -> Q [Dec]) -> Tool a
mkTool = Tool

-- | 'Tool' is a contravariant functor
contramapTool :: (a -> b) -> Tool b -> Tool a
contramapTool f t = Tool $ \ ts a -> runTool t ts (f a)

-- | Make a tool that reads its argument to decide what to do
readTool :: (a -> Tool a) -> Tool a
readTool t = mkTool $ \ ts x -> runTool (t x) ts x

-- | Apply a tool that acts on elements of a list to the entire list
subTools :: Tool a -> Tool [a]
subTools t = Tool $ \ ts as -> concat <$> mapM (runTool t ts) as

-- | Apply a tool that acts on nodes to an entire API
apiNodeTool :: Tool APINode -> Tool API
apiNodeTool = contramapTool (\ api -> [an | ThNode an <- api ]) . subTools

-- | Apply a tool that acts on datatype nodes (i.e. those that are not
-- synonyms) to an entire API
apiDataTypeTool :: Tool APINode -> Tool API
apiDataTypeTool = contramapTool (\ api -> [an | ThNode an <- api, hasDataType $ anSpec an ]) . subTools
  where
    hasDataType (SpSynonym _) = False
    hasDataType _             = True

-- | Create a tool that acts on nodes from its action on individual
-- specs.
apiSpecTool :: Tool (APINode, SpecNewtype)
            -> Tool (APINode, SpecRecord )
            -> Tool (APINode, SpecUnion  )
            -> Tool (APINode, SpecEnum   )
            -> Tool (APINode, APIType    )
            -> Tool APINode
apiSpecTool n r u e s = Tool $ \ ts an -> case anSpec an of
              SpNewtype sn -> runTool n ts (an, sn)
              SpRecord  sr -> runTool r ts (an, sr)
              SpUnion   su -> runTool u ts (an, su)
              SpEnum    se -> runTool e ts (an, se)
              SpSynonym ss -> runTool s ts (an, ss)
