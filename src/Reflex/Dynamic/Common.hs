{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
module Reflex.Dynamic.Common
  (
    DynMaybe
  , DynMBuildable(..)
  , AllDynMBuildable
  , functionPOPFromClass
  ) where

import           Data.Functor.Compose        (Compose)
import           Reflex                      (Dynamic)
import           Generics.SOP                (All2, Code, POP, SListI2, type (-.->)(Fn), Proxy (..), hcpure)
import           Generics.SOP.Dict           (Dict, withDict)
import           Generics.SOP.Constraint     (Constraint)

import           Generics.SOP.Distribute     (functionPOPFromClass)

type DynMaybe t = Compose (Dynamic t) Maybe

class DynMBuildable t m a where
  dynMBuild::DynMaybe t a -> m (DynMaybe t a)

-- This constraint means that for each type 'b' in a field of a constructor of 'a', b must satisfy 'DynMBuildable t m b'
type AllDynMBuildable t m a = (All2 (DynMBuildable t m) (Code a))

--functionPOPFromClass::forall c f g xss.SListI2 xss=>Dict (All2 c) xss->(forall a.c a=>f a -> g a)->POP (f -.-> g) xss
--functionPOPFromClass d fn = withDict d $ hcpure (Proxy :: Proxy c) $ Fn fn

{-
functionPOPFromClass'::forall c f g xss.(All2 c xss, SListI2 xss)=>(forall a.c a=>f a -> g a)->POP (f -.-> g) xss
functionPOPFromClass' fn =
  let dict :: Dict (All2 c) xss
      dict = all_POP hdicts
  in  withDict dict $ hcpure (Proxy :: Proxy c) $ Fn fn
-}
