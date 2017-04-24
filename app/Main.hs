{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
import           GHCJS.DOM.Types                  (JSM)
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom                       hiding (mainWidget, run)
import           Reflex.Dom.Core                  (mainWidget)
import           Reflex.Dom.Old                   (MonadWidget)

import           Control.Monad.Fix                (MonadFix)

import           Control.Monad                    (join)
import           Data.Functor.Compose             (Compose (..), getCompose)
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes, isNothing)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Data.Traversable                 (sequenceA)

import           System.Process                   (spawnProcess)
import           Text.Read                        (readMaybe)

import           Reflex.Dynamic.PerConstructor (whichFired,ConWidget(..),DynMaybe,Generic,HasDatatypeInfo,DynMBuildable(..),AllDynMBuildable,dynMBuildableToConWidgets)
import           Reflex.Dynamic.EqProduct (buildUnsafeDynMBuildableEqProduct)
import           Reflex.Dynamic.CollectDyn (collectDynGeneric)


import qualified GHC.Generics                     as GHC

-- NB: This is just for warp.
main::IO ()
main = do
  let port :: Int = 3702
  pHandle <- spawnProcess "open" ["http://localhost:" ++ show port]
  run port testWidget

data TestEither = LeftInt Int | RightText T.Text deriving (Show,GHC.Generic)

instance Generic TestEither
instance HasDatatypeInfo TestEither


data TestSum = A Int | B T.Text | C Double | D T.Text deriving (Show, GHC.Generic)
instance Generic TestSum
instance HasDatatypeInfo TestSum

data TestProd = TestProd Int Double deriving (Show,GHC.Generic)
instance Generic TestProd
instance HasDatatypeInfo TestProd

data TestProdHold = TestProdHold Int T.Text Double Int T.Text Double deriving (Show,GHC.Generic)
instance Generic TestProdHold
instance HasDatatypeInfo TestProdHold

testWidget::JSM ()
testWidget = mainWidget $ do
  el "span" $ text "Int: "
  dynMInt <- build (Compose . constDyn $ Just (2::Int))
  el "br" $ blank
  el "span" $ text "Either Int Text: "
  dynMTE1 <- buildSum (LeftInt <$> dynMInt)
  el "br" blank
  dynMaybeText dynMTE1
  el "br" blank
  el "span" $ text "Text: "
  dynMText <- build (Compose $ constDyn (Just $ ("ABC"::T.Text)))
  el "br" blank
  el "span" $ text "Either Int Text: "
  dynMTE2 <- buildSum (RightText <$> dynMText)
  el "br" blank
  dynMaybeText dynMTE2
  el "br" blank
  el "span" $ text "Double: "
  dynMDouble <- build (Compose . constDyn $ Nothing)
  el "br" blank

  el "span" $ text "TestSum: "
  dynMTS <- buildSum (C <$> dynMDouble)
  el "br" blank
  dynMaybeText dynMTS
  el "br" blank

  el "span" $ text "TestProd: "
  dynMTP <- buildSum (Compose . constDyn . Just $ TestProd 2 2.0)
  el "br" blank
  dynMaybeText dynMTP  
  el "br" blank
  el "br" blank

  el "span" $ text "TestProdHold: "
  el "br" blank
  dynMTPH <- buildUnsafeDynMBuildableEqProduct (Compose . constDyn . Just $ TestProdHold 12 "Hello" 3.14 13 "Goodbye" 3.0)
  el "br" blank
  _ <- buildUnsafeDynMBuildableEqProduct dynMTPH
  el "br" blank
  dynMaybeText dynMTPH
  el "br" blank
  el "br" blank

  el "span" $ text "CollectDynGeneric: "
  el "br" blank
  el "span" $ text "Int: "
  dynMInt2 <- build (Compose . constDyn $ Nothing)
  el "br" blank
  el "span" $ text "Double: "
  dynMDouble2 <- build (Compose . constDyn $ Nothing)
  el "br" blank
  dynText $ T.pack . show <$> testCollectDyn (dynMInt2, dynMDouble2)

  el "br" blank  
  return ()

dynMaybeText::(ReflexConstraints t m, Show a)=>DynMaybe t a->m ()
dynMaybeText = dynText . fmap (T.pack . show) . getCompose

traceDynAsEv::PostBuild t m=>(a->String)->Dynamic t a->m (Event t a)
traceDynAsEv f dyn = do
  postbuild <- getPostBuild
  let f' prefix x = prefix ++ f x
      upEv = traceEventWith (f' "update-") $ updated dyn
      pbEv = traceEventWith (f' "postbuild-") $ tag (current dyn) postbuild
  return $ leftmost [upEv, pbEv] 


type ReflexConstraints t m = (MonadWidget t m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
type WidgetConstraints t m a = (ReflexConstraints t m, Show a)


-- These don't "fire" on Nothing/Nothing updates.  But they do fire on Nothing/Just, Just/Nothing and Just/Just
uniqDynJust::Reflex t=>DynMaybe t a -> DynMaybe t a
uniqDynJust = Compose . uniqDynBy (\a b->isNothing a && isNothing b) . getCompose


testCollectDyn::Reflex t=>(DynMaybe t Int,DynMaybe t Double)->Dynamic t (Maybe Int, Maybe Double)
testCollectDyn (dma,dmb) = collectDynGeneric (getCompose dma, getCompose dmb) 


fieldWidget'::(WidgetConstraints t m a)=>(T.Text -> Maybe a) -> (a -> T.Text)->DynMaybe t a -> m (DynMaybe t a)
fieldWidget' parse print dma = do
  inputEv <- fmapMaybe id <$> traceDynAsEv (const "fieldWidget'-") (getCompose dma) -- Event t a
  let inputEvT = print <$> inputEv
      config = TextInputConfig "text" "" inputEvT (constDyn M.empty)
  aDyn <- _textInput_value <$> textInput config
  return . Compose $ parse <$> aDyn

fieldWidget::(WidgetConstraints t m a,Read a)=>DynMaybe t a -> m (DynMaybe t a)
fieldWidget = fieldWidget' (readMaybe . T.unpack) (T.pack . show)

sumChooserWH::WidgetConstraints t m a=>[ConWidget t m a]->m (DynMaybe t a)
sumChooserWH cws = mdo
  let indexedCN = zip [0..] (T.pack . conName <$> cws)
      inputIndexEv = whichFired (switchedTo <$> cws)
      ddConfig = DropdownConfig inputIndexEv (constDyn mempty)
  chosenIndexEv <- _dropdown_change <$> dropdown 0 (constDyn $ M.fromList indexedCN) ddConfig -- put dropdown in DOM
  let newIndexEv = leftmost [inputIndexEv
                            ,chosenIndexEv]
  curIndex <- holdDyn 0 newIndexEv
  let switchWidgetEv = updated . uniqDyn $ curIndex
      newWidgetEv = (\n -> (widget <$> cws) !! n) <$> switchWidgetEv -- Event t (m (DynMaybe t a))
--  dynText $ T.pack .show <$> curIndex
  Compose . join . fmap getCompose <$> widgetHold (head $ widget <$> cws) newWidgetEv


class WidgetConstraints t m a => TestBuilder t m a where
  build::DynMaybe t a -> m (DynMaybe t a)

instance TestBuilder t m a=>DynMBuildable t m a where
  dynMBuild = build

  
buildSum::forall a t m.(Functor m, Generic a, HasDatatypeInfo a
                       , WidgetConstraints t m a
                       , AllDynMBuildable t m a)
  =>DynMaybe t a->m (DynMaybe t a)
buildSum = sumChooserWH . dynMBuildableToConWidgets 


instance WidgetConstraints t m Int => TestBuilder t m Int where
  build = fieldWidget

instance WidgetConstraints t m Double => TestBuilder t m Double where
  build = fieldWidget

instance WidgetConstraints t m T.Text => TestBuilder t m T.Text where
  build = fieldWidget' Just id


instance TestBuilder t m a => TestBuilder t m (Maybe a) where
  build = buildSum

instance (TestBuilder t m a, TestBuilder t m b) => TestBuilder t m (Either a b) where
  build = buildSum








