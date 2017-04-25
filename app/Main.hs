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
import           Reflex.Dom.Time                  (delay)
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

data TestProduct = TestProduct Int T.Text Double Int T.Text Double deriving (Show,GHC.Generic)
instance Generic TestProduct
instance HasDatatypeInfo TestProduct


rebuildStyle = ("style" =: "background-color:#D98880")
updateStyle  = ("style" =: "background-color:#F9E79F")
restingStyle = ("style" =: "background-color:#7DCEA0")

testWidget::JSM ()
testWidget = mainWidget $ do
  el "h2" $ text "Various utilities to deal with distributing and sequencing Dynamics on sum and product types."
  el "h2" $ text "Each of these utilities is available for any type which is an instance of Generic and has fields which are likewise instances of Generic."

  el "br" blank
  el "span" $ text "Each input/display widget below will usually look like "
  elAttr "span" restingStyle $ text "this"
  el "span" $ text ". But when it's rebuilt from scratch it will temporarily change to "
  elAttr "span" rebuildStyle $ text "this color."
  el "span" $ text " And when the value is updated but the widget is not rebuilt, it will temporarily change to "
  elAttr "span" updateStyle $ text "this color"
  el "span" $ text ". This allows you to see exactly when widgets are updated and rebuilt."
  el "br" $ blank
  el "h3" $ text "Building widgets for sum-types with minimal rebuilding (Reflex.Dynamic.PerConstructor)"
  el "p" $ text "Given a type \"data TestEither = LeftInt Int | RightText Text\", we build an input widget for it and hook that up to a dynamic default value."
  el "p" $ text "First we give it a \"LeftInt\" input.  Note that you can set the widget to whatever you like and the output reflects that.  But if you change the input, the output matches the new input value.  And input updates that are on the same constructor do not need to rebuild the widget."
  el "p" $ text "First we give it an \"Int\" input via LeftInt."  
  dynMInt <- build (Compose . constDyn $ Just (2::Int))
  el "span" $ text "::Dynamic t (Maybe Int)"
  el "br" $ blank
  dynMTE1 <- buildSum (LeftInt <$> dynMInt)
  el "span" $ text " (widget for TestEither)"
  el "br" blank
  dynMaybeText dynMTE1
  el "span" $ text " (current output of widget)" 
  el "br" blank

  el "p" $ text "Now we give it a \"Text\" input via RightText."  
  dynMText <- build (Compose $ constDyn (Just $ ("ABC"::T.Text)))
  el "span" $ text "::Dynamic t (Maybe Text)"
  el "br" blank
  dynMTE2 <- buildSum (RightText <$> dynMText)
  el "span" $ text " (widget for TestEither)"
  el "br" blank
  dynMaybeText dynMTE2
  el "span" $ text " (current output of widget)" 
  el "br" blank
  el "br" blank
  
  el "span" $ text "We demonstrate with a larger sum: \"data TestSum = A Int | B T.Text | C Double | D T.Text\""
  el "p" $ text "We give it a Double input via C"
  dynMDouble <- build (Compose . constDyn $ Nothing)
  el "span" $ text "::Dynamic t (Maybe Double)"  
  el "br" blank
  dynMTS <- buildSum (C <$> dynMDouble)
  el "span" $ text " (widget for TestSum)"
  el "br" blank
  dynMaybeText dynMTS
  el "span" $ text " (current output of widget)"
  el "br" blank
  el "br" blank
  
  el "h3" $ text "Widgets for products of types with Eq instances => minimal updating (Reflex.Dynamic.EqProduct)"
  el "p" $ text "Given a product type, e.g., \"data TestProduct = TestProduct Int T.Text Double Int T.Text Double\" where each field is an instance of Eq, we don't need to update all the fields when only one field of the input changes. But if any field is unparseable, we have to update them all because the input switches to \"Nothing\""
  el "span" $ text "TestProduct: "
  el "br" blank
  dynMTPH <- buildUnsafeDynMBuildableEqProduct (Compose . constDyn . Just $ TestProduct 12 "Hello" 3.14 13 "Goodbye" 3.0)
  el "span" $ text "Dynamic t (Maybe TestProduct)"
  el "br" blank
  _ <- buildUnsafeDynMBuildableEqProduct dynMTPH
  el "span" $ text " (widget for TestProd)"
  el "br" blank
  dynMaybeText dynMTPH
  el "span" $ text " (current output of widget)"
  el "br" blank
  el "br" blank


  el "h3" $ text "Collecting Dynamics out of products (Reflex.Dynamic.CollectDyn)"
  el "p" $ text "Given a type, A,  with Dynamic fields and a corresponding type, B, with static fields, we can \"collect\" the dynamic fields of type A and produce a Dynamic B.  For example (Dynamic t (Maybe Int), Dynamic t (Maybe Double)) -> Dynamic t (Maybe Int, Maybe Double)"
  el "br" blank
  dynMInt2 <- build (Compose . constDyn $ Nothing)
  el "span" $ text "::Dynamic t (Maybe Int)"
  el "br" blank
  dynMDouble2 <- build (Compose . constDyn $ Nothing)
  el "span" $ text "::Dynamic t (Maybe Double)"
  el "br" blank
  dynText $ T.pack . show <$> testCollectDyn (dynMInt2, dynMDouble2)
  el "span" $ text " (current dynamic value of collectDynGeneric applied to the tuple of dynamics)"
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

-- this widget flashes "rebuildStyle" on postBuild, updateStyle when the inpt update and rests at restingStyle
fieldWidget'::(WidgetConstraints t m a)=>(T.Text -> Maybe a) -> (a -> T.Text)->DynMaybe t a -> m (DynMaybe t a)
fieldWidget' parse print dma = do
  postBuild <- getPostBuild
  let updatedInputEv = () <$ updated (getCompose dma)
  updatedDelayedEv <- delay 1.0  $ leftmost [updatedInputEv, postBuild] 
  attrs <- foldDyn const M.empty $ leftmost [rebuildStyle  <$ postBuild
                                            , updateStyle  <$ updatedInputEv
                                            , restingStyle <$ updatedDelayedEv] 
  inputEv <- fmapMaybe id <$> traceDynAsEv (const "fieldWidget'-") (getCompose dma) -- Event t a
  let inputEvT = print <$> inputEv
      config = TextInputConfig "text" "" inputEvT attrs
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








