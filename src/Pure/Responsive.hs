{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, PatternSynonyms, ViewPatterns, CPP, DuplicateRecordFields, RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Pure.Responsive where

import Pure hiding (MinWidth,MaxWidth,minWidth,maxWidth)
import Pure.Data.Cond
import Pure.Data.Prop

import Control.Arrow ((&&&))
import Control.Monad (join,unless,void)
import Data.IORef
import GHC.Generics as G

import Data.Function ((&))

data Responsive = Responsive_
    { as          :: Features -> [View] -> View
    , features    :: Features
    , children    :: [View]
    , fireOnMount :: Bool
    , maxWidth    :: Int
    , minWidth    :: Int
    , onUpdate    :: IO ()
    } deriving (Generic)

instance Default Responsive where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Responsive :: Responsive -> Responsive
pattern Responsive r = r

pattern OnlyMobile :: Responsive -> Responsive
pattern OnlyMobile r = (MinWidth 320 (MaxWidth 767 r))

pattern OnlyTablet :: Responsive -> Responsive
pattern OnlyTablet r = (MinWidth 768 (MaxWidth 991 r))

pattern OnlyComputer :: Responsive -> Responsive
pattern OnlyComputer r = (MinWidth 992 r)

pattern OnlyLargeScreen :: Responsive -> Responsive
pattern OnlyLargeScreen r = (MinWidth 1200 (MaxWidth 1919 r))

pattern OnlyWidescreen :: Responsive -> Responsive
pattern OnlyWidescreen r = (MinWidth 1920 r)

data ResponsiveState = RS
    { width   :: Int
    , handler :: IORef (IO ())
    , ticking :: IORef Bool
    }

instance Pure Responsive where
    view =
        LibraryComponentIO $ \self ->
            let
                handleResize = do
                    RS {..} <- get self
                    tick <- readIORef ticking
                    unless tick $ do
                        writeIORef ticking True
                        void $ addAnimation handleUpdate

                handleUpdate = do
                    Responsive_ {..} <- ask self
                    RS {..} <- get self
                    writeIORef ticking False
                    w <- innerWidth
                    modify self $ \_ RS {..} -> RS { width = w, .. }
                    onUpdate

            in def
                { construct = RS <$> innerWidth <*> newIORef def <*> newIORef def

                , mounted = do
                    Responsive_ {..} <- ask self
                    RS {..} <- get self
                    Win w <- getWindow
                    h <- onRaw (Node w) "resize" def (\_ _ -> handleResize)
                    writeIORef handler h
                    fireOnMount # handleUpdate

                , unmounted = do
                    RS {..} <- get self
                    join $ readIORef handler
                    writeIORef handler def

                , render = \Responsive_ {..} RS {..} ->
                     (width <= maxWidth && width >= minWidth) #
                        as features children

                }

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.innerWidth" innerWidth_js :: IO Int
#endif

innerWidth :: IO Int
innerWidth =
#ifdef __GHCJS__
    innerWidth_js
#else
    return 0
#endif

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data FireOnMount = FireOnMount_
pattern FireOnMount :: HasProp FireOnMount a => Prop FireOnMount a -> a -> a
pattern FireOnMount p a <- (getProp FireOnMount_ &&& id -> (p,a)) where
    FireOnMount p a = setProp FireOnMount_ p a

data MaxWidth = MaxWidth_
pattern MaxWidth :: HasProp MaxWidth a => Prop MaxWidth a -> a -> a
pattern MaxWidth p a <- (getProp MaxWidth_ &&& id -> (p,a)) where
    MaxWidth p a = setProp MaxWidth_ p a

data MinWidth = MinWidth_
pattern MinWidth :: HasProp MinWidth a => Prop MinWidth a -> a -> a
pattern MinWidth p a <- (getProp MinWidth_ &&& id -> (p,a)) where
    MinWidth p a = setProp MinWidth_ p a

data OnUpdate = OnUpdate_
pattern OnUpdate :: HasProp OnUpdate a => Prop OnUpdate a -> a -> a
pattern OnUpdate p a <- (getProp OnUpdate_ &&& id -> (p,a)) where
    OnUpdate p a = setProp OnUpdate_ p a

instance HasProp As Responsive where
    type Prop As Responsive = Features -> [View] -> View
    getProp _ = as
    setProp _ a r = r { as = a }

instance HasFeatures Responsive where
    getFeatures = features
    setFeatures as r = r { features = as }

instance HasChildren Responsive where
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasProp FireOnMount Responsive where
    type Prop FireOnMount Responsive = Bool
    getProp _ = fireOnMount
    setProp _ fom r = r { fireOnMount = fom }

instance HasProp MaxWidth Responsive where
    type Prop MaxWidth Responsive = Int
    getProp _ = maxWidth
    setProp _ mw r = r { maxWidth = mw }

instance HasProp MinWidth Responsive where
    type Prop MinWidth Responsive = Int
    getProp _ = minWidth
    setProp _ mw r = r { minWidth = mw }

instance HasProp OnUpdate Responsive where
    type Prop OnUpdate Responsive = IO ()
    getProp _ = onUpdate
    setProp _ ou r = r { onUpdate = ou }
