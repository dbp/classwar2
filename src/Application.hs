{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Fay
import Snap.Snaplet.AcidState
import Data.Map (Map)
import qualified Data.Map as M
import Data.SafeCopy
import Data.Typeable

import Game


------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _fay :: Snaplet Fay
    , _acid :: Snaplet (Acid GameState)
    }

makeLenses ''App

instance HasAcid App GameState where
     -- NOTE(dbp 2014-02-23): I obviously don't understand lenses, since the whole point is
     -- that they are composable, but view (snapletValue . acid) doesn't work...
     getAcidStore = (view snapletValue) . (view acid)

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
