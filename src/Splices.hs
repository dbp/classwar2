{-# LANGUAGE OverloadedStrings #-}

module Splices where

import qualified Data.Text as T
import Heist
import Heist.Splices
import Snap.Snaplet.Heist
import Heist.Interpreted
import Data.List (lookup)
import Data.Maybe (fromJust)
import qualified Text.XmlHtml as X

import Application
import Game

gameSplices :: Game -> Splices (Splice AppHandler)
gameSplices (Game i s b cfs cs ps) =
  do "stage" ## textSplice (T.pack (show s))
     "id" ## textSplice (T.pack (show i))
     "players" ## mapSplices (runChildrenWith . playerSplice) ps
     "board" ## mapSplices (runChildrenWith . placeSplice) (zip [0..] (unBoard b))

playerSplice :: Player -> Splices (Splice AppHandler)
playerSplice (Player nm as c p al) = do "name" ## textSplice nm
                                        "assets" ## textSplice (T.pack (show as))
                                        "class" ## textSplice (T.pack (show c))
                                        "position" ## textSplice (T.pack (show p))


placeSplice :: (Int, Place) -> Splices (Splice AppHandler)
placeSplice (i, p) = do "render" ## textSplice (T.pack (show p))
                        "is-place" ## isPlaceSplice i


isPlaceSplice :: Int -> Splice AppHandler
isPlaceSplice i = do n <- getParamNode
                     case lookup "id" (X.elementAttrs n) of
                       Nothing -> return []
                       Just n -> ifISplice (n == (T.pack (show i)))
