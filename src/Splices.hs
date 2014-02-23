{-# LANGUAGE OverloadedStrings #-}

module Splices where

import qualified Data.Text as T
import Heist
import Snap.Snaplet.Heist
import Heist.Interpreted

import Application
import Game

gameSplices :: Game -> Splices (Splice AppHandler)
gameSplices (Game i s b cfs cs ps) =
  do "stage" ## textSplice (T.pack (show s))
     "id" ## textSplice (T.pack (show i))
     "players" ## mapSplices (runChildrenWith . playerSplice) ps

playerSplice :: Player -> Splices (Splice AppHandler)
playerSplice (Player nm as c p al) = do "name" ## textSplice nm
                                        "assets" ## textSplice (T.pack (show as))
                                        "class" ## textSplice (T.pack (show c))
