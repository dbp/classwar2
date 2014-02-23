{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

{-|

This module contains game mechanics and logic for Class War.

-}

module Game where

import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.SafeCopy
import Data.Typeable

data Class = Capitalist | Worker | Student | Manager | SmallBusinessOwner | UnderClass deriving (Typeable, Show, Eq)

data ClassCategory = MajorClasses | MinorClasses deriving (Typeable, Show)

data Player = Player { pName :: Text
                     , pAssets :: Int
                     , pClass :: Class
                     , pPosition :: Int
                     , pAllies :: [Player]
                     } deriving Typeable

data ChanceCard = ChanceCard { ccText :: String
                             , ccAssets :: Int
                             , ccAssetsMult :: Int
                             , ccExtraTurns :: Int
                             , ccSkippedTurns :: Int
                             , ccSpaces :: Int
                             , ccMoveTo :: Maybe Place
                             } deriving Typeable

blankChanceCard :: ChanceCard
blankChanceCard = ChanceCard "" 0 0 0 0 0 Nothing

defaultChanceCards :: [ChanceCard]
defaultChanceCards = [ blankChanceCard { ccText = "Gain 4 assets.", ccAssets = 4 }
                     , blankChanceCard { ccText = "Move forward 3 spaces", ccSpaces = 3}
                     , blankChanceCard { ccText = "Move back 2 spaces", ccSpaces = -2}
                     , blankChanceCard { ccText = "Lose 2 assets", ccAssets = -2}
                     , blankChanceCard { ccText = "Move back 1 space", ccSpaces = -1}
                     , blankChanceCard { ccText = "Gain 12 assets", ccAssets = 12}
                     ]

data ConfrontationCard = ConfrontationTurns Int | ConfrontationAssets Int deriving Typeable

defaultConfrontationCards :: [ConfrontationCard]
defaultConfrontationCards = [ConfrontationAssets 2, ConfrontationTurns 1, ConfrontationAssets 4, ConfrontationAssets 5]

data Place = WorkerPlace Int
            | CapitalistPlace Int
            | BothPlace {workerAmnt :: Int, capAmnt :: Int}
            | ChancePlace
            | ConfrontPlace
            | AlliancePlace ClassCategory deriving (Typeable)

instance Show Place where
  show (WorkerPlace n) = "Worker Gains " ++ (show n)
  show (CapitalistPlace n) = "Capitalist Gains " ++ (show n)
  show (BothPlace nw nc) = "Worker Gains " ++ (show nw) ++ ", Capitalist Gains " ++ (show nc)
  show ChancePlace = "Chance"
  show ConfrontPlace = "Confrontation"
  show (AlliancePlace cat) = "Alliance with " ++ (show cat)

data Die = One | Two | Three | Four | Five | Six deriving Typeable

data Board = Board { unBoard :: [Place] } deriving Typeable

data Stage = Stage1 | Stage2 | Stage3 deriving (Typeable, Show)

defaultBoard :: Board
defaultBoard = Board [ WorkerPlace 1
                     , CapitalistPlace 3
                     , AlliancePlace MinorClasses
                     , WorkerPlace 2
                     , ChancePlace
                     , CapitalistPlace 1
                     , AlliancePlace MajorClasses
                     , BothPlace 3 2
                     , BothPlace 1 1
                     , WorkerPlace 2
                     , ConfrontPlace
                     , WorkerPlace 3
                     , AlliancePlace MajorClasses
                     , ChancePlace
                     , WorkerPlace 1
                     , ConfrontPlace
                     , BothPlace 2 2
                     , CapitalistPlace 2
                     , AlliancePlace MinorClasses
                     , ChancePlace
                     ]

-- | Players is sorted in order of play.
data Game = Game { gameId :: Int
                 , gameStage :: Stage
                 , gameBoard :: Board
                 , gameConfrontCards :: [ConfrontationCard]
                 , gameChanceCards :: [ChanceCard]
                 , gamePlayers :: [Player]
                 } deriving Typeable

data GameState = GameState { unGameState :: !(Map Int Game) } deriving (Typeable)

getGame :: Int -> Query GameState (Maybe Game)
getGame n = M.lookup n . unGameState <$> ask

newGame :: Int -> Update GameState ()
newGame n = modify (GameState . M.insert n (Game n Stage1 defaultBoard defaultConfrontationCards defaultChanceCards []) . unGameState)

updateGame :: Game -> Update GameState ()
updateGame gm = modify (GameState . M.insert (gameId gm ) gm . unGameState)

-- Acid state boilerplate
deriveSafeCopy 0 'base ''Class
deriveSafeCopy 0 'base ''ClassCategory
deriveSafeCopy 0 'base ''Player
deriveSafeCopy 0 'base ''ChanceCard
deriveSafeCopy 0 'base ''ConfrontationCard
deriveSafeCopy 0 'base ''Place
deriveSafeCopy 0 'base ''Die
deriveSafeCopy 0 'base ''Board
deriveSafeCopy 0 'base ''Stage
deriveSafeCopy 0 'base ''Game
deriveSafeCopy 0 'base ''GameState
makeAcidic ''GameState ['getGame, 'newGame, 'updateGame]

type Wager = Int

-- Non persistent data follows
data View = View

data Move = RollAndMove | ConfrontWager | AllianceChoice Player

render :: Game -> View
render = undefined

play :: Move -> Game -> Game
play = undefined

moves :: Game -> (Player, [Move])
moves = undefined
