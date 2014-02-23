{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import           Control.Lens
import           Control.Monad.Trans
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Fay
import           Snap.Util.FileServe
import           Heist
import           Heist.Splices
import           Heist.Interpreted
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Snap.Snaplet.AcidState
import           Control.Monad.Trans (liftIO)
import           System.Random (randomIO, randomRIO)
------------------------------------------------------------------------------
import           Application
import           Game
import           Splices

routes :: [(ByteString, AppHandler ())]
routes = [ ("/fay",      with fay fayServe)
         , ("/new",      newGameHandler)
         , ("/game/:id", gameHandler)
         , ("",          serveDirectory "static")
         ]

showError :: Text -> AppHandler ()
showError e = renderWithSplices "error" ("err" ## textSplice e)

gamePathId :: Int -> ByteString
gamePathId n = B.append "/game/" (B8.pack (show n))
gamePath :: Game -> ByteString
gamePath = gamePathId . gameId

getGameId :: AppHandler Int
getGameId = do n <- fmap abs $ liftIO randomIO
               mg <- query (GetGame n)
               case mg of
                 Nothing -> return n
                 Just _ -> getGameId

newGameHandler :: AppHandler ()
newGameHandler = do n <- getGameId
                    update (NewGame n)
                    redirect (gamePathId n)

gameHandler :: AppHandler ()
gameHandler =
  do mid <- getParam "id"
     case fmap (read.B8.unpack) mid of
       Nothing -> pass
       Just i ->
         do mg <- query (GetGame i)
            case mg of
              Nothing -> pass
              Just game ->
                route $ over (mapped._2) ($ game)
                             [("", ifTop . showGameHandler)
                             ,("add_player", addPlayerHandler)
                             ,("roll", rollHandler)
                             ]

showGameHandler :: Game -> AppHandler ()
showGameHandler game = renderWithSplices "game/show" (gameSplices game)

addPlayerHandler :: Game -> AppHandler ()
addPlayerHandler game =
  do n <- getParam "name"
     case n of
       Nothing -> showError "No name specified."
       Just name -> do let np = Player (T.decodeUtf8 name) 0 Worker 0 []
                       update (UpdateGame (game { gamePlayers =  np : (gamePlayers game)}))
                       redirect (gamePath game)

rollHandler :: Game -> AppHandler ()
rollHandler game = do roll <- liftIO $ randomRIO (1,6)
                      let allP = gamePlayers game
                      let curP = head allP
                      let board = unBoard (gameBoard game)
                      let newPos = (roll + pPosition curP) `mod` length board
                      let place = board !! newPos
                      let (newP, newG, placeMsg) = modPlayer (curP { pPosition = newPos}) game place
                      let players = tail allP ++ [newP]
                      update (UpdateGame (newG { gamePlayers = players }))
                      setMessage (T.concat [pName curP
                                           ," rolled a "
                                           , T.pack (show roll)
                                           , ". "
                                           , T.pack placeMsg])
                      redirect (gamePath game)
  where modPlayer play game place =
          case place of
            WorkerPlace n -> if pClass play == Worker
                                then let newP = play { pAssets = pAssets play + n}
                                         newG = game { gamePlayers = newP : tail (gamePlayers game)}
                                     in
                                     (newP, newG, "Got " ++ (show n) ++ " assets.")
                                else (play, game, "")
            CapitalistPlace n -> if pClass play == Capitalist
                                    then let newP = play { pAssets = pAssets play + n}
                                             newG = game { gamePlayers = newP : tail (gamePlayers game)}
                                         in
                                         (newP, newG, "Got " ++ (show n) ++ " assets.")
                                    else (play, game, "")
            BothPlace nw nc
              | pClass play == Worker ->
                let newP = play { pAssets = pAssets play + nw}
                    newG = game { gamePlayers = newP : tail (gamePlayers game)}
                in
                (newP, newG, "Got " ++ (show nw) ++ " assets.")
              | pClass play == Capitalist ->
                let newP = play { pAssets = pAssets play + nc}
                    newG = game { gamePlayers = newP : tail (gamePlayers game)}
                in
                (newP, newG,
                 "Got " ++ (show nc) ++ " assets.")
              | otherwise -> (play, game, "")
            ChancePlace -> let curC = head (gameChanceCards game)
                               restC = tail (gameChanceCards game)
                               newP' = if ccAssets curC /= 0
                                       then play { pAssets = pAssets play + ccAssets curC}
                                       else play
                               (newP, pa) = if ccSpaces curC /= 0
                                             then (newP' { pPosition = (pPosition play + ccSpaces curC) `mod` (length (unBoard (gameBoard game)))}
                                                  , True)
                                             else (newP', False)
                               allP = gamePlayers game
                               newG = game { gameChanceCards = restC ++ [curC], gamePlayers = newP : tail allP}
                               msg = "Chance Card: " ++ (ccText curC)
                           in if pa
                              then let (newPr, newGr, msgR) = modPlayer newP newG (unBoard (gameBoard newG) !! pPosition newP)
                                   in (newPr, newGr, msg ++ ". " ++ msgR)
                              else (newP, newG, msg)
            ConfrontPlace -> (play, game, "")
            AlliancePlace cls -> (play, game, "")

setMessage :: Text -> AppHandler ()
setMessage t = with sess $ do setInSession "message" t
                              commitSession

hasMessageSplice :: Splice AppHandler
hasMessageSplice = do m <- lift $ with sess (getFromSession "message")
                      ifISplice (isJust m)

renderMessageSplice :: Splice AppHandler
renderMessageSplice = do m <- lift $ with sess (getFromSession "message")
                         lift $ with sess $ do deleteFromSession "message"
                                               commitSession
                         case m of
                           Nothing -> return []
                           Just m' -> return [X.TextNode m']

messageSplices :: Splices (Splice AppHandler)
messageSplices = do "has-message" ## hasMessageSplice
                    "render-message" ## renderMessageSplice

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h mempty { hcInterpretedSplices = messageSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    a <- nestSnaplet "acid" acid $ acidInit (GameState M.empty)
    f <- nestSnaplet "fay" fay initFay
    return $ App h s f a
