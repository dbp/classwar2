{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import           Control.Lens
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Fay
import           Snap.Util.FileServe
import           Heist
import           Heist.Interpreted
import qualified Data.Map as M
import           Snap.Snaplet.AcidState
import           Control.Monad.Trans (liftIO)
import           System.Random (randomIO)
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
getGameId = do n <- liftIO randomIO
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
                             ,("add_player", addPlayerHandler)]

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

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    a <- nestSnaplet "acid" acid $ acidInit (GameState M.empty)
    f <- nestSnaplet "fay" fay initFay
    return $ App h s f a
