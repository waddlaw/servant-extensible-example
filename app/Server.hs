{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Todo

import           Control.Concurrent.STM
import           Control.Lens             ((&), (.~))
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as B
import           Data.Extensible
import           Data.IntMap              (IntMap)
import qualified Data.IntMap              as IntMap
import           Network.HTTP.Media       ((//), (/:))
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

type API = Get '[HTML] ByteString
         :<|> "static" :> Raw
         :<|> Todo.CRUD

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ bs = bs

api :: Proxy API
api = Proxy

todoList :: [Todo]
todoList =
  [ #todoId @= 1 <: #title @= "アドベントカレンダーを書く" <: #done @= True <: nil
  , #todoId @= 2 <: #title @= "Haskellで仕事する" <: #done @= False <: nil
  , #todoId @= 3 <: #title @= "寝る" <: #done @= False <: nil
  ]

server :: ByteString -> TVar (Int, IntMap Todo) -> Server API
server indexHtml db = index
       :<|> serveDirectoryFileServer "todo-server/static"
       :<|> getTodoAll
       :<|> postTodo
       :<|> putTodoId
       :<|> deleteTodoId
  where
    index              = pure indexHtml
    getTodoAll         = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
    postTodo todo      = liftIO . atomically $ do
                           (maxId, m) <- readTVar db
                           let newId = maxId + 1
                               newTodo = todo & #todoId .~ newId
                           writeTVar db (newId, IntMap.insert newId newTodo m)
                           pure newTodo
    putTodoId tid todo = liftIO . atomically . modifyTVar db $
                           \(maxId, m) -> (maxId, IntMap.insert tid todo m)
    deleteTodoId tid   = liftIO . atomically . modifyTVar db $
                           \(maxId, m) -> (maxId, IntMap.delete tid m)

main :: IO ()
main = do
  db <- atomically $ newTVar (0, IntMap.empty)
  indexHtml <- B.readFile "todo-server/templates/index.html"
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api (server indexHtml db)
