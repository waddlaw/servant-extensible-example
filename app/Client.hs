{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import           Todo                     as Todo

import           Control.Lens             (view, (&), (.~))
import           Control.Monad            (void)
import           Control.Monad.IO.Class
import           Data.Extensible
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Client

getTodoAll   :: ClientM [Todo]
postTodo     :: Todo -> ClientM Todo
putTodoId    :: Int -> Todo -> ClientM ()
deleteTodoId :: Int -> ClientM ()

getTodoAll :<|> postTodo :<|> putTodoId :<|> deleteTodoId = client Todo.crud

todoList :: [Todo]
todoList =
  [ #todoId @= 1 <: #title @= "アドベントカレンダーを書く" <: #done @= True <: nil
  , #todoId @= 2 <: #title @= "Haskellで仕事する" <: #done @= False <: nil
  , #todoId @= 3 <: #title @= "寝る" <: #done @= False <: nil
  ]

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "localhost" 8080 ""
  void . flip runClientM env $ do
    mapM_ postTodo todoList
    putTodoId 1 $ (todoList !! 0) & #done .~ True
    deleteTodoId 3
    list <- getTodoAll
    liftIO . mapM_ putStrLn $ map (view #title) list
