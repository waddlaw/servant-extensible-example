module Main where

import           Todo       as Todo

import           Servant.JS

main :: IO ()
main = writeJSForAPI Todo.crud jquery "todo-server/static/todo_crud.js"
