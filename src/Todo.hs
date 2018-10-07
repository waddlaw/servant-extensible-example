{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Todo
  ( Todo
  , CRUD
  , crud
  ) where

import           Data.Extensible

import           Data.Proxy
import           Servant.API
import           Web.FormUrlEncoded (FromForm (..), parseUnique)

type Todo = Record
  '[ "todoId" >: Int
   , "title"  >: String
   , "done"   >: Bool
   ]

instance FromForm Todo where
  fromForm form =
    hsequence $ #todoId <@=> parseUnique "todoId" form
             <: #title  <@=> parseUnique "title"  form
             <: #done   <@=> parseUnique "done"   form
             <: nil

type CRUD =    "todo" :> "all" :> Get '[JSON] [Todo]
          :<|> "todo" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
          :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
          :<|> "todo" :> Capture "id" Int :> Delete '[JSON] ()


crud :: Proxy CRUD
crud = Proxy
