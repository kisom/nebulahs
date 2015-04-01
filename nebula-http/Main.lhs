> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Control.Monad.IO.Class
> import Data.Aeson
> import Data.Text.Lazy as Text
> import System.Environment
> import Web.Scotty (scotty)
> import Web.Scotty.Trans
>
> import qualified Nebula.Store as Store 

This Main module serves as an HTTP API to the Nebula data store as a
demonstrator.

The getPort function looks up a port and determines if it's 

> getPort :: IO Int
> getPort = do
>   args <- getArgs
>   case args of
>    []     -> return 3000
>    (x:xs) -> return $ read x



> main :: IO ()
> main = do
>    port <- getPort
>    scotty port $ do
>        get "/" $ do
>          text $ Text.pack "hello, world"
>        post "/entry/:uuid" $ do
>          uuid    <- param "uuid"
>          reqBody <- body
>          return ()

