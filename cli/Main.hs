

{-# LANGUAGE LambdaCase #-}


module Main (
  main
) where


import Data.Aeson (eitherDecode, encode)
import Data.Default (def)
import Language.Marlowe.Runtime.Client (handle)
import System.Environment (getArgs)
import System.Exit (die)

import qualified Data.ByteString.Lazy.Char8 as LBS8 (getContents, putStrLn, unpack)


main :: IO ()
main =
   do
     config <-
       getArgs >>= \case
         []     -> pure def
         [file] -> read <$> readFile file
         _      -> die "Invalid argument."
     LBS8.getContents
       >>= (
         \case
           Left msg    -> die msg
           Right input -> handle config input >>= \case
                            Right msg -> LBS8.putStrLn $ encode msg
                            Left  msg -> die . LBS8.unpack $ encode msg
       ) . eitherDecode
