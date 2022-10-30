

{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Data.Aeson ((.=), decode, encode, object)
import Data.Default (def)
import Language.Marlowe.Lambda (handle)

import Data.ByteString.Lazy.Char8 as LBS8 (getContents, putStr)


main :: IO ()
main =
   do
     Just input <- decode <$> LBS8.getContents
     result <- handle input def
     LBS8.putStr . encode
       $ case result of
           Right msg -> object ["success" .= msg]
           Left  msg -> object ["failure" .= msg]
