

module Main (
  main
) where


import Data.Aeson (eitherDecode, encode)
import Data.Default (def)
import Language.Marlowe.Lambda (handle)
import System.Exit (die)

import qualified Data.ByteString.Lazy.Char8 as LBS8 (getContents, putStrLn, unpack)


main :: IO ()
main =
   do
     input' <- eitherDecode <$> LBS8.getContents
     case input' of
       Left msg    -> die msg
       Right input -> do
                        result <- handle input def
                        case result of
                          Right msg -> LBS8.putStrLn $ encode msg
                          Left  msg -> die . LBS8.unpack $ encode msg
