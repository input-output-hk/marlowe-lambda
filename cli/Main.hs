

module Main (
  main
) where


import Data.Default (def)
import Language.Marlowe.Lambda (handle)


main :: IO ()
main =
   do
     input <- getContents
     print =<< handle input def
