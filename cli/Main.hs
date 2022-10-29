module Main (
  main
) where


import Language.Marlowe.Lambda (handler)


main :: IO ()
main =
   do
     input <- getContents
     print =<< handler input undefined
