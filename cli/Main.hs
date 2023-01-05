

{-# LANGUAGE LambdaCase #-}


module Main (
  main
) where


import Data.Aeson (eitherDecode, encode)
import Language.Marlowe.Runtime.App (handle)
import Language.Marlowe.Runtime.App.Parser (getConfigParser)

import qualified Data.ByteString.Lazy.Char8 as LBS8 (getContents, putStrLn, lines)
import qualified Options.Applicative as O


main :: IO ()
main =
  do
    configParser <- getConfigParser
    config <-
      O.execParser
        $ O.info
          (O.helper {- <*> O.versionOption -} <*> configParser)
          (
            O.fullDesc
              <> O.progDesc "This command-line tool reads lines of JSON from standard input, interpets them as Marlowe App requests, executes them, and prints the response JSON on standard output."
              <> O.header "marlowe-lambda-cli: run marlowe application requests"
          )
    requests <- LBS8.lines <$> LBS8.getContents
    sequence_
      [
        case eitherDecode line of
          Right request -> handle config request
                             >>= \case
                              Right response -> LBS8.putStrLn $ encode response
                              Left message -> LBS8.putStrLn $ encode message
          Left message -> LBS8.putStrLn $ encode message
      |
        line <- requests
      ]
