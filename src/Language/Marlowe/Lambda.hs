module Language.Marlowe.Lambda (
  handler
) where


import Aws.Lambda (Context)


handler :: String
        -> Context ()
        -> IO (Either String Int)
handler someText _ =
  do
    let wordsCount = length $ words someText
    if wordsCount > 0 then
      pure $ Right wordsCount
    else
      pure $ Left "Sorry, your text was empty"
