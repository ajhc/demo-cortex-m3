import Text.ParserCombinators.Parsec

rssTag :: Parser [String]
rssTag = count 5 titleTag -- <= Error occured!!!

titleTag :: Parser String
titleTag = string "<title>"

main :: IO ()
main = do
  c <- getContents
  print $ parse rssTag "" c
