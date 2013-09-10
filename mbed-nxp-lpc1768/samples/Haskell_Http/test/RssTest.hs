import Control.Monad
import Text.ParserCombinators.Parsec

rssTag = do
  manyTill anyChar $ try atomTag
  i1 <- itemTag
  i2 <- itemTag
  i3 <- itemTag
  return [i1, i2, i3]

atomTag = do string "<atom"
             many (noneOf ">")
             string ">"

itemTag = do string "<item>"
             t <- titleTag
             skipTag "link"
             skipTag "guid"
             skipTag "pubDate"
             skipTag "description"
             string "</item>"
             return t

titleTag = do string "<title>"
              t <- many (noneOf "<>")
              string "</title>"
              return t

skipTag t = do string ('<':t)
               many (noneOf "<")
               string $ "</" ++ t ++ ">"

main :: IO ()
main = do
  c <- getContents
  print $ parse rssTag "" c
