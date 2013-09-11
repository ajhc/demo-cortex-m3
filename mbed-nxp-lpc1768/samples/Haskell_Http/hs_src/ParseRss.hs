module ParseRss where
import Data.List

titleTag :: String
titleTag = "<title>"

findTitleTag :: String -> String
findTitleTag = go
  where go s = let s' = dropWhile (/= '<') s
               in if titleTag `isPrefixOf` s' then drop (length titleTag) s'
                  else go $ drop 1 s'

showTitle :: (Char -> IO ()) -> String -> IO String
showTitle io = go
  where go (x:xs) = do
          if x /= '<' then io x >> go xs
            else return xs

printTitle :: (Char -> IO ()) -> String -> IO ()
printTitle io s = do
  skip s >>= ioprint >>= ioprint >>= ioprint
  return ()
  where skip = showTitle (const $ return ()) . findTitleTag
        ioprint = showTitle io . findTitleTag
