import Data.List

titleTag :: String
titleTag = "<title>"

findTitleTag :: String -> String
findTitleTag = go
  where go s = let s' = dropWhile (/= '<') s
               in if titleTag `isPrefixOf` s' then drop (length titleTag)  s'
                  else go $ drop 1 s'

titleList :: String -> [String]
titleList = unfoldr go
  where go s = let s' = findTitleTag s
               in if s' == "" then Nothing
                  else Just (takeWhile (/= '<') s', s')

main :: IO ()
main = do
  c <- getContents
  print . take 4 . drop 1 . titleList $ c
