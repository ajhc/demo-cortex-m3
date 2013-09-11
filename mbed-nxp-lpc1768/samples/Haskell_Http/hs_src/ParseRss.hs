module ParseRss where
import Data.List

titleTag :: String
titleTag = "<title>"

findTitleTag :: String -> String
findTitleTag s = if titleTag `isPrefixOf` s' then s'' `seq` s''
                 else s''' `seq` findTitleTag s'''
  where s'   = dropWhile (/= '<') s
        s''  = drop (length titleTag) s'
        s''' = drop 1 s'

showTitle :: (String -> IO ()) -> String -> IO String
showTitle io s = do
  io $ takeWhile (/= '<') s
  return $ drop 1 $ dropWhile (/= '<') s

printTitle :: (String -> IO ()) -> String -> IO ()
printTitle io s = do
  skip s >>= skip >>= ioprint
  return ()
  where skip = showTitle (const $ return ()) . findTitleTag
        ioprint = showTitle io . findTitleTag
