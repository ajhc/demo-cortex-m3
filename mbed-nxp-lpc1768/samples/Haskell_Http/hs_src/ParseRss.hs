module ParseRss where
import Data.List

titleTag :: String
titleTag = "<title>"

type SL = [String]

dropSL :: Int -> SL -> SL
dropSL _ []                    = []
dropSL _ [[]]                  = []
dropSL n xs | n <= 0           = xs
dropSL n [x]                   = dropSL 0 [drop n x]
dropSL n (x:xs) | length x < n = dropSL (n - length x) xs
                | otherwise    = (drop n x):xs

dropWhileSL :: (Char -> Bool) -> SL -> SL
dropWhileSL _ []         =  []
dropWhileSL _ [[]]       =  []
dropWhileSL p (x:xs) = let x' = dropWhile p x
                       in if x' /= "" then x':xs
                          else dropWhileSL p xs

takeWhileSL               :: (Char -> Bool) -> SL -> String
takeWhileSL _ []     =  ""
takeWhileSL _ [[]]   =  ""
takeWhileSL p (x:xs) = go $ takeWhile p x
  where go x' | length x > length x' = x'
              | otherwise            = x ++ takeWhileSL p xs

isPrefixOfSL :: String -> SL -> Bool
isPrefixOfSL [] _     = True
isPrefixOfSL _  []    = False
isPrefixOfSL _  [[]]  = False
isPrefixOfSL s  (x:xs) | length s <= length x             = s `isPrefixOf` x
                       | take (length x) s `isPrefixOf` x = (drop (length x) s) `isPrefixOfSL` xs
                       | otherwise                        = False

findTitleTag :: SL -> SL
findTitleTag s = if titleTag `isPrefixOfSL` s' then s''
                 else findTitleTag s'''
  where s'   = dropWhileSL (/= '<') s
        s''  = dropSL (length titleTag) s'
        s''' = dropSL 1 s'

showTitle :: (String -> IO ()) -> SL -> IO SL
showTitle io s = do
  io $ takeWhileSL (/= '<') s
  return $ dropSL 1 $ dropWhileSL (/= '<') s

printTitle :: (String -> IO ()) -> SL -> IO ()
printTitle io s = do
  skip s >>= skip >>= ioprint
  return ()
  where skip = showTitle (const $ return ()) . findTitleTag
        ioprint = showTitle io . findTitleTag
