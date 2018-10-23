import Data.List  
import Data.Char
import Data.Ord
-- This function will convert string to lowercase and remove special characters and return list
toWordList = map (map toLower . filter (`notElem` ",.?!-:;'\"")) . words
-- This function will count each words how many time they appeared
countWords xs = map (\xs -> (head xs, length xs)) . group . sort $ xs
-- This function will sort the results from countWords function in order
sortWords sort = sortBy (comparing snd) sort
-- This function will be used by countCommonWords and dropCommonWords to get the specific words from the list
specialWord n = snd . unzip . take n . reverse . sort . map pair . group . sort
     where pair x = (length x, head x)
-- This function will count commonWords in the list
countCommonWords ws = filter (`notElem` specialWord 20 ws) ws
-- This function will get the most frequently used words
dropCommonWords ws = filter (`elem` specialWord 20 ws) ws
-- This function will be called by makeHistogram Function to get the ourput we wanted
toAsterickBar x = show (fst x) ++ " : " ++ (replicate (snd x) '*') ++ "\n"
-- Histogram Function
makeHistogram xs = concat $ map toAsterickBar xs

main = do
  putStrLn "Introduce a filename Noman:"
  fname <- getLine
  textdata <- readFile fname
  let wordlist = toWordList textdata
  putStrLn "Report:"
  putStrLn ("\t" ++ (show $ length wordlist) ++ " words")
  putStrLn ("\t" ++ (show $ length $ countCommonWords wordlist) ++ " common words")
  putStrLn "\nHistogram of the most frequent words (excluding common words):"
  putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist