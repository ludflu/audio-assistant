module TriggerWindow where

import Data.List


windows :: Int -> [a] -> [[a]]
windows n xs = Data.List.transpose (take n (tails xs))

dewindow :: [[a]] -> [a]
dewindow as = let first = head as
                  windowLength = length first
		  rest = tail as
		  rinds = map (drop (windowLength - 1)) rest
	       in first ++ concat rinds

turnOn :: Int ->[ (Bool,a) ] -> Bool
turnOn thresh bs = let b = map fst bs
		       numTrue = length $ filter (== True) b
		    in (numTrue >= thresh) 


turnOff :: Int -> [ (Bool,a) ] -> Bool
turnOff thresh bs = let b = map fst bs
	  	        numFalse = length $ filter (== False) b
		     in (numFalse >= thresh) 

justOn :: Int -> [[(Bool,a)]] -> [[(Bool,a)]]
justOn thresh sq = let notOff = not . (turnOff thresh)
		       notOn = not . (turnOn thresh)
		    in takeWhile notOff $ dropWhile notOn sq

-- given 
-- a list of items, 
-- a list of flags, 
-- a window size and 
-- a threshold
-- returns ONLY the items
-- in between and "activation" and a "deactivation"
-- where an activate consists of:
-- a window where "threshold" percent of the items were flagged
--
findBoundary :: [a] -> [Bool] -> Int -> Double -> [a]
findBoundary items test size threshold = 
  let itemss = zip test items
      thresh = round $ fromIntegral size * threshold
      wnd = windows size items
      onlyOn = justOn thresh wnd
   in map snd $ dewindow onlyOn
