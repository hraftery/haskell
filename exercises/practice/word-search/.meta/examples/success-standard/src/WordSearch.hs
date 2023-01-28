module WordSearch (wordSearch) where

import Control.Monad ( msum ) -- we use it like "fromJusts" in GHC.Data.Maybe 
import Data.List ( elemIndices, transpose )

type ColRow   = (Int, Int)
type StartEnd = (ColRow, ColRow)

wordSearch :: [String] -> [String] -> [(String, Maybe StartEnd)]
wordSearch grid = map (\x -> (x, toOneBased $ wordSearchSingle grid x))

wordSearchSingle :: [String] -> String -> Maybe StartEnd
wordSearchSingle _    ""     = error "Must provide search word"
wordSearchSingle grid (w:ws) = let starts = [(ci,ri) | (ri,r) <- zip [0..] grid
                                                     , ci     <- elemIndices w r]
                               in msum $ map (wordSearchSingleFrom grid ws) starts

wordSearchSingleFrom :: [String] -> String -> ColRow -> Maybe StartEnd
wordSearchSingleFrom grid ws (c,r) = let end = msum [left,right,down,up,
                                                     diagLD, diagLU, diagRD, diagRU]
                                     in (\e -> Just ((c,r),e)) =<< end where
  gi            = length grid - 1
  gridDiagRight = zipWith rotate [0      ..] $ map (replicate gi '\0' ++) grid
  gridDiagLeft  = zipWith rotate [gi,gi-1..] $ map (replicate gi '\0' ++) grid
  len   = length ws
  cDR   = c + (gi-r) -- column in diagonal grid depends on row
  cDL   = c + r
  left  = if ws == (take len . reverse . take c . (!!r)              ) grid
          then Just (c-len,r) else Nothing
  right = if ws == (take len .       drop (c+1) . (!!r)              ) grid
          then Just (c+len,r) else Nothing
  down  = if ws == (take len .       drop (r+1) . (!!c)   . transpose) grid
          then Just (c,r+len) else Nothing
  up    = if ws == (take len . reverse . take r . (!!c)   . transpose) grid
          then Just (c,r-len) else Nothing
  diagLD= if ws == (take len .       drop (r+1) . (!!cDL) . transpose) gridDiagLeft
          then Just (c-len,r+len) else Nothing
  diagLU= if ws == (take len . reverse . take r . (!!cDL) . transpose) gridDiagLeft
          then Just (c+len,r-len) else Nothing
  diagRD= if ws == (take len .       drop (r+1) . (!!cDR) . transpose) gridDiagRight
          then Just (c+len,r+len) else Nothing
  diagRU= if ws == (take len . reverse . take r . (!!cDR) . transpose) gridDiagRight
          then Just (c-len,r-len) else Nothing

toOneBased :: Maybe StartEnd -> Maybe StartEnd
toOneBased Nothing                  = Nothing
toOneBased (Just ((c0,r0),(c1,r1))) = Just ((c0+1,r0+1),(c1+1,r1+1))

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
