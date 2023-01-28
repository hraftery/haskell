module WordSearch (wordSearch) where

type ColRow   = (Int, Int)
type StartEnd = (ColRow, ColRow)

wordSearch :: [String] -> [String] -> [(String, Maybe StartEnd)]
wordSearch haystack needles = error "You need to implement this function."
