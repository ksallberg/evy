module Util where

getDec :: Int -> String -> String
getDec _decs "0" = "0"
getDec decs str = beforeDot ++ "." ++ take decs afterDot
  where
    beforeDot = takeWhile (/= '.') str
    afterDot = tail $ dropWhile (/= '.') str
