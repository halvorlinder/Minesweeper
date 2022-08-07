module Parse(coordinatePred, coordinateRead) where 
import Data.Char (ord, toLower, isNumber, isAlpha)
coordinateRead :: String -> Int
coordinateRead numstr
 | isNat numstr = read numstr
 | isLetter numstr =  ( ord . toLower . head $numstr ) - 87
 | otherwise = error "Could not read coord from string"

coordinatePred :: String -> Bool
coordinatePred numstr =  isNat numstr || isLetter numstr

isNat :: String -> Bool
isNat = all isNumber

isLetter :: String -> Bool
isLetter str = length str == 1 && isAlpha  ( head str )