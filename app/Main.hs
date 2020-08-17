module Main where

type City = (Float, Float)

main :: IO ()
main = interact handle

handle :: String -> String
handle = showCities . readCities

readCities :: String -> [City]
readCities = (map readCity) . lines

readCity :: String -> City
readCity coords = (x, y)
    where ws = words coords
          x = read $ head ws
          y = read $ head $ tail ws

showCities :: [City] -> String
showCities = unlines . (map showCity)

showCity :: City -> String
showCity city = show (fst city) ++ show (snd city)
