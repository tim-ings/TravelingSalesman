module Main where

import Data.List
import Debug.Trace

data City = City { x :: Float
                 , y :: Float
                 } deriving (Eq, Show)
data Trip = Trip { city :: City
                 , dist :: Float
                 } deriving (Eq, Show)
type Route = ([Trip], [City])

main :: IO ()
main = interact handle

handle :: String -> String
handle inp = show $ bestRoute $ snd nn
    where cs = readCities inp
          nn = findPathsFromAllStarts cs []

readCities :: String -> [City]
readCities = (map readCity) . lines

readCity :: String -> City
readCity coords = City x y
    where ws = words coords
          x = read $ head ws
          y = read $ head $ tail ws

euclidDist :: City -> City -> Float
euclidDist c0 c1 = sqrt ((x c1 - x c0) ** 2 + (y c1 - y c0) ** 2)

cityDist :: City -> City -> Trip
cityDist origin dest = Trip dest $ euclidDist origin dest

cityDists :: City -> [City] -> [Trip]
cityDists origin = map (cityDist origin)

cmpCityDist :: Trip -> Trip -> Ordering
cmpCityDist t0 t1 = compare (dist t0) (dist t1)

nearestNeighbour :: City -> [City] -> (Trip, [City])
nearestNeighbour origin others = (head trips, unvisited)
    where trips = (sortBy cmpCityDist) $ cityDists origin others
          unvisited = map city $ tail trips

findNNRoute :: [Trip] -> [City] -> Route
findNNRoute path unvisited
    | unvisited == [] = (path, unvisited)
    | otherwise = findNNRoute (nextCity : path) remaining
        where (nextCity, remaining) = nearestNeighbour (city $ head path) unvisited

filterOrigin :: City -> [City] -> [City]
filterOrigin origin = filter (\c -> x c /= x origin && y c /= y origin)

findPathsFromAllStarts :: [City] -> [Route] -> ([City], [Route])
findPathsFromAllStarts cities routes
    | length routes >= length cities = (cities, routes)
    | otherwise = findPathsFromAllStarts cities (route:routes)
        where origin = cities !! length routes
              route = findNNRoute [Trip origin 0] $ (filterOrigin origin) cities

routeLength :: Route -> Float
routeLength = sum . map dist . fst

routeLengths :: [Route] -> [Float]
routeLengths = map routeLength

compareRouteLengths :: (Route, Float) -> (Route, Float) -> Ordering
compareRouteLengths rl0 rl1 = compare (snd rl0) (snd rl1)

bestRoute :: [Route] -> (Route, Float)
bestRoute routes = head $ sortBy compareRouteLengths $ rls
    where lengths = routeLengths routes
          rls = zip routes lengths
