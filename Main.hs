import System.Random
import Data.List
import Text.Printf

generateBrewTimes :: Int -> IO [Double]
generateBrewTimes n = do
    g <- newStdGen
    let randomMinutes = take n $ randomRs (1.5, 9.5) g
    return $ map (fromIntegral . round . (*10) . max 0) randomMinutes >>= \x -> [x / 10]

cleanData :: [Double] -> [Double]
cleanData = filter (\x -> x >= 1.0 && x <= 15.0)

analyze :: [Double] -> (Double, Double, Double)
analyze xs = (mean, minimum xs, maximum xs)
  where mean = sum xs / fromIntegral (length xs)

visualize :: [Double] -> IO ()
visualize xs = do
    let buckets = [ ("1-3min", \x -> x < 3)
                  , ("3-5min", \x -> x >= 3 && x < 5)
                  , ("5-7min", \x -> x >= 5 && x < 7)
                  , ("7-9min", \x -> x >= 7 && x < 9)
                  , ("9+min",  \x -> x >= 9)
                  ]
        counts = [(label, length (filter f xs)) | (label, f) <- buckets]
    putStrLn "\nKahvinkeittoajat jakautuvat:"
    mapM_ (\(l,c) -> printf "%-8s : %s\n" l (replicate c '#')) counts

main :: IO ()
main = do
    putStrLn "Simuloidaan 30 päivän kahvinkeittoajat..."
    times <- generateBrewTimes 30
    let cleaned = cleanData times
    let (avg, minT, maxT) = analyze cleaned
    printf "Päiviä: %d\n" (length cleaned)
    printf "Keskiarvo: %.1f min\n" avg
    printf "Min: %.1f min, Max: %.1f min\n" minT maxT
    visualize cleaned
