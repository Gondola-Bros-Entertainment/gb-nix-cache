module Main (main) where

import qualified Data.ByteString as BS
import qualified NovaCache.Compression as Compression
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

-- | Run a named test, short-circuit on first failure.
test :: String -> IO Bool -> IO Bool
test name action = do
  putStr ("  " ++ name ++ "... ")
  hFlush stdout
  result <- action
  if result
    then do
      putStrLn "OK"
      pure True
    else do
      putStrLn "FAILED"
      pure False

-- | Assert equality.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual
  | expected == actual = pure True
  | otherwise = do
      putStrLn ""
      putStrLn ("    " ++ label)
      putStrLn ("    expected: " ++ show expected)
      putStrLn ("    actual:   " ++ show actual)
      pure False

-- | Assert a Bool is True.
assertTrue :: String -> Bool -> IO Bool
assertTrue _ True = pure True
assertTrue label False = do
  putStrLn ""
  putStrLn ("    " ++ label ++ ": expected True")
  pure False

main :: IO ()
main = do
  putStrLn "nova-cache compression tests"
  putStrLn "=============================="
  putStrLn ""
  putStrLn "Compression:"
  ok1 <-
    test "compress/decompress roundtrip" $
      let input = BS.pack [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]
          compressed = Compression.compressXz input
          decompressed = Compression.decompressXz compressed
       in assertEqual "roundtrip" input decompressed
  ok2 <-
    test "compress/decompress roundtrip (empty)" $
      let compressed = Compression.compressXz BS.empty
          decompressed = Compression.decompressXz compressed
       in assertEqual "roundtrip empty" BS.empty decompressed
  ok3 <-
    test "compressed is smaller for repetitive data" $
      let input = BS.replicate 10000 0x42
          compressed = Compression.compressXz input
       in assertTrue "smaller" (BS.length compressed < BS.length input)
  putStrLn ""
  if ok1 && ok2 && ok3
    then putStrLn "All compression tests passed." >> exitSuccess
    else putStrLn "Some compression tests failed." >> exitFailure
