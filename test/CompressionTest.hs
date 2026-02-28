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

-- | Assert a Right value matches.
assertRight :: (Eq a, Show a) => String -> a -> Either String a -> IO Bool
assertRight label expected (Right actual) = assertEqual label expected actual
assertRight label _ (Left err) = do
  putStrLn ""
  putStrLn ("    " ++ label)
  putStrLn ("    expected Right, got Left: " ++ err)
  pure False

-- | Assert a Left (error case).
assertLeft :: (Show a) => String -> Either String a -> IO Bool
assertLeft _ (Left _) = pure True
assertLeft label (Right val) = do
  putStrLn ""
  putStrLn ("    " ++ label)
  putStrLn ("    expected Left, got Right: " ++ show val)
  pure False

main :: IO ()
main = do
  putStrLn "nova-cache compression tests"
  putStrLn "=============================="
  putStrLn ""
  putStrLn "Compression:"
  ok1 <-
    test "compress/decompress roundtrip" $ do
      let input = BS.pack [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100]
          compressed = Compression.compressXz input
      result <- Compression.decompressXz compressed
      assertRight "roundtrip" input result
  ok2 <-
    test "compress/decompress roundtrip (empty)" $ do
      let compressed = Compression.compressXz BS.empty
      result <- Compression.decompressXz compressed
      assertRight "roundtrip empty" BS.empty result
  ok3 <-
    test "compressed is smaller for repetitive data" $
      let input = BS.replicate 10000 0x42
          compressed = Compression.compressXz input
       in assertTrue "smaller" (BS.length compressed < BS.length input)
  ok4 <-
    test "decompress invalid data returns Left" $ do
      result <- Compression.decompressXz (BS.pack [0, 1, 2, 3])
      assertLeft "invalid xz" result
  putStrLn ""
  if ok1 && ok2 && ok3 && ok4
    then putStrLn "All compression tests passed." >> exitSuccess
    else putStrLn "Some compression tests failed." >> exitFailure
