module Main (main) where

import DLPack
import Numeric.TBLIS

main :: IO ()
main = do
  let a :: [[Double]]
      a =
        [ [1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]
        ]
      b :: [[Double]]
      b =
        [ [1, 1, 1],
          [1, 1, 1],
          [1, 1, 1]
        ]
  (b' :: [Double]) <-
    withDLTensor a $ \tensorA ->
      withDLTensor b $ \tensorB -> do
        tblisAdd (1 :: Double) False tensorA "ij" 0 tensorB "ji"
        return $ tensorToFlatList tensorB
  print b'
  putStrLn "Hello world!"
