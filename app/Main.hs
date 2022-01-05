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
      c :: [Double]
      c = [1, 1, 1]
  (b' :: [Double]) <-
    withDLTensor a $ \dlTensorA ->
      withDLTensor c $ \dlTensorB -> do
        let tensorA = tblisFromDLTensor dlTensorA
            tensorB = tblisScale 0 $ tblisFromDLTensor dlTensorB
        tblisAdd tensorA "ij" tensorB "ji"
        return $ tensorToFlatList tensorB
  print b'
  putStrLn "Hello world!"
