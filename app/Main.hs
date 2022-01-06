module Main (main) where

import DLPack
import GHC.Exts (IsList (..))
import Numeric.TBLIS
import Prelude hiding (toList)

toList1 :: IsTblisType a => DLTensor -> [a]
toList1 t = case tblisFromDLTensor t of
  Right t' -> toList (tblisToTypedTensor @1 t')
  Left e -> error (show e)

toList2 :: IsTblisType a => DLTensor -> [[a]]
toList2 t = case tblisFromDLTensor t of
  Right t' -> toList (tblisToTypedTensor @2 t')
  Left e -> error (show e)

toList3 :: IsTblisType a => DLTensor -> [[[a]]]
toList3 t = case tblisFromDLTensor t of
  Right t' -> toList (tblisToTypedTensor @3 t')
  Left e -> error (show e)

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
      c = [3, 2, 1]
  withDLTensor a $ \dlTensorA ->
    withDLTensor c $ \dlTensorB -> do
      tblisAdd (1 :: Double) dlTensorA "ij" (0 :: Double) dlTensorB "j"
      print (toList1 @Double dlTensorB)
  withDLTensor a $ \dlTensorA ->
    withDLTensor c $ \dlTensorB ->
      withDLTensor c $ \dlTensorC -> do
        tblisMult
          (1 :: Double)
          dlTensorA
          "ij"
          (1 :: Double)
          dlTensorB
          "j"
          (0 :: Double)
          dlTensorC
          "i"
        print (toList1 @Double dlTensorC)
  putStrLn "Hello world!"
