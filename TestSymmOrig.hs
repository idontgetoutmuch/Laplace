{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}

module Main (main) where

import Data.Array.Repa
import SolverGet

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

instance (Source t a, Pretty a) => Pretty (Array t DIM1 a) where
 pPrint a = brackets $ hcat $ punctuate (comma <> space) elems
   where
     elems = [ pPrint (a!j) | i <- [0..n-1], let j = Z:. i ]
     Z :. n = extent a

instance (Source t a, Pretty a) => Pretty (Array t DIM2 a) where
 pPrint a = vcat elems
   where
     elems = [ pPrint (slice a j) | i <- [0..n-1], let j = Any :. i :. All]
     Z :. n :. _m = extent a

dx :: Rational
dx = 1

n :: Int
n = 101

r, z, u :: Double
r = 5.0
z = 0.0
u = 1.0

arr = fromListUnboxed (Z :. n + 1 :. n + 1) (replicate ((n+1) * (n+1)) z)

bndVal a = traverse a id f
  where
    f get (Z :. ix :. iy)
      | ix == 0
      = r
    f get (Z :. ix :. iy)
      | ix == n
      = r
    f get (Z :. ix :. iy)
      | iy == 0
      = r * (1.0 + x * (x - 1.0))
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      | iy == n
      = r * (1.0 + x * (1.0 - x))
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      = get (Z :. ix :. iy)

bndMask a = traverse a id f
  where
    f get (Z :. ix :. iy)
      | ix == 0
      = z
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      | ix == n
      = z
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      | iy == 0
      = z
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      | iy == n
      = z
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      = u

main = do
  p <- computeP $ bndMask arr
  v <- computeP $ bndVal arr
  t <- solveLaplace 32000 p v arr
  putStrLn "\nResults\n"
  putStrLn $ show n
  putStrLn $ render $ pPrint (t ! (Z :. (n `div` 2) :. (n `div` 2)))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int)  :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int ) :. n          ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. n          ))

