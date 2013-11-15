{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Array.Repa
import SolverOverR

dx, dy :: Rational
dx = 1
dy = 1 / 2

n, m :: Int
n = 5
m = 7

r, z, u :: Double
r = 5.0
z = 0.0
u = 1.0

arr = fromListUnboxed (Z :. n + 1 :. m + 1) (replicate ((n+1) * (m+1)) z)

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
      | iy == m
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
      | iy == m
      = z
      where x = fromRational $ (fromIntegral ix) * dx
    f get (Z :. ix :. iy)
      = u

checkConvergence :: Double -> Double -> Array U DIM2 Double -> Array D DIM2 Double
checkConvergence dx dy a = traverse a id f
  where
    f get (Z :. ix :. iy)
      | ix == 0
      = get (Z :. ix :. iy)
    f get (Z :. ix :. iy)
      | ix == n
      = get (Z :. ix :. iy)
    f get (Z :. ix :. iy)
      | iy == 0
      = get (Z :. ix :. iy)
    f get (Z :. ix :. iy)
      | iy == m
      = get (Z :. ix :. iy)
    f get (Z :. ix :. iy)
      = abs (v - u / denom)
      where
        beta = dx / dy
        denom = 2 * (1 + beta^2)
        v = get (Z :. ix :. iy)
        u =           get (Z :. (ix-1) :. iy)     + get (Z :. (ix+1) :. iy) +
            beta^2 * (get (Z :. ix     :. (iy-1)) + get (Z :. ix     :. (iy+1)))

main = do
  p <- computeP $ bndMask arr
  v <- computeP $ bndVal arr
  t <- solveLaplace 91 1.0 0.5 1.0 p v arr
  xs :: [Array U DIM1 Double] <- mapM (\i -> computeP $ slice t (Any :. i)) [0..m]
  mapM_ (putStrLn . show) xs
  c :: Array U DIM2 Double <- computeP $ checkConvergence (fromRational dx) (fromRational dy) t
  ys :: [Array U DIM1 Double] <- mapM (\i -> computeP $ slice c (Any :. i)) [0..m]
  mapM_ (putStrLn . show) ys
