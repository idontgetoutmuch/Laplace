{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main (main) where

import Data.Array.Repa hiding ( map, (++) )
import qualified Data.Array.Repa as A
import RedBlackStencilDiag

import Text.PrettyPrint.HughesPJClass ( render, pPrint )
import PrettyPrint ()

import Diagrams.Prelude hiding ( render )
import qualified Diagrams.Prelude as P
import Diagrams.Coordinates ( (^&) )
import Diagrams.TwoD.Arrow
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine

import Data.List.Split ( chunksOf )

import Text.Printf ( printf )


dx :: Rational
dx = 1

n :: Int
n = 5

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

redBlack n = concat $
             take n $
             cycle [ take n $ cycle [1, -1]
                   , take n $ cycle [-1, 1]
                   ]

gridSq :: (Int, Double) -> Diagram B R2
gridSq (c, x) = text (printf "%2.2f" x) # scale 0.2 # fc white
                <> square 1 # lw 0 # fc (getColour c)

  where

    getColour x | x == -1 = black
    getColour x | x ==  1 = red
    getColour x           = white

grid :: Int -> [(Int,Double)] -> Diagram B R2
grid n vs = if aLen == sLen
            then result
            else error $ "Specified grid size " ++ show sLen ++
                         " Actual grid size "   ++ show aLen
  where
    aLen = length vs
    sLen = n * n
    result = vcat $
             map hcat $
             map (map gridSq) $
             chunksOf n vs

main = do
  p <- computeP $ bndMask arr
  v <- computeP $ bndVal arr
  t <- solveLaplace 1 1.0 p v arr

  ts <- mapM (\nSteps -> solveLaplace  nSteps 1.3 p v arr) [1,2,3,4,5,6,7,24,25,26,27]
  us <- mapM (\nSteps -> solveLaplace' nSteps 1.3 p v arr) [1,2,3,4,5,6,7,24,25,26,27]

  let displayGrid ts fn =

        mainRender (DiagramOpts (Just 900) (Just 600) fn
                   , DiagramLoopOpts False Nothing 0)
        (vcat
         [ hcat [ grid (n+1) (zip (redBlack (n+1)) (toList v))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!0)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!1)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!2)))
                ]
         , strutY 1.0
         , hcat [ grid (n+1) (zip (redBlack (n+1)) (toList (ts!!3)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!4)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!5)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!6)))
                ]
         , strutY 1.0
         , hcat [ grid (n+1) (zip (redBlack (n+1)) (toList (ts!!7)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!8)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!9)))
                , strutX 1.0
                , grid (n+1) (zip (redBlack (n+1)) (toList (ts!!10)))
                ]
         ]
        )

  displayGrid ts "diagrams/example1.svg"
  displayGrid us "diagrams/example2.svg"

  diffs <- computeP $ A.zipWith (-) (ts!!10) (us!!10) :: IO (Array U DIM2 Double)

  putStrLn $ show diffs

  putStrLn "\nResults\n"
  putStrLn $ show n
  putStrLn $ render $ pPrint (t ! (Z :. (n `div` 2) :. (n `div` 2)))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int)  :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int ) :. n          ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. n          ))
