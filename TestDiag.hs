{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main (main) where

import Data.Array.Repa hiding ( map, (++) )
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

gridSq ec = case ec of
  Left  c ->            square 1 # lw 0 # fc (getColour c)
  Right c -> (arr c) <> square 1 # lw 0 # fc (getColour c)
  where
    arr c = arrowBetween' (with & arrowHead .~ spike & arrowTail .~ quill) (sPt c) (nPt c)
            # centerXY
    sPt x | x == -1 = p2 (0.50, 0.70)
    sPt x | x ==  1 = p2 (0.50, 0.10)
    sPt x           = error $ "Spins can be up or down: " ++ show x

    nPt x | x == -1 = p2 (0.50, 0.10)
    nPt x | x ==  1 = p2 (0.50, 0.70)
    nPt x           =  error $ "Spins can be up or down: " ++ show x

    getColour x | x == -1 = saddlebrown
    getColour x | x ==  1 = antiquewhite
    getColour x           = white

isingGrid :: P.Renderable (P.Path R2) b => Int -> [Int] -> Diagram b R2
isingGrid n vs = if aLen == sLen
                 then result
                 else error $ "Specified grid size " ++ show sLen ++
                              " Actual grid size "   ++ show aLen
  where
    aLen = length vs
    sLen = n * n
    result = vcat $
             map hcat $
             map (map gridSq) $
             map (map Right) $
             chunksOf n vs

main = do
  p <- computeP $ bndMask arr
  v <- computeP $ bndVal arr
  t <- solveLaplace 100 1.0 p v arr

  mainRender (DiagramOpts (Just 500) (Just 500) "diagrams/example1.svg"
             , DiagramLoopOpts False Nothing 0)
             (isingGrid 3 (replicate (3 * 3) 1) :: Diagram B R2)

  putStrLn "\nResults\n"
  putStrLn $ show n
  putStrLn $ render $ pPrint (t ! (Z :. (n `div` 2) :. (n `div` 2)))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int)  :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. (0 :: Int ) :. n          ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. (0 :: Int) ))
  putStrLn $ render $ pPrint (t ! (Z :. n           :. n          ))

