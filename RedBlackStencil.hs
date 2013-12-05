{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module RedBlackStencil
   (solveLaplace)
where	
import Data.Array.Repa                 as A
import Data.Array.Repa.Stencil         as A
import Data.Array.Repa.Stencil.Dim2    as A
 

-- | Solver for the Laplace equation.
solveLaplace :: Monad m
                => Int			-- ^ Number of iterations to use.
                -> Double             	-- ^ weight for over relaxing (>0.0 and <2.0).
                -> Array U DIM2 Double	-- ^ Boundary value mask.
                -> Array U DIM2 Double	-- ^ Boundary values.
                -> Array U DIM2 Double	-- ^ Initial state. Should
                                        -- have even number of columns
                -> m (Array U DIM2 Double)

solveLaplace !steps !omega !arrBoundMask !arrBoundValue !arrInit
 =  do rBM   <- computeP $ extractRed arrBoundMask
       bBM   <- computeP $ extractBlack arrBoundMask
       rBV   <- computeP $ extractRed arrBoundValue
       bBV   <- computeP $ extractBlack arrBoundValue
       rInit <- computeP $ extractRed arrInit
       bInit <- computeP $ extractBlack arrInit
       let go 0 !r !b = computeP $ combineRB r b -- return final combined array
           go n !r !b 
            = do (newR,newB) <- relaxLaplace r b
                 go (n - 1) newR newB
           relaxLaplace !r !b
            = do
                 r' <- computeP
                       $ A.szipWith (+) (rBV:: Array U DIM2 Double)
                       $ A.szipWith (*) (rBM:: Array U DIM2 Double)
                       $ A.szipWith (overRSum omega) r
                       $ A.smap (/4)
                       $ altMapStencil2 (BoundConst 0) leftSt rightSt b
                 b' <- computeP
                       $ A.szipWith (+) (bBV:: Array U DIM2 Double)
                       $ A.szipWith (*) (bBM:: Array U DIM2 Double)
                       $ A.szipWith (overRSum omega) b
                       $ A.smap (/4)
                       $ altMapStencil2 (BoundConst 0) rightSt leftSt r'
                       -- Note use of r' to compute b'
                 return (r' , b')     
       go steps rInit bInit
 
overRSum :: Double -> Double -> Double -> Double
overRSum omega old new = (1-omega)*old + omega*new

combineRB :: Array U DIM2 Double -> Array U DIM2 Double -> Array D DIM2 Double
combineRB r b =     -- arr(i,j) 
                    --     | even(i+j) = r(i, j `div` 2)
                    --     | otherwise = b(i, j `div` 2)
                    -- arr has i <- 0..n-1 , j <- 0..2m-1
                    -- r   has i <- 0..n-1 , j <- 0..m-1
                    -- b   has i <- 0..n-1 , j <- 0..m-1
            traverse2 r b
                     (\ (e :. i :. j) _ -> (e :. i :. 2*j))
                     (\ get1 get2 (e :. i :. j) -> 
                              (if even(i+j) then get1 else get2)
                                (e :. i :. j `div` 2))
                                
{-# INLINE combineRB #-}

extractRed :: Array U DIM2 Double -> Array D DIM2 Double
extractRed arr =  
    -- Expects even number of columns for arr
                     -- r(i,j) = arr(i, 2*j + (i `mod` 2))
                     -- arr has i <- 0..n-1, j <- 0..2m-1
                     -- r   has i <- 0..n-1 , j <- 0..m-1
              traverse arr 
                      (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                      (\get (e :. i :. j) -> get (e :. i :. 2*j + (i `mod` 2)))

{-# INLINE extractRed #-}

extractBlack :: Array U DIM2 Double -> Array D DIM2 Double
extractBlack arr =  
    -- Expects even number of columns for arr
                     -- b(i,j) = arr(i, 2*j + ((i+1) `mod` 2))
                     -- arr has i <- 0..n-1, j <- 0..2m-1
                     -- b   has i <- 0..n-1 , j <- 0..m-1
             traverse arr 
                      (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                      (\get (e :. i :. j) -> get (e :. i :. 2*j + ((i+1) `mod` 2)))

{-# INLINE extractBlack #-}

altMapStencil2
      :: Boundary Double
      -> Stencil DIM2 Double
      -> Stencil DIM2 Double
      -> Array U DIM2 Double
      -> Array D DIM2 Double                   
altMapStencil2 !bd !s1 !s2 !arr
        -- Maps stencil s1 on even rows and s2 on odd rows of arr.
        -- Currently does both on all indices and selects after.
        -- This may be ok if laziness + inlining removes redundant computation
         = traverse2 (mapStencil2 bd s1 arr) (mapStencil2 bd s2 arr)
                     (\ e _ -> e)
                     (\ get1 get2 (e :. i :. j) -> 
                            (if even i then get1 else get2) (e :. i :. j)
                     )
                     
{-# INLINE altMapStencil2 #-}

-- Stencils

leftSt :: Stencil DIM2 Double -- odd rows from b, even from r
leftSt  =   [stencil2|  0 1 0         
                        1 1 0 
                        0 1 0 |]

rightSt :: Stencil DIM2 Double -- even rows from b, odd from r
rightSt =   [stencil2|  0 1 0 
                        0 1 1 
                        0 1 0 |]

{-# NOINLINE solveLaplace #-}
