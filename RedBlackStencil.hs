{-# LANGUAGE BangPatterns, TemplateHaskell, QuasiQuotes #-}
module RedBlackStencil
   (solveLaplace)
where	
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Stencil.Dim2		as A
import qualified Data.Array.Repa.Shape		as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 

-- | Solver for the Laplace equation.
solveLaplace :: Monad m
                => Int			-- ^ Number of iterations to use.
                -> Double		-- ^ weight for over relaxing (>0.0 and <2.0).     -- new
                -> Array U DIM2 Double	-- ^ Boundary value mask.
                -> Array U DIM2 Double	-- ^ Boundary values.
                -> Array U DIM2 Double	-- ^ Initial state.
                -> m (Array U DIM2 Double)

solveLaplace !steps !omega !arrBoundMask !arrBoundValue !arrInit       -- new arg
 =  do
     rBM   <- computeP $ extractRed arrBoundMask
     bBM   <- computeP $ extractBlack arrBoundMask
     rBV   <- computeP $ extractRed arrBoundValue
     bBV   <- computeP $ extractBlack arrBoundValue
     rInit <- computeP $ extractRed arrInit
     bInit <- computeP $ extractBlack arrInit
     evenRows <- computeP $ traverse rInit id 
                                  (\ _ (e :. i :. j) -> even i) 
                        -- bool array of shape rInit indicating even rows    
     let relaxL r b = relaxLaplace omega r b rBM bBM rBV bBV evenRows
         go 0 !r !b = computeP $ combineRB r b
         go n !r !b 
           = do 
               (newR,newB) <- relaxL r b
               go (n - 1) newR newB
     go steps rInit bInit

relaxLaplace
	:: Monad m
    => Double
    -> Array U DIM2 Double	
	-> Array U DIM2 Double
    -> Array U DIM2 Double
    -> Array U DIM2 Double
    -> Array U DIM2 Double	
	-> Array U DIM2 Double	
    -> Array U DIM2 Bool	
	-> m (Array U DIM2 Double, Array U DIM2 Double)
relaxLaplace omega r b rBM bBM rBV bBV evenRows
         = do
             r' <- computeP
                   $ A.szipWith (+) rBV
                   $ A.szipWith (*) rBM
                   $ A.smap (/4)
                   $ altMapStencil2 (BoundConst 0) leftSt rightSt b evenRows
                   
             b' <- computeP
                   $ A.szipWith (+) bBV
                   $ A.szipWith (*) bBM
                   $ A.smap (/4)
                   $ altMapStencil2 (BoundConst 0) rightSt leftSt r' evenRows
                   -- Note use of r' rather than r to compute b'
    --         return (r', b')
    -- Omega Not yet working because of insufficient representation information
             r'' <- computeP
                       $ A.zipWith (+) (A.map (* (1- omega)) r)
                                        (A.map (* omega) (r'::Array U DIM2 Double))
             b'' <- computeP
                       $ A.zipWith (+) (A.map (* (1- omega)) b)
                                        (A.map (* omega) (b'::Array U DIM2 Double))
             return (r'' , b'')
     --      

combineRB :: Array U DIM2 Double -> Array U DIM2 Double -> Array D DIM2 Double
combineRB r b =     -- arr(i,j) 
                    --     | even(i+j) = r(i, j `div` 2)
                    --     | otherwise = b(i, j `div` 2)
                    -- arr has i <- 0..2n-1 , j <- 0..2m-1
                    -- r   has i <- 0..n-1 , j <- 0..m-1
                    -- b   has i <- 0..n-1 , j <- 0..m-1
            traverse2 r b
                     (\ (e :. i :. j) _ -> (e :. i :. 2*j))
                     (\ get1 get2 (e :. i :. j) -> 
                              (if even(i+j) then get1 else get2)
                                (e :. i :. j `div` 2))

extractRed :: Array U DIM2 Double -> Array D DIM2 Double
extractRed arr =     -- r(i,j) = arr(i, 2*j - (i `mod` 2))
                     -- arr has i <- 0..2n-1, j <- 0..2m-1
                     -- r   has i <- 0..n-1 , j <- 0..m-1
              traverse arr 
                      (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                      (\get (e :. i :. j) -> get (e :. i :. 2*j + (i `mod` 2)))

extractBlack :: Array U DIM2 Double -> Array D DIM2 Double
extractBlack arr =   -- b(i,j) = arr(i, 2*j - ((i+1) `mod` 2))
                     -- arr has i <- 0..2n-1, j <- 0..2m-1
                     -- b   has i <- 0..n-1 , j <- 0..m-1
             traverse arr 
                      (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                      (\get (e :. i :. j) -> get (e :. i :. 2*j + ((i+1) `mod` 2)))


{- altMapStencil2
  :: (Source r1 c, Source r2 Bool) =>
     Boundary c
     -> Stencil DIM2 c
     -> Stencil DIM2 c
     -> Array r1 DIM2 c
     -> Array r2 DIM2 Bool
     -> Array D DIM2 c 
-}                              
altMapStencil2 bd s1 s2 x bools
        -- maps stencils s1 or s2 indicated by bools array
        -- currently does both on all items and selects after
         = alt (mapStencil2 bd s1 x) (mapStencil2 bd s2 x) bools
           where alt l r bools
                   = A.zipWith selector (A.zipWith (,) l r) bools
                       where selector (m,n) True  = m
                             selector (m,n) False = n
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
