{-# LANGUAGE BangPatterns #-}
module SolverOverR
	(solveLaplace)
where	
import Data.Array.Repa		        as R hiding ((++))
import Data.Array.Repa.Unsafe           as R
import qualified Data.Array.Repa.Shape	as S

-- | Solver for the Laplace equation.
solveLaplace :: Monad m
                => Int			-- ^ Number of iterations to use.
                -> Double               -- ^ Grid length in the x direction
                -> Double               -- ^ Grid length in the y direction
                -> Double		-- ^ weight for over relaxing
                                        --   (>0.0 and <2.0).
                -> Array U DIM2 Double	-- ^ Boundary value mask.
                -> Array U DIM2 Double	-- ^ Boundary values.
                -> Array U DIM2 Double	-- ^ Initial state.
                -> m (Array U DIM2 Double)

solveLaplace steps dx dy omega arrBoundMask arrBoundValue arrInit
 = go steps arrInit
 where	go !i !arr
	   | i == 0	
           = return     arr

	   | otherwise	
	   = do arr' <- relaxLaplace dx dy omega arrBoundMask arrBoundValue arr
                go (i - 1) arr'
{-# NOINLINE solveLaplace #-}


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
--  Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.

-- New computation function is
-- u'(i,j) = (1-omega) * u(i,j) + 
--           (omega/4) * (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))

-- 
relaxLaplace :: Monad m
                => Double -- ^ Grid length in the x direction
                -> Double -- ^ Grid length in the y direction
                -> Double -- ^ weight for over relaxing (>0.0 and <2.0)
                -> Array U DIM2 Double	-- ^ Boundary condition mask
                -> Array U DIM2 Double	-- ^ Boundary condition values
                -> Array U DIM2 Double	-- ^ Initial matrix
                -> m (Array U DIM2 Double)

relaxLaplace dx dy omega arrBoundMask arrBoundValue arr
  = computeP
  $ R.zipWith (+) arrBoundValue
  $ R.zipWith (*) arrBoundMask
  $ unsafeTraverse arr id elemFn
  where
	_ :. height :. width	
		= extent arr

        beta = dx / dy
        denom = 2 * (1 + beta^2)
        
	{-# INLINE elemFn #-}
	elemFn !get !d@(sh :. i :. j)
	 = if isBorder i j
		 then  get d
		 else {- get (sh :. i :. j) * (1 - omega) + -}
		      (           get (sh :. (i-1) :. j)     + get (sh :. (i+1) :. j)
                       + beta^2 *(get (sh :.     i :. (j-1)) + get (sh :.     i :. (j+1)))) {- * omega -} / denom
	

	-- Check if this element is on the border of the matrix.
	-- If so we can't apply the stencil because we don't have all the neighbours.
	{-# INLINE isBorder #-}
	isBorder !i !j
	 	=  (i == 0) || (i >= height {- width -}  - 1) 
	 	|| (j == 0) || (j >= width {- height -} - 1) 
{-# INLINE relaxLaplace #-}
