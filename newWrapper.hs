{-# LANGUAGE BangPatterns, RankNTypes, FlexibleContexts #-}
-- | Solver for the Laplace equation
--	You supply a BMP files specifying the boundary conditions.
--	The output is written back to another BMP file.

-- Adapted from https://github.com/ghc/nofib/blob/master/fibon/Repa/Laplace/Main.hs
-- to pass in weightings to solver for over relaxing 
-- (New bits indicated)

import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.Algorithms.ColorRamp
import Data.Array.Repa.IO.BMP	
import Data.Array.Repa.IO.Timing
import System.Environment
import Data.Word
import Control.Monad
import RedBlackStencil          as RB                                      -- new
import Data.Array.Repa          as R
import Prelude                  as P

type Solver m 
	=  Monad m
    => Int			-- ^ Number of iterations to use.
    -> Double		-- ^ weight for over relaxing (>= 0.0).                -- new
	-> Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> m (Array U DIM2 Double)

solvers                                                                     -- new
 = 	[  ("redblack", 	RB.solveLaplace)
    ]                                 

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [strSolver, steps, omega, fileInput, fileOutput]	                  -- new arg
	    -> 	let Just solver	= lookup strSolver solvers
	  	in  laplace solver (read steps) (read omega) fileInput fileOutput -- new arg

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage	:: String
usage	= unlines
	[ "Usage: laplace <solver> <iterations> <weighting> <input.bmp> <output.bmp>"
	, ""
	, "  iterations  :: Int    Number of iterations to use in the solver."
	, "  weighting  :: Double      Weighting for overrelaxing between 0.0 and 2.0"  -- new
	, "  input.bmp   :: FileName  Uncompressed RGB24 or RGBA32 BMP file for initial and boundary values."
	, "  output.bmp  :: FileName  BMP file to write output to."
	, "" 
	, "  solver = one of " P.++ show (P.map fst solvers)
	, ""
	, "  Format of input file:"
	, "      Boundary values are indicated in greyscale,"
	, "        ie from the list [(x, x, x) | x <- [0 .. 255]]"
	, "      Non-boundary values are indicated in blue,"
	, "        ie (0, 0, 255)"
	, "      Any other pixel value is an error." 
	, ""
	, "  NOTE: For GHC 7.0.3, this runs better when you turn off the parallel"
	, "        garbage collector. Run with +RTS -qg"
	, "" ]
			

-- | Solve it.
laplace :: Solver IO
	-> Int			-- ^ Number of iterations to use.
	-> Double       -- ^ weighting                                         -- new
	-> FilePath 	-- ^ Input file.
	-> FilePath		-- ^ Output file
	-> IO ()

laplace solve steps omega fileInput fileOutput                            -- new arg
 = do
	-- Load up the file containing boundary conditions.
	arrImage	<- liftM (either (error . show) id)
			$  readImageFromBMP fileInput

	arrBoundValue   <- computeP $ R.map slurpBoundValue arrImage
	arrBoundMask	<- computeP $ R.map slurpBoundMask  arrImage
	let arrInitial	= arrBoundValue		
	
	-- Run the Laplace solver and print how long it took.
	(arrFinal, t)   <- time $ solve steps omega arrBoundMask arrBoundValue arrInitial 
	                                                                         -- new arg
	putStrLn ("omega = " P.++ show omega)                                    -- new
	putStr (prettyTime t)

	-- Write out the result to a file.
	arrImageOut     <- computeP
	                $  R.map rgb8OfDouble
	                $  R.map (rampColorHotToCold 0.0 1.0) arrFinal

	writeImageToBMP	fileOutput arrImageOut
	


-- | Extract the boundary value from a RGB triple.
slurpBoundValue :: (Word8, Word8, Word8) -> Double
{-# INLINE slurpBoundValue #-}
slurpBoundValue (!r, !g, !b)
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 0

	-- A boundary value.
	| (r == g) && (r == b) 
	= fromIntegral (fromIntegral r :: Int) / 255
	
	| otherwise
	= error $ "Unhandled pixel value in input " P.++ show (r, g, b)


-- | Extract boundary mask from a RGB triple.
slurpBoundMask :: (Word8, Word8, Word8) -> Double
{-# INLINE slurpBoundMask #-}
slurpBoundMask (!r, !g, !b)
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 1

	-- A boundary value.
	| (r == g) && (r == b) 
	= 0
	
	| otherwise
	= error $ "Unhandled pixel value in input " P.++ show (r, g, b)
