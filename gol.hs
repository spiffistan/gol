import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Set as S
import Control.Monad (replicateM)
import System.Random
import Control.Parallel
import Control.Parallel.Strategies

data World = World { width :: Double, height :: Double, pixWidth :: Int, pixHeight :: Int } deriving Show

type Cell = (Int, Int)
type CellSet = Set Cell

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)
  random gen =
    let ((x, y), gen') = random gen
    in ((x, y), gen')

grid_size = 400
cell_size = 1
seed_cells = 25000

neighbors :: Cell -> CellSet
neighbors (x,y) = fromList [ (x-1,y-1), (x,y-1), (x+1,y-1)
                           , (x-1,y  ),          (x+1,y  )
                           , (x-1,y+1), (x,y+1), (x+1,y+1) ]

stays_alive :: Int -> Bool
stays_alive n_count = n_count >= 2 && n_count <= 3

becomes_alive :: Int -> Bool
becomes_alive = (==) 3

count_alive_neighbors :: CellSet -> Cell -> Int
count_alive_neighbors living = size . intersection living . neighbors

dead_cells_around :: CellSet -> CellSet
dead_cells_around living = around living `difference` living
  where around = fold union empty . S.map neighbors

-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overcrowding.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

transition :: ViewPort -> Float -> CellSet -> CellSet
transition _ _ living = steady_life `par` new_life `par` steady_life `union` new_life
  where
    steady_life = S.filter (stays_alive . count) living
    new_life    = S.filter (becomes_alive . count) $ dead_cells_around living
    count       = count_alive_neighbors living
  
-- Glider:
--
--      X
--        X
--    X X X

glider :: CellSet 
glider = fromList [(0,1), (1,2), (2,0), (2,1), (2,2)]

randomWorld :: Int -> CellSet
randomWorld i = fromList $ (take seed_cells $ randomRs ((-grid_size,-grid_size),(grid_size,grid_size)) $ mkStdGen i :: [(Int,Int)])

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Rendering code

aliveColor = makeColor 1.0 1.0 1.0 1.0 -- white
deadColor = makeColor 0.2 0.2 0.2 1.0 -- gray

model_to_screen :: World -> (Double, Double) -> (Float, Float)
model_to_screen world (x,y) =
  let xscale = (fromIntegral (pixWidth world)) / (width world)
      yscale = (fromIntegral (pixHeight world)) / (height world)
  in
    (realToFrac $ x * xscale, realToFrac $ y * yscale)
 

render_grid :: World -> CellSet -> Picture
render_grid world grid = Pictures (Prelude.map (render_cell world) ((S.toList grid) `using` parList rdeepseq))

render_cell :: World -> Cell -> Picture
render_cell world (x,y) =
  let (x',y') = model_to_screen world (fromIntegral x, fromIntegral y)
  in Pictures $ [ Color aliveColor $ Translate x' y' $ rectangleSolid cell_size cell_size ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = 
  let w = World { width = 100, height = 100, pixWidth = 100, pixHeight = 100 }
  in
    do
      x <- getStdGen
      simulate (InWindow "Game of Life" (800, 800) (0, 0))
        (greyN 0.1)
        10
        (randomWorld $ fst $ (next x))
        (render_grid w)
        transition
    
