import System.Random
import System.IO.Unsafe
import Data.Foldable
import Data.Sequence

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 } deriving (Show)

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ Prelude.length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (Prelude.take r n) ++ (Prelude.drop (r+1) n)
             }

makeMaze w h = Maze (Prelude.take (w * h) (repeat (True, True))) w h

find sets a = if (index sets a) /= a then Main.find sets (index sets a)
              else index sets a

unite sets a b = let x = Main.find sets a
                     y = Main.find sets b
                 in update y x sets

createWalls maze i n = if i == n then []
                       else let h = if (i + 1) `mod` (width maze) /= 0 then [(i, i + 1)]
                                    else []
                                v = if i + (width maze) < n then [(i, i + (width maze))]
                                    else []
                            in h ++ v ++ createWalls maze (i + 1) n

removeWalls cellsSeq sets walls i = if Prelude.length walls == i then cellsSeq
                                 else if Main.find sets (fst (walls !! i)) == Main.find sets (snd (walls !! i)) then removeWalls cellsSeq sets walls (i + 1)
                                      else let united = unite sets (fst (walls !! i)) (snd (walls !! i))
                                               removed = if (fst (walls !! i)) + 1 == (snd (walls !! i)) then update (fst (walls !! i)) (False, snd (index cellsSeq (fst (walls !! i)))) cellsSeq
                                                         else update (fst (walls !! i)) (fst (index cellsSeq (fst (walls !! i))), False) cellsSeq
                                           in removeWalls removed united walls (i + 1)

kruskal maze = let walls = shuffle (createWalls maze 0 ((width maze) * (height maze)))
               in let sets = fromList [0..(width maze) * (height maze)]
                      cellsSeq = fromList (cells maze)
                  in Maze (toList (removeWalls cellsSeq sets walls 0)) (width maze) (height maze)

coor maze (x, y) = x * (width maze) + y
coorXY maze n = (n `div` (width maze), n `mod` (width maze))

wallBetween maze a b = if a == b then False
                       else if a > b then wallBetween maze b a
                            else if coor maze a + 1 == coor maze b then fst ((cells maze) !! (coor maze a))
                                 else snd ((cells maze) !! (coor maze a))

solveHelp maze s g p = if fst s < 0 || (height maze) <= fst s ||
                          snd s < 0 || (width maze) <= snd s ||
                          wallBetween maze s p then []
                       else let top = [s] ++ if (fst s - 1, snd s) == p then [] else solveHelp maze (fst s - 1, snd s) g s
                            in if last top == g then top
                               else let right = [s] ++ if (fst s, snd s + 1) == p then [] else solveHelp maze (fst s, snd s + 1) g s
                                    in if last right == g then right
                                    else let left = [s] ++ if (fst s + 1, snd s) == p then [] else solveHelp maze (fst s + 1, snd s) g s
                                         in if last left == g then left
                                         else [s] ++ if (fst s, snd s - 1) == p then [] else solveHelp maze (fst s, snd s - 1) g s

solvePerfect maze s g = solveHelp maze s g s

firstLine i n = if i == n then []
                else ['-', '+'] ++ firstLine (i + 1) n
cellLine maze i j path = if j == (width maze) then []
                         else (if elem (coorXY maze (i + j)) path then ['*'] else [' ']) ++ (if fst ((cells maze) !! (i + j)) then ['|'] else [' ']) ++ cellLine maze i (j + 1) path
wallLine maze i j = if j == (width maze) then []
                    else (if snd ((cells maze) !! (i + j)) then ['-'] else [' ']) ++ ['+'] ++ wallLine maze i (j + 1)
nextLine maze i path = if i == (width maze) * (height maze) then []
                       else ['|'] ++ cellLine maze i 0 path ++ ['\n'] ++ ['+'] ++ wallLine maze i 0 ++ ['\n'] ++ nextLine maze (i + (width maze)) path
showMaze maze path = ['+'] ++ firstLine 0 (width maze) ++ ['\n'] ++ nextLine maze 0 path
