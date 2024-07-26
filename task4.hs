import Control.Monad.State.Lazy

data Stack a = Empty | Node a (Stack a) deriving (Show)

instance Eq a => Eq (Stack a) where
  Empty == Empty = True
  (Node x xs) == (Node y ys) = x == y && xs == ys

push :: a -> State (Stack a) ()
push x = state $ \s -> ((), Node x s)

pop :: State (Stack a) (Maybe (a, Stack a))
pop = state $ \s -> case s of
  Empty -> (Nothing, Empty)
  (Node x xs) -> (Just (x, xs), xs)

main :: IO ()
main = do
  let (_, stack1) = runState (push (1 :: Int)) Empty
  if stack1 == Node 1 Empty then
    putStrLn "Test 1: Passed"
  else
    putStrLn "Test 1: Failed"

  let (_, stack2) = runState (push (2 :: Int) >> push (1 :: Int)) Empty
  if stack2 == Node 2 (Node 1 Empty) then
    putStrLn "Test 2: Passed"
  else
    putStrLn "Test 2: Failed"

  let (_, stack3) = runState (push (3 :: Int) >> push (2 :: Int) >> push (1 :: Int)) Empty
  if stack3 == Node 3 (Node 2 (Node 1 Empty)) then
    putStrLn "Test 3: Passed"
  else
    putStrLn "Test 3: Failed"

  let (result, _) = runState pop (Node 1 Empty :: Stack Int)
  if result == Just (1, Empty) then
    putStrLn "Test 4: Passed"
  else
    putStrLn "Test 4: Failed"

  let (result, _) = runState pop Empty :: (Maybe (Int, Stack Int), Stack Int)
  if result == (Nothing, Empty) then
    putStrLn "Test 5: Passed"
  else
    putStrLn "Test 5: Failed"
