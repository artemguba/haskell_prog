data Stack a = Empty | Node a (Stack a)

push :: a -> Stack a -> Stack a
push x Empty = Node x Empty
push x (Node y stack) = Node y (push x stack)

pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (Node x stack) = Just (x, stack)

instance Show a => Show (Stack a) where
    show Empty = "Empty"
    show (Node x stack) = show x ++ " : " ++ show stack

main :: IO ()
main = do
    let stack1 = push 1 (push 2 (push 3 Empty))
    print stack1
    case pop stack1 of
        Nothing -> putStrLn "Stack is empty"
        Just (x, newStack) -> do
            putStrLn $ "Popped: " ++ show x
            print newStack
