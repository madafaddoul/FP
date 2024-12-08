-- func_stack_test.hs
import FuncStack
import Data.Time

-- Test the functional stack implementation
main :: IO ()
main = do
    start <- getCurrentTime
    
    let stack1 = emptyStack
    let stack2 = push 10 stack1
    let stack3 = push 20 stack2
    print $ "Stack after pushing 10 and 20: " ++ show stack3
    print $ "Top element (peek): " ++ show (peek stack3)
    let (poppedElement, stack4) = pop stack3
    print $ "Popped element: " ++ show poppedElement
    print $ "Stack after popping: " ++ show stack4
    
    end <- getCurrentTime
    let diff = diffUTCTime end start
    print $ "Time taken: " ++ show diff