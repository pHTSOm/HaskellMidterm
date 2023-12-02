-- Syntax and semantics
data Expr = Val Int | Add Expr Expr 

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- Define Combinators for Continuations:
type Stack = [Int]

type Cont = Stack -> Stack

haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . (n:)

addC :: Cont -> Cont
addC c = c . add
  where add (m:n:s) = (n + m) : s

-- Rewrite Evaluators Using Combinators:
eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c = pushC n c
eval'' (Add x y) c = eval'' x (eval'' y (addC c))

-- Define a New Type Code for Continuations:
data Code = HALT | PUSH Int Code | ADD Code
  deriving Show

-- Map Continuations to Code and Define exec:
-- exec :: Code -> Cont
-- exec HALT = haltC
-- exec (PUSH n c) = pushC n (exec c)
-- exec (ADD c) = addC (exec c)

-- Simplify exec Definition:
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

-- Final Compilation Functions Using Code:
comp :: Expr -> Code 
comp e = comp' e HALT 

comp' :: Expr -> Code -> Code 
comp' (Val n) c = PUSH n c 
comp' (Add x y) c = comp' x (comp' y (ADD c))


--Test
main :: IO ()
main = do
  let expression = Val 5 `Add` Val 7  -- Example expression: 5 + 7
  let initialStack = []  -- Initial stack
  let finalStack = exec (comp expression) initialStack
  putStrLn $ "Result: " ++ show finalStack
