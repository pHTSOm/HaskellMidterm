-- Define the syntax of the source language
data Expr = Val Int | Add Expr Expr

-- Add a Show instance for Expr
instance Show Expr where
  show (Val n) = show n
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

-- Define the virtual machine code
data Code = HALT | PUSH Int Code | ADD Code

-- Define the stack
type Stack = [Int]

-- Compile expressions to code
comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

-- Execute code on the stack
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (n + m : s)

-- Evaluate expressions using the compiler and virtual machine
evalExpr :: Expr -> Stack
evalExpr e = exec (comp e) []

-- Example usage
main :: IO ()
main = do
  let exampleExpr = Add (Val 2) (Val 3)
  putStrLn $ "Expression: " ++ show exampleExpr
  putStrLn $ "Result: " ++ show (evalExpr exampleExpr)