-- Datatype for arithmetic expressions
data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr

-- Datatype for the code of the virtual machine
data Code = HALT | PUSH Int Code | ADD Code

-- Datatype for the stack of the virtual machine
type Stack = [Int]

-- Execute code on the stack
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m : n : s) = exec c ((n + m) : s)

-- Compile an expression to code
comp :: Expr -> Code
comp expr = comp' expr HALT

-- Compile with a continuation (code)
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw _ = HALT  -- Ignore Throw during compilation
comp' (Catch x h) c = comp' x c  -- Ignore Catch during compilation

-- Test the compiler
testCompiler :: Expr -> Stack
testCompiler expr = exec (comp expr) []

-- Example usage:
main :: IO ()
main = do
  putStrLn "Test 1: Arithmetic expression without exceptions"
  print $ testCompiler (Add (Val 3) (Val 5))  -- Result: [8]

  putStrLn "\nTest 2: Catching an exception"
  print $ testCompiler (Catch Throw (Val 42))  -- Result: [42]

  putStrLn "\nTest 3: Throwing and catching an exception"
  print $ testCompiler (Catch Throw (Add (Val 2) (Val 3)))  -- Result: [5]
