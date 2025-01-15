-- Стековая вычислительная машина на языке Haskell

import Data.Char (isDigit)
import Text.Read (readMaybe)

-- Тип данных для команд

data Command a
  = Push a                  -- Поместить элемент на стек
  | Pop                     -- Удалить верхний элемент
  | Add                     -- Сложение двух верхних элементов
  | Sub                     -- Вычитание
  | Mul                     -- Умножение
  | Div                     -- Деление
  | Dup                     -- Дублирование верхнего элемента
  | Branch (Command a) (Command a) -- Ветвление
  | Print                   -- Вывод верхнего элемента
  deriving (Show, Eq)

-- Тип данных для состояния машины

type Stack a = [a]
type Program a = [Command a]

data MachineState a = MachineState
  { stack :: Stack a
  , output :: [String]
  } deriving (Show)

-- Тип для обработки ошибок

data MachineError
  = DivisionByZero
  | EmptyStack
  | InvalidCommand String
  deriving (Show, Eq)

-- Функция для выполнения команды

runCommand :: (Eq a, Show a, Fractional a) => Command a -> MachineState a -> Either MachineError (MachineState a)
runCommand (Push x) (MachineState st out) = Right $ MachineState (x : st) out
runCommand Pop (MachineState [] out) = Left EmptyStack
runCommand Pop (MachineState (_:xs) out) = Right $ MachineState xs out
runCommand Add (MachineState (x:y:xs) out) = Right $ MachineState ((x + y) : xs) out
runCommand Add (MachineState _ _) = Left EmptyStack
runCommand Sub (MachineState (x:y:xs) out) = Right $ MachineState ((y - x) : xs) out
runCommand Sub (MachineState _ _) = Left EmptyStack
runCommand Mul (MachineState (x:y:xs) out) = Right $ MachineState ((x * y) : xs) out
runCommand Mul (MachineState _ _) = Left EmptyStack
runCommand Div (MachineState (x:y:xs) out)
  | x == 0 = Left DivisionByZero
  | otherwise = Right $ MachineState ((y / x) : xs) out
runCommand Div (MachineState _ _) = Left EmptyStack
runCommand Dup (MachineState (x:xs) out) = Right $ MachineState (x:x:xs) out
runCommand Dup (MachineState _ _) = Left EmptyStack
runCommand (Branch cmd1 cmd2) state@(MachineState (x:xs) out)
  | x /= 0    = runCommand cmd1 (MachineState xs out)
  | otherwise = runCommand cmd2 (MachineState xs out)
runCommand (Branch _ _) _ = Left EmptyStack
runCommand Print (MachineState (x:xs) out) = Right $ MachineState xs (out ++ [show x])
runCommand Print (MachineState _ _) = Left EmptyStack

-- Функция для выполнения программы

runProgram :: (Eq a, Show a, Fractional a) => Program a -> MachineState a -> Either MachineError (MachineState a)
runProgram program initialState = foldl runCommandWrapper (Right initialState) program
  where
    runCommandWrapper (Left err) _ = Left err
    runCommandWrapper (Right state) cmd = runCommand cmd state

-- Пример использования

main :: IO ()
main = do
  let program = [Push 5, Push 3, Add, Print] -- Пример программы
  let initialState = MachineState [] []      -- Начальное состояние машины
  case runProgram program initialState of
    Left err -> putStrLn $ "Error: " ++ show err
    Right finalState -> do
      putStrLn "Final Stack:"
      print $ stack finalState
      putStrLn "Output:"
      mapM_ putStrLn $ output finalState
