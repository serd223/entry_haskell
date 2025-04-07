import Data.Char (chr, ord)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)

main = do
  args <- getArgs
  progName <- getProgName
  if null args
    then putStrLn $ "Usage: " ++ progName ++ " <code>"
    else run . parse . head $ args

run :: [Keyword] -> IO ()
run kws = runInner ([], [], kws, newInterpreter)

runInner :: ([IO ()], [Keyword], [Keyword], Interpreter) -> IO ()
runInner d = case interpretAll $ Intermediate d of
  Intermediate d' -> runInner d'
  Pause (ioc, ios, old, kws, Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d}) -> do
    unwrapIo $ reverse ios
    hFlush stdout
    l <- repromptUntilInput ioc
    if null l
      then return ()
      else runInner ([], old, kws, Interpreter {stack = setNth sp (ord $ head l) st, stack_ptr = sp, skip = sk, rev = rv, dir = d})
  Quit ios -> unwrapIo $ reverse ios

repromptUntilInput :: IO [Char] -> IO [Char]
repromptUntilInput ioc = do
  l <- ioc
  if null l
    then repromptUntilInput ioc
    else return l

unwrapIo :: [IO ()] -> IO ()
unwrapIo [] = return ()
unwrapIo (io : rest) = do
  _ <- io
  unwrapIo rest

stackSize :: Int
stackSize = 256

data Interpreter = Interpreter {stack :: [Int], stack_ptr :: Int, skip :: Bool, rev :: Bool, dir :: Int} deriving (Show)

newInterpreter :: Interpreter
newInterpreter = Interpreter {stack = replicate stackSize 0, stack_ptr = 0, skip = False, rev = False, dir = 1}

nudgeStackPtr :: Int -> Int -> Int
nudgeStackPtr d sp
  | d > 0 = if sp + d >= stackSize then 0 else sp + d
  | sp + d < 0 = stackSize - 1
  | otherwise = sp + d

iLeft :: Interpreter -> Interpreter
iLeft Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = st, stack_ptr = nudgeStackPtr (negate d) sp, skip = sk, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iRight :: Interpreter -> Interpreter
iRight Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = st, stack_ptr = nudgeStackPtr d sp, skip = sk, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iAdd :: Interpreter -> Interpreter
iAdd Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = mapNth sp (+ 1) st, stack_ptr = sp, skip = sk, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iDec :: Interpreter -> Interpreter
iDec Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = mapNth sp (subtract 1) st, stack_ptr = sp, skip = sk, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iPrint :: Interpreter -> (Interpreter, IO ())
iPrint i@Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then (i, putChar . chr . nth (stack_ptr i) . stack $ i)
    else (Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}, return ())

iIf :: Interpreter -> Interpreter
iIf Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = nth sp st > 0, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iFi :: Interpreter -> Interpreter
iFi Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = nth sp st <= 0, rev = rv, dir = d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

iRev :: Interpreter -> Interpreter
iRev Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv, dir = d} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = not rv, dir = negate d}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv, dir = d}

data CycleResult = Continue (Interpreter, IO ()) | Request (IO [Char])

interpret :: Interpreter -> Keyword -> CycleResult
interpret i kw =
  case kw of
    Main.Left -> Continue (iLeft i, return ())
    Main.Right -> Continue (iRight i, return ())
    Add -> Continue (iAdd i, return ())
    Dec -> Continue (iDec i, return ())
    Print -> Continue $ iPrint i
    If -> Continue (iIf i, return ())
    Fi -> Continue (iFi i, return ())
    Rev -> Continue (iRev i, return ())
    Input -> Request getLine

data InterpretResult = Intermediate ([IO ()], [Keyword], [Keyword], Interpreter) | Pause (IO [Char], [IO ()], [Keyword], [Keyword], Interpreter) | Quit [IO ()]

interpretAll :: InterpretResult -> InterpretResult
interpretAll (Intermediate (ios, kws, [], i)) = Quit ios
interpretAll (Intermediate (ios, old, kw : rest, i)) = case interpret i kw of
  Continue (i', io) ->
    if not $ rev i'
      then interpretAll (Intermediate (io : ios, kw : old, rest, i'))
      else interpretAll (Intermediate (io : ios, kw : rest, old, Interpreter {stack = stack i', stack_ptr = stack_ptr i', skip = skip i', rev = False, dir = dir i'}))
  Request ioc -> Pause (ioc, ios, kw : old, rest, i)

data Keyword = Left | Right | Add | Dec | Print | If | Fi | Rev | Input deriving (Show)

parse :: String -> [Keyword]
parse [] = []
parse tokens =
  let kw
        | startsWith "<" tokens = Just Main.Left
        | startsWith ">" tokens = Just Main.Right
        | startsWith "add" tokens = Just Add
        | startsWith "dec" tokens = Just Dec
        | startsWith "print" tokens = Just Print
        | startsWith "if" tokens = Just If
        | startsWith "fi" tokens = Just Fi
        | startsWith "rev" tokens = Just Rev
        | startsWith "input" tokens = Just Input
        | otherwise = Nothing
   in case kw of
        Just k -> k : parse (tail tokens)
        Nothing -> parse (tail tokens)

------------------------------------------------------------------------

nth :: Int -> [a] -> a
nth _ [] = error "Index out of bounds"
nth 0 (x : _) = x
nth i (x : rest) = nth (i - 1) rest

setNth :: Int -> a -> [a] -> [a]
setNth idx x = mapNth idx (const x)

mapNth :: Int -> (a -> a) -> [a] -> [a]
mapNth idx f = zipWith (\i n -> (if i == idx then f n else n)) [0 ..]

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith prefix l = (length prefix <= length l) && and (zipWith (==) prefix l)
