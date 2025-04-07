import Data.Char (chr, ord)
import System.Environment (getArgs, getProgName)

main = do
  args <- getArgs
  progName <- getProgName
  if null args
    then putStrLn $ "Usage: " ++ progName ++ " <code>"
    else let (ios, _, _, _) = interpretAll ([], [], parse . tokenize . head $ args, newInterpreter) in run . reverse $ ios

run :: [IO ()] -> IO ()
run [] = return ()
run (io : rest) = do
  _ <- io
  run rest

data Interpreter = Interpreter {stack :: [Int], stack_ptr :: Int, skip :: Bool, rev :: Bool} deriving (Show)

newInterpreter :: Interpreter
newInterpreter = Interpreter {stack = replicate 256 0, stack_ptr = 0, skip = False, rev = False}

iLeft :: Interpreter -> Interpreter
iLeft Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp - 1, skip = sk, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iRight :: Interpreter -> Interpreter
iRight Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp + 1, skip = sk, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iAdd :: Interpreter -> Interpreter
iAdd Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = map (\(i, n) -> if i == sp then n + 1 else n) (enumerate st), stack_ptr = sp, skip = sk, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iDec :: Interpreter -> Interpreter
iDec Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = map (\(i, n) -> if i == sp then n - 1 else n) (enumerate st), stack_ptr = sp, skip = sk, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iPrint :: Interpreter -> (Interpreter, IO ())
iPrint i@Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then (i, putChar . chr . nth (stack_ptr i) . stack $ i)
    else (Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}, return ())

iIf :: Interpreter -> Interpreter
iIf Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = nth sp st > 0, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iFi :: Interpreter -> Interpreter
iFi Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = nth sp st <= 0, rev = rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

iRev :: Interpreter -> Interpreter
iRev Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = rv} =
  if not sk
    then Interpreter {stack = st, stack_ptr = sp, skip = sk, rev = not rv}
    else Interpreter {stack = st, stack_ptr = sp, skip = False, rev = rv}

interpret :: Interpreter -> Keyword -> (Interpreter, IO ())
interpret i kw =
  case kw of
    Main.Left -> (iLeft i, return ())
    Main.Right -> (iRight i, return ())
    Add -> (iAdd i, return ())
    Dec -> (iDec i, return ())
    Print -> iPrint i
    If -> (iIf i, return ())
    Fi -> (iFi i, return ())
    Rev -> (iRev i, return ())

interpretAll :: ([IO ()], [Keyword], [Keyword], Interpreter) -> ([IO ()], [Keyword], [Keyword], Interpreter)
interpretAll (ios, kws, [], i) = (ios, kws, [], i)
interpretAll (ios, old, kw : rest, i) =
  let (i', io) = interpret i kw
   in if not $ rev i'
        then interpretAll (io : ios, kw : old, rest, i')
        else interpretAll (io : ios, kw : rest, old, Interpreter {stack = stack i', stack_ptr = stack_ptr i', skip = skip i', rev = False})

newtype Tokens = Tokens [String] deriving (Show)

data Keyword = Left | Right | Add | Dec | Print | If | Fi | Rev deriving (Show)

parse :: Tokens -> [Keyword]
parse (Tokens []) = []
parse (Tokens tokens) =
  let k = case head tokens of
        "<" -> Main.Left
        ">" -> Main.Right
        "add" -> Add
        "dec" -> Dec
        "print" -> Print
        "if" -> If
        "fi" -> Fi
        "rev" -> Rev
        s -> error $ "Invalid token: " ++ s ++ "."
   in k : (parse . Tokens . tail $ tokens)

tokenize :: String -> Tokens
tokenize = Tokens . words

------------------------------------------------------------------------

nth :: Int -> [a] -> a
nth _ [] = error "Index out of bounds"
nth 0 (x : _) = x
nth i (x : rest) = nth (i - 1) rest

enumerate :: [a] -> [(Int, a)]
enumerate xs = let (_, res, _) = enumerateInner (reverse xs, [], length xs - 1) in res

enumerateInner :: ([a], [(Int, a)], Int) -> ([a], [(Int, a)], Int)
enumerateInner ([], ixs, -1) = ([], ixs, -1)
enumerateInner (x : rest, ixs, i) = enumerateInner (rest, (i, x) : ixs, i - 1)
