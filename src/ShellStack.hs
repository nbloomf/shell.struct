module Main where

import Data.List (lines, unlines, (\\))

import qualified System.IO.Strict as S
  ( readFile, getContents )
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory
  ( getHomeDirectory
  , createDirectoryIfMissing
  , getDirectoryContents
  , doesFileExist
  )



main :: IO ()
main = do
  args <- getArgs

  {- prepare data path -}
  home <- getHomeDirectory
  let dataDir = home ++ "/.shellstruct/stack"
  createDirectoryIfMissing True dataDir

  case args of
    {- default stack -}
    ("push":xs) -> stackPush (dataDir ++ "/stack") xs
    ["pop"]     -> stackPop  (dataDir ++ "/stack")
    ["peek"]    -> stackPeek (dataDir ++ "/stack")

    {- named stack -}
    (f:"push":xs) -> stackPush (dataDir ++ "/" ++ f) xs
    [f,"pop"]     -> stackPop  (dataDir ++ "/" ++ f)
    [f,"peek"]    -> stackPeek (dataDir ++ "/" ++ f)

    {- otherwise -}
    ["help"] -> showUsageAndQuit
    ["show"] -> printStacksAndQuit dataDir
    _        -> showUsageAndQuit

  exitSuccess


strictFileMap :: FilePath -> (String -> Maybe (a, String)) -> String -> IO a
strictFileMap path f msg = do
  x <- S.readFile path
  let y = f x
  case y of
    Just (a,z) -> do
      writeFile path z
      return a
    Nothing -> do
      hPutStrLn stderr msg
      exitFailure


checkFile :: FilePath -> IO ()
checkFile path = do
  test <- doesFileExist path
  if test
    then return ()
    else writeFile path ""

stackPush :: FilePath -> [String] -> IO ()
stackPush path [] = do
  checkFile path
  s <- S.readFile path
  x <- S.getContents
  let stack = push (encode x) (readStack s)
  writeFile path (writeStack stack)
  exitSuccess
stackPush path xs = do
  checkFile path
  s <- S.readFile path
  let stack = pushAll (map encode xs) (readStack s)
  writeFile path (writeStack stack)
  exitSuccess

stackPop :: FilePath -> IO ()
stackPop path = do
  checkFile path
  s <- S.readFile path
  let stack = readStack s
  case pop stack of
    Just (x,st) -> do
      putStrLn $ decode x
      writeFile path (writeStack st)
      exitSuccess
    Nothing -> do
      hPutStrLn stderr "Empty Stack!"
      exitFailure

stackPeek :: FilePath -> IO ()
stackPeek path = do
  checkFile path
  s <- readFile path
  let stack = readStack s
  case peek stack of
    Just x -> do
      putStrLn $ decode x
      exitSuccess
    Nothing -> do
      hPutStrLn stderr "Empty Stack!"
      exitFailure


showUsageAndQuit :: IO ()
showUsageAndQuit = do
  putStr $ unlines
    [ "Usage: ss-stack NAME? COMMAND"
    , "Manipulate named stacks of text at the command line."
    , "  help    Show usage"
    , "  show    Print list of stacks to stdout"
    , "  push    Push string arguments to stack"
    , "  pop     Remove top item from stack and print to stdout"
    , "  peek    Print top item from stack to stdout"
    --, "  print   Print stack to stdout (digest)"
    ]
  exitSuccess

printStacksAndQuit :: FilePath -> IO ()
printStacksAndQuit dir = do
  xs <- getDirectoryContents dir
  putStr $ unlines $ (xs \\ [".",".."])
  exitSuccess



{- stack data -}
data Stack
  = Stack [String]
  deriving (Eq, Show)

readStack :: String -> Stack
readStack str = Stack (lines str)

writeStack :: Stack -> String
writeStack (Stack xs) = unlines xs



{- stack operations -}
push :: String -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pushAll :: [String] -> Stack -> Stack
pushAll [] st = st
pushAll (x:xs) st = pushAll xs (push x st)

pop :: Stack -> Maybe (String, Stack)
pop (Stack [])     = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

peek :: Stack -> Maybe String
peek st = do
  (x,_) <- pop st
  return x



{- string conversions -}
encode :: String -> String
encode = concatMap encodeChar

encodeChar :: Char -> String
encodeChar x = case x of
  '\\' -> "\\\\"
  '\n' -> "\\n"
  c    -> [c]

decode :: String -> String
decode ""  = ""
decode [a] = [a]
decode (a:b:cs)
  | a == '\\' && b == '\\' = '\\' : decode cs
  | a == '\\' && b == 'n'  = '\n' : decode cs
  | otherwise              = a    : decode (b:cs)
