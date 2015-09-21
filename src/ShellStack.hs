{- A basic command line program for manipulating stacks. -}
{- Author: Nathan Bloomfield                             -}

module Main where

import Data.List (lines, unlines, (\\))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory
  ( getHomeDirectory
  , createDirectoryIfMissing
  , getDirectoryContents
  , doesFileExist )
import qualified System.IO.Strict as S
  ( readFile, getContents )



main :: IO ()
main = do
  args <- getArgs

  {- prepare data path -}
  home <- getHomeDirectory
  let dataDir = home ++ "/.shellstruct/stack"
  createDirectoryIfMissing True dataDir
  let defaultStack = dataDir ++ "/stack"
  checkFile defaultStack

  case args of
    {- basics -}
    ["help"] -> showUsage >> exitSuccess
    ["show"] -> printStacksAndQuit dataDir

    {- default stack -}
    ("push":xs) -> stackPush  defaultStack xs
    ["pop"]     -> stackPop   defaultStack
    ["peek"]    -> stackPeek  defaultStack
    ["print"]   -> stackPrint defaultStack

    {- named stack -}
    (name:xs) -> do
      let namedStack = dataDir ++ "/" ++ name
      checkFile namedStack
      case xs of
        ("push":ys) -> stackPush  namedStack ys
        ["pop"]     -> stackPop   namedStack
        ["peek"]    -> stackPeek  namedStack
        ["print"]   -> stackPrint namedStack
        _           -> showUsage >> exitFailure

    {- otherwise -}
    _        -> showUsage >> exitFailure

  exitSuccess



{- IO stack operations -}
stackPush :: FilePath -> [String] -> IO ()
stackPush path [] = do
  x <- S.getContents
  let foo = fmap (mapSnd writeStack) . push (encode x) . readStack
  strictFileMap path foo "Cannot Push!"
stackPush path xs = do
  let foo = fmap (mapSnd writeStack) . pushAll (map encode xs) . readStack
  strictFileMap path foo "Cannot Push!"

stackPop :: FilePath -> IO ()
stackPop path = do
  let foo = fmap (mapSnd writeStack) . pop . readStack
  x <- strictFileMap path foo "Cannot Pop: Empty Stack!"
  putStrLn $ decode x

stackPeek :: FilePath -> IO ()
stackPeek path = do
  let foo = fmap (mapSnd writeStack) . peek . readStack
  x <- strictFileMap path foo "Cannot Peek: Empty Stack!"
  putStrLn $ decode x

stackPrint :: FilePath -> IO ()
stackPrint path = do
  let foo = fmap (mapSnd writeStack) . printS . readStack
  xs <- strictFileMap path foo "Cannot Print!"
  putStr $ unlines xs



{- IO utilities -}
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

showUsage :: IO ()
showUsage = do
  putStr $ unlines
    [ "Manipulate stacks of text at the command line."
    , "Example Usage:"
    , "  Default Stack"
    , "    zstack push \"o rly\" ya       # push one or more string args"
    , "    echo \"lol wut\" | zstack push # push from stdin"
    , "    zstack peek                  # write top element to stdout"
    , "    zstack print                 # write stack to stdout"
    , "    zstack pop                   # write&remove top element"
    , "  Named Stacks"
    , "    zstack foo push \"yup mmhmm\"  # etc"
    , "    zstack foo pop               # etc"
    , "    zstack show                  # write list of stack names to stdout"
    ]

printStacksAndQuit :: FilePath -> IO ()
printStacksAndQuit dir = do
  xs <- getDirectoryContents dir
  putStr $ unlines $ (xs \\ [".",".."])
  exitSuccess



{- misc utilities -}
mapSnd :: (b1 -> b2) -> (a,b1) -> (a,b2)
mapSnd f (x,y) = (x, f y)



{- stack data -}
data Stack
  = Stack [String]
  deriving (Eq, Show)

readStack :: String -> Stack
readStack str = Stack (lines str)

writeStack :: Stack -> String
writeStack (Stack xs) = unlines xs



{- stack operations -}
push :: String -> Stack -> Maybe ((), Stack)
push x (Stack xs) = Just ((), Stack (x:xs))

pushAll :: [String] -> Stack -> Maybe ((), Stack)
pushAll [] st = Just ((), st)
pushAll (x:xs) st = do
  ((), st') <- push x st
  pushAll xs st'

pop :: Stack -> Maybe (String, Stack)
pop (Stack [])     = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

peek :: Stack -> Maybe (String, Stack)
peek st = do
  (x,_) <- pop st
  return (x, st)

printS :: Stack -> Maybe ([String], Stack)
printS st@(Stack xs) = Just (xs, st)
  


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
