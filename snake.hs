--SET PATH=%PATH%;%ProgramFiles%\Haskell Platform\8.4.3\bin;%ProgramFiles%\Haskell Platform\8.4.3\lib\extralibs\bin
--cabal update
--cabal install ansi-terminal
--cabal install base
--cd D:\OneDrive\Documents\Projects\ELTE\agda\
--ghci -cpp -threaded snake.hs
--ghc -cpp -threaded -o snakehs snake.hs

--translate to Agda to prove logic
--translate that back into Haskell and show that it runs
--use Agda to prove screen
--translate the proof back into Haskell and integrate it into the
--  original Haskell snake code as a runtime verification library
--use Agda to prove timing - difficult to reason about as besides threadDelay,
--  only the time of the code itself and how could Agda introspect its own proof checking?
--use Agda to prove randomness - obviously this is not feasible as its a statistical problem
--  requiring a one-way function to prove a true PRNG

--Agda can check 2 things: if it only monitors inputs (time elapsed, key presses, RNG seed)
--  it can generate a runtime verifier for the game logic state World or it can verify the output
--if it monitors only the output, it can still verify the game logic as well
--  perhaps also any inputs that lead up to this current output could be verified...

--import System.Console.ANSI
--import Control.Monad
--console API works in GHCI
--  but does not work in WinGHCI as it does not have proper console API access
import qualified System.Random as R (randomR, mkStdGen, StdGen)
import System.IO (hIsTerminalDevice, hSetEcho, hSetBuffering,
  BufferMode (NoBuffering), stdin, stdout, putStr)
--import System.Info (os)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if defined(mingw32_HOST_OS)
import System.Process (system)
import System.Win32.Console.CtrlHandler (CtrlEvent, c_SetConsoleCtrlHandler,
  mkHandler, PHANDLER_ROUTINE)
import System.Win32.Types (BOOL)
import Data.Char (chr)
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (freeHaskellFunPtr, nullFunPtr)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Concurrent (myThreadId, ThreadId)
import qualified Control.Exception as E (throwTo)
--import System.IO.Unsafe
getCh = do
  isTerm <- hIsTerminalDevice stdin
  if isTerm then fmap (chr.fromEnum) c_getch else getChar
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
chkKB = fmap (0 /=) c_kbhit
foreign import ccall unsafe "conio.h kbhit"
  c_kbhit :: IO CInt
--System.Info.os == "mingw32" && System.Info.arch == "x86_64" && System.Info.compilerName == "ghc"
-- && Data.Version.showVersion System.Info.compilerVersion == "8.4"
--if System.Info.os == "mingw32" then do { system " "; return () } else return ()
--all of the non-import dependency portions could be runtime decided
--  instead of conditionally compiled...
handler :: ThreadId -> IORef PHANDLER_ROUTINE -> CtrlEvent -> IO BOOL
handler tid ref _ = do
  -- (fst $ limits initWorld)
  putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
  setConsole True ref
  E.throwTo tid (ExitFailure (-1))
  return False

initConsole = do
  tid <- myThreadId
  ref <- newIORef nullFunPtr
  r <- mkHandler (handler tid ref)
  writeIORef ref r
  setConsole False ref
  return ref

setConsole bRestore ref = do
  hSetEcho stdin bRestore
  if not bRestore then do { system " "; return () } else return ()
  h <- readIORef ref
  --putStrLn (show h)
  c_SetConsoleCtrlHandler h (not bRestore)
  if bRestore then freeHaskellFunPtr h else return ()
  --this is problematic as its a unique pointer and must be freed
  --  library is defective due to global variable necessity

cASE_ARROWKEYS = '\xe0'
cASE_ARROWKEYS_PRE = do return True
uPARROW = 'H'
dOWNARROW = 'P'
rIGHTARROW = 'M'
lEFTARROW = 'K'
#else
--sudo apt install ghc
--sudo apt install cabal-install
--cabal install random
--cabal install select
import System.Posix.IO (stdInput)
import System.Posix.IO.Select (select'')
import System.Posix.IO.Select.Types (finite)
import System.Posix.Signals (installHandler, Handler(Catch, Default), sigINT, sigTERM)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Concurrent (myThreadId, ThreadId)
import qualified Control.Exception as E (throwTo)
getCh = getChar
chkKB = fmap (== 1) (select'' [stdInput] [] [] (finite 0 0))
initConsole = do
  setConsole False False
  return False

setConsole bRestore _ = do
  hSetEcho stdin bRestore
  tid <- myThreadId
  installHandler sigINT (if bRestore then Default else (Catch (do (handler tid)))) Nothing
  installHandler sigTERM (if bRestore then Default else (Catch (do (handler tid)))) Nothing
  where
    handler :: ThreadId -> IO ()
    handler tid = do
      -- (fst $ limits initWorld)
      putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
      setConsole True False
      E.throwTo tid (ExitFailure (-1))
      --exitWith (ExitFailure (-1))
      --exitWith should be called from the main program thread in order to exit the process
cASE_ARROWKEYS = '\x1b'
cASE_ARROWKEYS_PRE = fmap (== '[') getCh
uPARROW = 'A'
dOWNARROW = 'B'
rIGHTARROW = 'C'
lEFTARROW = 'D'
#endif

eSCVT100 :: String
eSCVT100 = "\x1b[" --check infocmp vt100 and infocmp -Cr vt100
gET_CURSOR_POS :: String
gET_CURSOR_POS = eSCVT100 ++ "6n"
cLEAR_SCREEN :: String
cLEAR_SCREEN = eSCVT100 ++ "2J"
sET_CURSOR :: Int -> Int -> String
sET_CURSOR x y = eSCVT100 ++ show x ++ ";" ++ show y ++ "H"
sET_MAX_CURSOR :: String
sET_MAX_CURSOR = sET_CURSOR 999 999
cLR_BLACK  = 0
cLR_RED = 1
cLR_GREEN = 2
cLR_YELLOW = 3
cLR_BLUE = 4
cLR_MAGENTA = 5
cLR_CYAN = 6
cLR_WHITE = 7
sET_BKGND_COLOR :: Integer -> String
sET_BKGND_COLOR x = eSCVT100 ++ "4" ++ show x ++ "m"
hIDE_CURSOR :: String
hIDE_CURSOR = eSCVT100 ++ "?25l"
sHOW_CURSOR :: String
sHOW_CURSOR = eSCVT100 ++ "?25h"

data Direction = North
               | South
               | East
               | West
               | None
               deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data World = World { snake :: Snake
                   , food :: Position
                   , lastFood :: Int
                   , direction :: Direction
                   , rand :: R.StdGen
                   , limits :: (Int, Int)
                   } deriving (Show)
                   
data GameState = Playing World
               | GameOver
               deriving (Show)

                   
showChars :: IO ()
showChars = do
  isTerm <- hIsTerminalDevice stdin
  c <- if isTerm then getCh else getChar --do not use low level keyboard calls otherwise
  putChar c
  if c /= 'x' then showChars else threadDelay 100000

drawBorder :: World -> IO ()
drawBorder w = do
  let (r, c) = limits w
  mapM_ whiteOut [(1, x) | x <- [1..c]]
  mapM_ whiteOut [(r, x) | x <- [1..c]]
  mapM_ whiteOut [(x, 1) | x <- [2..r-1]]
  mapM_ whiteOut [(x, c) | x <- [2..r-1]]
  let (sr, sc) = head (snake w)
  putStr ((sET_CURSOR sr sc) ++ (sET_BKGND_COLOR cLR_GREEN) ++ " ")
  where
    whiteOut (row, col) = do 
      putStr ((sET_CURSOR row col) ++ (sET_BKGND_COLOR cLR_WHITE) ++ " ")

mainLoop w = do
  isTerm <- hIsTerminalDevice stdin
  is <- if isTerm then chkKB else return True
  (newDir, exit) <- if is then do
    ch <- getCh
    if ch == 'x' then return (None, True)
    else if ch == cASE_ARROWKEYS then do
      nxt <- cASE_ARROWKEYS_PRE
      if nxt then do
        fmap (\cn -> if cn == uPARROW then (North, False)
        else if cn == dOWNARROW then (South, False)
        else if cn == rIGHTARROW then (West, False)
        else if cn == lEFTARROW then (East, False)
        else (None, False)) getCh
      else return (None, False)
    else return (None, False)
  else do { threadDelay 100000; return (None, False) } --microseconds
  case exit of
    True -> return ()
    otherwise -> do
      let dir = direction w
      let nextW = w { direction = case newDir of
          North -> if dir /= South || length (snake w) == 1 then newDir else dir
          South -> if dir /= North || length (snake w) == 1 then newDir else dir
          East -> if dir /= West || length (snake w) == 1 then newDir else dir
          West -> if dir /= East || length (snake w) == 1 then newDir else dir
          None -> dir }
      case (direction nextW) of
        None -> mainLoop w
        otherwise -> do
        let foodW = nextW { lastFood = lastFood nextW + 1 }
        fW <- if lastFood foodW == 50 && food foodW == (-1, -1) then do
          let genW = genFood foodW
          let foodloc = food genW
          putStr (hIDE_CURSOR ++ (sET_CURSOR (fst foodloc) (snd foodloc)) ++
            (sET_BKGND_COLOR cLR_YELLOW) ++ " ")
          return genW
        else return foodW
        let (sr, sc) = head (snake fW)
        let upW = fW { snake = case (direction fW) of
            North -> (sr - 1, sc)
            South -> (sr + 1, sc)
            East -> (sr, sc - 1)
            West -> (sr, sc + 1)
          :snake fW}
        let (srup, scup) = head (snake upW)
        let foodloc = food upW
        reduceW <- if srup == fst foodloc && scup == snd foodloc then
          return upW { lastFood = 0, food = (-1, -1) }
        else do
          let (oldsr, oldsc) = last (snake upW)
          putStr (hIDE_CURSOR ++ (sET_CURSOR oldsr oldsc) ++ (sET_BKGND_COLOR cLR_BLACK) ++ " ")
          return upW { snake = if srup == fst foodloc && scup == snd foodloc then
            snake upW else init (snake upW) }
        --snake ran into wall or itself
        let over = if (srup == 1 || srup == (fst $ limits reduceW) ||
             scup == 1 || scup == (snd $ limits reduceW) ||
             (any (\(x, y) -> x == srup && y == scup) (tail (snake reduceW)))) then 2 else 0
        putStr ((sET_CURSOR srup scup) ++ (sET_BKGND_COLOR cLR_GREEN) ++
          " " ++ (sET_CURSOR srup scup) ++ sHOW_CURSOR)
        if over == 2 then return () else mainLoop reduceW
  where
    genFood w = do
      let (foodloc, g) = R.randomR (0, (fst $ limits w) * (snd $ limits w)) (rand w)
      let (a, b) = (foodloc `div` (snd $ limits w) + 1, foodloc `rem` (snd $ limits w) + 1)
      if a == 1 || a == (snd $ limits w) || b == 1 || b == (fst $ limits w) ||
        (any (\(x, y) -> a == x && b == y) (snake w)) then genFood w { rand = g }
      else w { food = (a, b), rand = g }

split :: Eq a => [a] -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (\l -> not (elem l d)) s

main = do
  ref <- initConsole
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr (cLEAR_SCREEN ++ hIDE_CURSOR ++ sET_MAX_CURSOR ++ gET_CURSOR_POS)
  str <- waitForEsc [] False
  let strs = split "[;R" (reverse str) -- "\x1b[%u;%uR"
  let pos = Just (read (strs !! 1), read (strs !! 2))
  --putStr (sET_CURSOR 1 1)
  --case pos of
  --  Nothing -> return ()
  --  Just (height, width) -> putStrLn $ "Width: " ++ show width ++ " Height: " ++ show height
  time <- getPOSIXTime
  let initWorld = World { snake = [case pos of
      Nothing -> (-1, -1)
      Just (height, width) -> (quot height 2, quot width 2)]
                     , food = (-1, -1)
                     , lastFood = 0
                     , direction = None
                     , rand = R.mkStdGen (round time)
                     , limits = case pos of
    Nothing -> (-1, -1)
    Just (height, width) -> (height, width) }
  drawBorder initWorld
  mainLoop initWorld
  putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
  setConsole True ref
  return ()
  where
    waitForEsc s h = do
    c <- getCh
    if not h && c /= '\x1b' then waitForEsc [] h
    else if c /= 'R' then waitForEsc (c:s) True else return (c:s)