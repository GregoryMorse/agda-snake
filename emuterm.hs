--ghc -cpp -o emuterm emuterm.hs
import System.IO (putStr, isEOF, stdin, stdout, hIsTerminalDevice,
  hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Environment (getArgs)
import System.Process (system)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement)
import Data.Char (chr, isDigit)

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if defined(mingw32_HOST_OS)
import System.Win32.Console.CtrlHandler (CtrlEvent, c_SetConsoleCtrlHandler,
  mkHandler, PHANDLER_ROUTINE)
import System.Win32.Types (BOOL)
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (freeHaskellFunPtr, nullFunPtr)
import Data.IORef
import System.IO.Unsafe
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
handler :: IORef PHANDLER_ROUTINE -> CtrlEvent -> IO BOOL
handler ref _ = do
  -- (fst $ limits initWorld)
  putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_FORE_COLOR cLR_WHITE) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
  setConsole True ref
  return False

initConsole = do
  ref <- newIORef nullFunPtr
  r <- mkHandler (handler ref)
  writeIORef ref r
  setConsole False ref
  return ref

setConsole bRestore ref = do
  hSetEcho stdin bRestore
  if not bRestore then do { system " "; return () } else return ()
  h <- readIORef ref
  c_SetConsoleCtrlHandler h (not bRestore)
  if bRestore then freeHaskellFunPtr h else return ()
  --this is problematic as its a unique pointer and must be freed
  --  library is defective due to global variable necessity

cASE_ARROWKEYS = '\xe0'
cASE_ARROWKEYS_PRE :: IO Bool
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
      -- (fst (limits initWorld))
      putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_FORE_COLOR cLR_WHITE) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
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
sET_CURSOR :: Int -> Int -> String
sET_CURSOR x y = eSCVT100 ++ show x ++ ";" ++ show y ++ "H"
sET_MAX_CURSOR :: String
sET_MAX_CURSOR = sET_CURSOR 999 999
tURN_OFF_CHAR_ATTR = eSCVT100 ++ "0m"
bOLD_MODE_ON = eSCVT100 ++ "1m"
bLINKING_MODE_ON = eSCVT100 ++ "5m"
rEVERSE_MODE_ON = eSCVT100 ++ "7m"
cLR_BLACK  = 0
cLR_WHITE = 7
sET_FORE_COLOR :: Integer -> String
sET_FORE_COLOR x = eSCVT100 ++ "3" ++ show x ++ "m"
sET_BKGND_COLOR :: Integer -> String
sET_BKGND_COLOR x = eSCVT100 ++ "4" ++ show x ++ "m"
hIDE_CURSOR :: String
hIDE_CURSOR = eSCVT100 ++ "?25l"
sHOW_CURSOR :: String
sHOW_CURSOR = eSCVT100 ++ "?25h"

type Position = (Int, Int)

type CharInfo = [Position] --utf8, flag

data World = World { buf :: CharInfo
                   , curPos :: Position
                   , cursorShow :: Bool
                   , charAttr :: Int
                   , limits :: (Int, Int)
                   } deriving (Show)

mainLoop w = do
  eof <- isEOF
  if eof then do return w
  else do
    ch <- getCh
    nextW <- if (ch == '\x1b') then do
      c <- getCh
      if (c == '[') then do
        (nc, numbuf) <- waitForNonDigit []
        if (nc == 'A') then do --cursor up
          return (if ((fst $ curPos w) /= 1) then w { curPos = ((fst $ curPos w) - 1, snd $ curPos w) } else w)
        else if (nc == 'B') then do --cursor down
          return (if ((fst $ curPos w) /= (fst $ limits w)) then w { curPos = ((fst $ curPos w) + 1, snd $ curPos w) } else w)
        else if (nc == 'C') then do --cursor right
          return (if ((snd $ curPos w) /= (snd $ limits w)) then w { curPos = (fst $ curPos w, (snd $ curPos w) + 1) } else w)
        else if (nc == 'D') then do --cursor left
          return (if ((snd $ curPos w) /= 1) then w { curPos = (fst $ curPos w, (snd $ curPos w) - 1) } else w)
        else if (nc == 'J' && numbuf == ['2']) then do --clear screen
          return w { buf = [(fromEnum ' ', charAttr w) | x <- buf w] }
        else if (nc == 'K' && numbuf == ['2']) then do --clear line
          let ht = splitAt (((fst $ curPos w)-1) * (snd $ limits w)) (buf w)
          let snip = splitAt (snd $ limits w) (snd ht)
          return w { buf = fst ht ++ [(fromEnum ' ', charAttr w) | x <- fst snip] ++ (snd snip) }
        else if (nc == '?') then do
          (nnc, nnumbuf) <- waitForNonDigit []
          if (nnc == 'l' && reverse nnumbuf == ['2', '5']) then --hide cursor
            return w { cursorShow = False }
          else if (nnc == 'h' && reverse nnumbuf == ['2', '5']) then --show cursor
            return w { cursorShow = True }
          else return w
        else if (nc == 'm') then do --character attributes off=0, bold=1, blinking=5, reverse color=7, 3x=fore color, 4x=back color
          let num = (read $ reverse numbuf) :: Integer
          return w { charAttr = if (num == 0) then (charAttr w) .&. (complement 0x7)
            else if (num == 1) then (charAttr w) .|. 1
            else if (num == 5) then (charAttr w) .|. 2
            else if (num == 7) then (charAttr w) .|. 4
            else if (num >= 30 && num <= 37) then (charAttr w) .&. (complement 0x38) .|. ((fromInteger (num - 30)) `shiftL` 3)
            else if (num >= 40 && num <= 47) then (charAttr w) .&. (complement 0x1C0) .|. ((fromInteger (num - 40)) `shiftL` 6)
            else charAttr w }
        else if (nc == 'n' && numbuf == ['6']) then do --get cursor position
          putStr("\x1b[" ++ (show $ fst $ curPos w) ++ ";" ++ (show $ snd $ curPos w) ++ "R")
          return w
        else if (nc == ';') then do
          (nnc, nnumbuf) <- waitForNonDigit []
          if (nnc == 'H') then do
            let mW = w { curPos = (read $ reverse numbuf, read $ reverse nnumbuf) }
            let mW1 = if ((fst $ curPos mW) == 0) then mW { curPos = ((fst $ curPos mW) + 1, snd $ curPos mW) } else mW
            let mW2 = if ((snd $ curPos mW1) == 0) then mW1 { curPos = (fst $ curPos mW1, (snd $ curPos mW1) + 1) } else mW1
            let mW3 = if ((fst $ curPos mW2) > (fst $ limits w)) then mW2 { curPos = (fst $ limits w, snd $ curPos mW2) } else mW2
            let mW4 = if ((snd $ curPos mW3) > (snd $ limits w)) then mW3 { curPos = (fst $ curPos mW3, snd $ limits w) } else mW3
            return mW4
          else
            return w
        else return w
      else return w
    else do
      let sp = splitAt (((fst $ curPos w)-1) * (snd $ limits w) + (snd $ curPos w)-1) (buf w)
      --windows still uses codepages and not utf-8
      chW <-
#if !defined(mingw32_HOST_OS)
      if (ch <= 0xBF) then do --check utf8 length but not validity
#endif
        return w { buf = (fst sp) ++ ((fromEnum (if ch == '\n' then ' ' else ch), charAttr w):(tail $ snd sp)) }
#if !defined(mingw32_HOST_OS)
      else if (ch >= 0xC0 && ch <= 0xDF) then
        cb <- getCh
        return w { buf = (fst sp) ++ [(ch | (cb `shiftL` 8), charAttr w):(tail $ snd sp)] }
      else if (ch >= 0xE0 && ch <= 0xEF) then
        cb <- getCh
        cc <- getCh
        return w { buf = (fst sp) ++ [(ch | (cb `shiftL` 8) | (cc `shiftL` 16), charAttr w):(tail $ snd sp)] }
      else
        cb <- getCh
        cc <- getCh
        cd <- getCh
        return w { buf = (fst sp) ++ [(ch | (cb `shiftL` 8) | (cc `shiftL` 16) | (cd `shiftL` 24), charAttr w):(tail $ snd sp)] }
#endif
      if (ch == '\n' || (snd $ curPos w) == (snd $ limits w)) then do
        let nW = chW { curPos = (snd $ curPos w, 1) }
        let nW1 = if (fst $ curPos nW) /= (fst $ limits w) then nW { curPos = ((snd $ curPos nW) + 1, fst $ curPos nW) } else nW
        return nW1
      else return chW { curPos = (fst $ curPos w, (snd $ curPos w) + 1) }
    mainLoop nextW
  where
    waitForNonDigit s = do
    c <- getCh
    if isDigit c then waitForNonDigit (c:s)
    else return (c, s)
    
drawOutput w i = do
  if i == (snd $ limits w) * (fst $ limits w) then return ()
  else do
    if (snd ((buf w) !! i)) .&. 1 /= 0 then do putStr bOLD_MODE_ON else do return ()
    if (snd ((buf w) !! i)) .&. 2 /= 0 then do putStr bLINKING_MODE_ON else do return ()
    if (snd ((buf w) !! i)) .&. 4 /= 0 then do putStr rEVERSE_MODE_ON else do return ()
    putStr ((sET_CURSOR ((i `div` (snd $ limits w))+1) ((i `rem` (snd $ limits w))+1)) ++ (sET_FORE_COLOR $ toInteger ((snd ((buf w) !! i) .&. 0x38) `shiftR` 3)) ++
        (sET_BKGND_COLOR $ toInteger ((snd ((buf w) !! i) .&. 0x1C0) `shiftR` 6)) ++ [if (fst ((buf w) !! i)) == 0 then ' ' else chr ((fst ((buf w) !! i)) .&. 0xFF)])
#if !defined(mingw32_HOST_OS)
    if ((fst ((buf w) !! i)) .&. 0xFF >= 0xC0) then putStr [chr (((fst ((buf w) !! i)) .&. 0xFF00) `shiftR` 8)] else return ()
    if ((fst ((buf w) !! i)) .&. 0xFF >= 0xE0) then putStr [chr (((fst ((buf w) !! i)) .&. 0xFF0000) `shiftR` 16)] else return ()
    if ((fst ((buf w) !! i)) .&. 0xFF >= 0xF0) then putStr [chr (((fst ((buf w) !! i)) .&. 0xFF000000) `shiftR` 24)] else return ()
#endif
    if (snd ((buf w) !! i)) .&. 7 /= 0 then putStr tURN_OFF_CHAR_ATTR else return ()
    drawOutput w (i + 1)
  
split :: Eq a => [a] -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (\l -> not (elem l d)) s

main = do
  args <- getArgs
  ref <- initConsole
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  ((curposy, curposx), (height, width)) <- if length args == 2 then return ((1, 1), (read (args !! 0), read (args !! 1)))
    else do
      putStr(gET_CURSOR_POS)
      st <- waitForEsc [] False
      let ss = split "[;R" (reverse st) -- "\x1b[%u;%uR"
      putStr(sET_MAX_CURSOR ++ gET_CURSOR_POS)
      str <- waitForEsc [] False
      let strs = split "[;R" (reverse str) -- "\x1b[%u;%uR"
      return ((read (ss !! 1), read (ss !! 2)), (read (strs !! 1), read (strs !! 2)))
  let initWorld = World { buf = [(0, 0) | x <- [1..(height * width)]]
                     , curPos = (curposy, curposx)
                     , charAttr = (fromInteger cLR_WHITE) `shiftL` 3 --bits 1-3=bold, blinking, reverse color, bits 4-6=fore color, bits 7-9= back color
                     , cursorShow = True
                     , limits = (height, width) }
  w <- mainLoop initWorld
  putStr hIDE_CURSOR
  drawOutput w 0
  putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_FORE_COLOR cLR_WHITE) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)
  setConsole True ref
  return ()
  where
    waitForEsc s h = do
    c <- getCh
    if not h && c /= '\x1b' then waitForEsc [] h
    else if c /= 'R' then waitForEsc (c:s) True else return (c:s)