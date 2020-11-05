--$HOME/.cabal/bin/agda on Ubuntu if installed via cabal install agda
--cd /D D:\OneDrive\Documents\Projects\ELTE\agda
--the Win32 callbacks are inherintly unsafe for some detailed reasons from calling convention, to the way the threading environment works in Haskell
--however the library used properly creates function pointers so there scope and safety should be okay and comparing with other Haskell library code there are not major differences
--a safe wrapper is GHC.ConsoleHandler.installHandler, --ghc-flag=-threaded does not work, but running from within GHCI worked at least for snake
--it seems from debugger that first in Linux, signal handling in alternate thread requires a throwTo to pass an exit exception to the main thread
--from within the executable file produced by GHCI, similar problem exists so exit is hooked already, and exiting from a thread can cause some IO conditions which deadlock the termination, or if the signal handler is not yet removed, an infinite recursion
--agda --ghc --ghc-flag=-cpp --ghc-flag=-threaded snake.agda
--agda --js snake.agda --compile-dir=snakejs
--ghc -c -O snakeVerify.hs
--ghc --make -no-hs-main -optc-O snakeVer.c snakeVerify -o snakever
--cabal install --lib random
module snake where

open import IO using (run; lift; _>>=_; _>>_; return; putStr; sequence′; mapM′; IO)
open import Codata.Musical.Notation using (∞; ♯_; ♭)
open import Data.Bool using (Bool; false; true; not; _∧_; _∨_) -- _≟_)
open import Data.Bool.Base using (if_then_else_)
open import Relation.Binary.PropositionalEquality public using (_≡_; refl; _≢_)
open import Relation.Nullary.Decidable using (⌊_⌋)
open import Data.Nat.DivMod using (_div_; _mod_)
open import Data.Unit using (⊤)
--open import Data.Unit.Polymorphic using () renaming (⊤ to Unit⊤)
open import Codata.Musical.Colist using (Colist; []; _∷_; map)
open import Codata.Musical.Costring using (Costring; toCostring)
open import Data.List using (List; span; reverse; []; _∷_; any; length)
open import Data.List.Categorical using (monadT)
open import Data.String using (String; fromList; toList; _++_; _==_)
open import Function using (_∘_; case_of_)
open import Category.Monad using (RawMonad)
open import Data.Maybe.Categorical using (monad)
open import Data.Maybe.Base using (nothing; just; maybe′) renaming (Maybe to MaybeAgda)
open import Data.Digit using (Digit; fromDigits)
open import Data.Fin using (#_; toℕ)
open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _*_) -- _≟_)
open import Data.Nat.Show using (show)
open import Data.Char using (Char; _≟_)
open import Agda.Builtin.Sigma using (Σ; fst; snd)
open import Agda.Builtin.IO public using (IO)
open import IO.Primitive using ()
open import Level using (_⊔_)

{-# FOREIGN GHC
#if defined(mingw32_HOST_OS)
import System.Process (system)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Win32.Console.CtrlHandler (CtrlEvent, c_SetConsoleCtrlHandler,
  mkHandler, PHANDLER_ROUTINE)
import qualified System.Win32.Console.CtrlHandler as CtrlHandler (Handler)
import System.Win32.Types (BOOL)
import Data.Char (chr)
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (freeHaskellFunPtr, nullFunPtr, FunPtr)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
foreign import ccall unsafe "conio.h kbhit"
  c_kbhit :: IO CInt
fromColist :: MAlonzo.Code.Codata.Musical.Colist.AgdaColist a -> [a]
fromColist MAlonzo.Code.Codata.Musical.Colist.Nil         = []
fromColist (MAlonzo.Code.Codata.Musical.Colist.Cons x xs) =
  x : fromColist (MAlonzo.RTE.flat xs)
type WinHandler = CtrlHandler.Handler
--dummy definitions from the Linux side
type CLong = CInt
type Fd = Integer
stdInput = 0
type CTimeval = Integer
data Timeout = Never | Time CTimeval
type SignalInfo = Integer
data Handler = Default | Ignore | Catch (IO ()) | CatchOnce (IO ()) | CatchInfo (SignalInfo -> IO ()) | CatchInfoOnce (SignalInfo -> IO ())
type ThreadId = Integer
myThreadId = do return 0
finite :: CLong -> CLong -> Timeout
finite _ _ = Time 0
type SignalSet = Integer
select'' _ _ _ _ = do return 0
installHandler _ _ _ = do return Default
sigINT = 0
sigTERM = 0
throwTo _ _ = do return ()
#else
import Foreign.C.Types (CLong, CInt)
import System.Posix.Types (Fd)
import System.Posix.IO (stdInput)
import System.Posix.IO.Select (select'')
import System.Posix.IO.Select.Types (finite, Timeout (Never, Time), CTimeval)
import System.Posix.Signals (installHandler, Handler(Catch, Default, Ignore, CatchOnce, CatchInfo, CatchInfoOnce), SignalInfo, SignalSet, sigINT, sigTERM)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import Control.Concurrent (myThreadId, ThreadId)
import Control.Exception (throwTo)
--dummy definitions from the Windows side
data IORef a = IORef a
type CtrlEvent = Integer
type WinHandler = CtrlEvent -> IO Bool
data Addr = Addr | NullAddr
data FunPtr a = FunPtr Addr
type PHANDLER_ROUTINE = FunPtr WinHandler
system :: String -> IO ExitCode
system _ = do return ExitSuccess
fromColist :: MAlonzo.Code.Codata.Musical.Colist.AgdaColist a -> [a]
fromColist _ = []
chr :: Int -> Char
chr _ = '\x0'
c_getch :: IO CInt
c_getch = do return 0
c_kbhit :: IO CInt
c_kbhit = do return 0
mkHandler :: (CtrlEvent -> IO Bool) -> IO (FunPtr WinHandler)
mkHandler _ = do return (FunPtr Addr)
c_SetConsoleCtrlHandler :: FunPtr WinHandler -> Bool -> IO Bool
c_SetConsoleCtrlHandler _ _ = do return True
freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr _ = do return ()
nullFunPtr :: FunPtr a
nullFunPtr = FunPtr NullAddr
newIORef :: a -> IO (IORef a)
newIORef x = do return (IORef x)
readIORef :: IORef a -> IO a
readIORef (IORef x) = do return x
writeIORef :: IORef a -> a -> IO ()
writeIORef _ _ = do return ()
#endif
toColist :: [a] -> MAlonzo.Code.Codata.Musical.Colist.AgdaColist a
toColist []       = MAlonzo.Code.Codata.Musical.Colist.Nil
toColist (x : xs) =
  MAlonzo.Code.Codata.Musical.Colist.Cons x (MAlonzo.RTE.Sharp (toColist xs))
type AgdaPair a b c d = (c , d)
data EnumDict a = Enum a => EnumDict
data NumDict a = Num a => NumDict
data IntegralDict a = Integral a => IntegralDict
data RealFracDict a = RealFrac a => RealFracDict
data RandomDict a = R.Random a => RandomDict
data RandomGenDict g = R.RandomGen g => RandomGenDict
#-}

postulate
  Int : Set

data ExitCode : Set where
  exitSuccess : ExitCode
  exitFailure : (a : Int) → ExitCode

postulate
--  FunPtr : Set
  FunPtr : Set → Set
--  IORef : Set
  IORef : Set → Set
  system : Costring → Agda.Builtin.IO.IO ExitCode
--  fromInteger : ℕ → Int
  Num : Set → Set
  instance NumInt : Num Int
  fromInteger : {a : Set} {{_ : Num a}} → ℕ → a
  CInt : Set
  chr : Int → Char
--  chrFromEnum : CInt → Char
  c_getch : Agda.Builtin.IO.IO CInt
  c_kbhit : Agda.Builtin.IO.IO CInt
  CtrlEvent : Set

BOOL : Set
BOOL = Bool
WinHandler : Set
WinHandler = CtrlEvent → Agda.Builtin.IO.IO BOOL
PHANDLER_ROUTINE : Set
--PHANDLER_ROUTINE = FunPtr
PHANDLER_ROUTINE = FunPtr WinHandler

postulate
  mkHandler : WinHandler → Agda.Builtin.IO.IO PHANDLER_ROUTINE
  c_SetConsoleCtrlHandler : PHANDLER_ROUTINE → BOOL → Agda.Builtin.IO.IO BOOL
--  freeHaskellFunPtr : FunPtr → Agda.Builtin.IO.IO ⊤
--  nullFunPtr : FunPtr
  freeHaskellFunPtr : {a : Set} → FunPtr a → Agda.Builtin.IO.IO ⊤
  nullFunPtr : {a : Set} → FunPtr a
--  newIORef : PHANDLER_ROUTINE → Agda.Builtin.IO.IO IORef
--  readIORef : IORef → Agda.Builtin.IO.IO PHANDLER_ROUTINE
--  writeIORef : IORef → PHANDLER_ROUTINE → Agda.Builtin.IO.IO ⊤
  newIORef : {a : Set} → a → Agda.Builtin.IO.IO (IORef a)
  readIORef : {a : Set} → IORef a → Agda.Builtin.IO.IO a
  writeIORef : {a : Set} → IORef a → a → Agda.Builtin.IO.IO ⊤
--  showAddr : PHANDLER_ROUTINE → Costring

{-# COMPILE GHC Int = type Int #-}

--ExitFailure Int - but ℕ and Integer in Agda are both Integer in Haskell
--postulate ExitCode : Set

{-# COMPILE GHC ExitCode = data ExitCode (ExitSuccess | ExitFailure) #-}
{-# COMPILE GHC system = system . fromColist #-}

--{-# COMPILE GHC fromInteger = fromInteger #-}
{-# COMPILE GHC Num = type NumDict #-}
{-# COMPILE GHC NumInt = NumDict #-}
{-# COMPILE GHC fromInteger = \ _ NumDict -> fromInteger #-}
{-# COMPILE GHC CInt = type CInt #-}
--{-# COMPILE GHC chrFromEnum = chr.fromEnum #-}
{-# COMPILE GHC chr = chr #-}
{-# COMPILE GHC c_kbhit = c_kbhit #-}
{-# COMPILE GHC c_getch = c_getch #-}
{-# COMPILE GHC FunPtr = type FunPtr #-}
{-# COMPILE GHC CtrlEvent = type CtrlEvent #-}
{-# COMPILE GHC mkHandler = mkHandler #-}
{-# COMPILE GHC c_SetConsoleCtrlHandler = c_SetConsoleCtrlHandler #-}
--{-# COMPILE GHC nullFunPtr = nullFunPtr #-}
{-# COMPILE GHC nullFunPtr = \ _ -> nullFunPtr #-}
--{-# COMPILE GHC freeHaskellFunPtr = freeHaskellFunPtr #-}
{-# COMPILE GHC freeHaskellFunPtr = \ _ -> freeHaskellFunPtr #-}
--{-# COMPILE GHC IORef = type IORef PHANDLER_ROUTINE #-}
{-# COMPILE GHC IORef = type IORef #-}
--{-# COMPILE GHC newIORef = newIORef #-}
--{-# COMPILE GHC readIORef = readIORef #-}
--{-# COMPILE GHC writeIORef = writeIORef #-}
{-# COMPILE GHC newIORef = \ _ -> newIORef #-}
{-# COMPILE GHC readIORef = \ _ -> readIORef #-}
{-# COMPILE GHC writeIORef = \ _ -> writeIORef #-}
--{-# COMPILE GHC showAddr = toColist . show #-}

postulate
  CTimeval : Set
  CLong : Set
  --fromIntegerCLong : ℕ → CLong
  instance NumCLong : Num CLong
  SignalInfo : Set

data Timeout : Set where
  never : Timeout
  time : CTimeval → Timeout

data PosixHandler : Set where
  default : PosixHandler
  ignore : PosixHandler
  catch : Agda.Builtin.IO.IO ⊤ → PosixHandler
  catchOnce : Agda.Builtin.IO.IO ⊤ → PosixHandler
  catchInfo : (SignalInfo → Agda.Builtin.IO.IO ⊤) → PosixHandler
  catchInfoOnce : (SignalInfo → Agda.Builtin.IO.IO ⊤) → PosixHandler

signal : Set
signal = CInt

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just : A → Maybe A

postulate
  Fd : Set
  stdInput : Fd
  finite : CLong → CLong → Timeout
  select'' : List Fd → List Fd → List Fd → Timeout → Agda.Builtin.IO.IO CInt
  ThreadId : Set
  myThreadId : Agda.Builtin.IO.IO ThreadId
  SignalSet : Set
  installHandler : signal → PosixHandler → Maybe SignalSet → Agda.Builtin.IO.IO PosixHandler
  sigINT : CInt
  sigTERM : CInt
  throwTo : ThreadId → ExitCode → Agda.Builtin.IO.IO ⊤
  
--{-# COMPILE GHC fromIntegerCLong = fromInteger #-}
{-# COMPILE GHC NumCLong = NumDict #-}
{-# COMPILE GHC CLong = type CLong #-}
{-# COMPILE GHC Fd = type Fd #-}
{-# COMPILE GHC stdInput = stdInput #-}
{-# COMPILE GHC finite = finite #-}
{-# COMPILE GHC CTimeval = type CTimeval #-}
{-# COMPILE GHC Timeout = data Timeout (Never | Time) #-}
{-# COMPILE GHC select'' = select'' #-}
{-# COMPILE GHC ThreadId = type ThreadId #-}
{-# COMPILE GHC myThreadId = myThreadId #-}
{-# COMPILE GHC PosixHandler = data Handler (Default | Ignore | Catch | CatchOnce | CatchInfo | CatchInfoOnce) #-}
{-# COMPILE GHC SignalInfo = type SignalInfo #-}
{-# COMPILE GHC SignalSet = type SignalSet #-}
{-# COMPILE GHC Maybe = data Maybe (Nothing | Just) #-}
{-# COMPILE GHC installHandler = installHandler #-}
{-# COMPILE GHC sigINT = sigINT #-}
{-# COMPILE GHC sigTERM = sigTERM #-}
{-# COMPILE GHC throwTo = throwTo #-}

data BufferMode : Set where
  noBuffering : BufferMode
  lineBuffering : BufferMode
  blockBuffering : Maybe Int → BufferMode

data _×_ (A B : Set) : Set where
  <_,_> : A → B → A × B
fst` : {A B : Set} → A × B → A
fst` < x , y > = x
snd` : {A B : Set} → A × B → B
snd` < x , y > = y

-- non-dependent pair; failed to reuse _×_

record Pair {a b} (A : Set a) (B : Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst`` : A
    snd`` : B

postulate
  --toInteger, fromEnum, round need a dictionary for type class kind
  --toInteger : Int → ℕ
  RealFrac : Set → Set
  Integral : Set → Set
  instance IntegralInt : Integral Int
  toInteger : {a : Set} {{_ : Integral a}} → a → ℕ
  --fromEnum : CInt → Int
  Enum : Set → Set
  instance EnumCInt : Enum CInt
  fromEnum : {a : Set} {{_ : Enum a}} → a → Int
  
  RStdGen : Set
  RmkStdGen : Int → RStdGen
  RandomGen : Set → Set
  instance RandomGenStdGen : RandomGen RStdGen
--  RrandomR : (Pair ℕ ℕ) → RStdGen → Pair ℕ RStdGen
--  RrandomR : {g : Set} {{_ : RandomGen g}} → (Pair ℕ ℕ) → g → Pair ℕ g
  Random : Set → Set
  instance RandomNat : Random ℕ
  RrandomR : {a g : Set} {{_ : Random a}} → {{_ : RandomGen g}} → (Pair a a) → g → Pair a g
  FileHandle : Set
  stdin : FileHandle
  stdout : FileHandle
  hGetChar : FileHandle → Agda.Builtin.IO.IO Char
  hSetBuffering : FileHandle → BufferMode → Agda.Builtin.IO.IO ⊤
  hSetEcho : FileHandle → Bool → Agda.Builtin.IO.IO ⊤
  hIsTerminalDevice : FileHandle → Agda.Builtin.IO.IO Bool
  POSIXTime : Set
  instance RealFracPOSIXTime : RealFrac POSIXTime
  getPOSIXTime : Agda.Builtin.IO.IO POSIXTime
--  round : POSIXTime → Int
  round : {a b : Set} {{_ : RealFrac a}} {{_ : Integral b}} → a → b
  threadDelay : Int → Agda.Builtin.IO.IO ⊤
  os : Costring

postulate
  getch_js : Char
  

os_js : Costring
os_js = Codata.Musical.Colist.fromList (toList "mingw32") --"linux"
hGetChar_js : FileHandle → IO.IO Char
hGetChar_js _ = return getch_js
open import Agda.Builtin.Char using (primNatToChar)

--fromEnum_js : {a : Set} {{_ : Enum a}} → a → Int
--fromEnum_js c = λ _ → 0

chr_js : Int → Char
chr_js c = Agda.Builtin.Char.primNatToChar 255


{-# COMPILE GHC RealFrac = type RealFracDict #-}
{-# COMPILE GHC Integral = type IntegralDict #-}
{-# COMPILE GHC toInteger = \ _ IntegralDict -> toInteger #-}
{-# COMPILE GHC IntegralInt = IntegralDict #-}
--{-# COMPILE GHC toInteger = toInteger #-}
{-# COMPILE GHC Enum = type EnumDict #-}
{-# COMPILE GHC EnumCInt = EnumDict #-}
--{-# COMPILE GHC fromEnum = fromEnum #-}
{-# COMPILE GHC fromEnum = \ _ EnumDict -> fromEnum #-}
{-# COMPILE GHC Pair = data AgdaPair ((,)) #-}
{-# COMPILE GHC RStdGen = type R.StdGen #-}
{-# COMPILE GHC RmkStdGen = R.mkStdGen #-}
{-# COMPILE GHC RandomGen = type RandomGenDict #-}
{-# COMPILE GHC RandomGenStdGen = RandomGenDict #-}
{-# COMPILE GHC Random = type RandomDict #-}
{-# COMPILE GHC RandomNat = RandomDict #-}
--{-# COMPILE GHC RrandomR = R.randomR #-}
--{-# COMPILE GHC RrandomR = \ _ RandomGenDict -> R.randomR #-}
{-# COMPILE GHC RrandomR = \ _ _ RandomDict RandomGenDict -> R.randomR #-}
--Handle is needed even though not in Haskell due to the signatures here
{-# COMPILE GHC FileHandle = type IO.Handle #-}
{-# COMPILE GHC stdin = IO.stdin #-}
{-# COMPILE GHC stdout = IO.stdout #-}
{-# COMPILE GHC hGetChar = IO.hGetChar #-}
{-# COMPILE GHC hSetBuffering = IO.hSetBuffering #-}
{-# COMPILE GHC hSetEcho = IO.hSetEcho #-}
--Even though LineBuffering and BlockBuffering are not used directly, indirectly they will be used in exhaustive pattern matching and must be brought in
{-# COMPILE GHC BufferMode = data IO.BufferMode (IO.NoBuffering | IO.LineBuffering | IO.BlockBuffering) #-}

{-# COMPILE GHC hIsTerminalDevice = IO.hIsTerminalDevice #-}
{-# COMPILE GHC getPOSIXTime = getPOSIXTime #-}
{-# COMPILE GHC POSIXTime = type POSIXTime #-}
{-# COMPILE GHC RealFracPOSIXTime = RealFracDict #-}
{-# COMPILE GHC round = \ _ _ RealFracDict IntegralDict -> round #-}
--{-# COMPILE GHC round = round #-}
{-# COMPILE GHC threadDelay = threadDelay #-}
{-# COMPILE GHC os = toColist System.Info.os #-}
{-# FOREIGN GHC
import qualified System.Info (os)
import qualified System.Random as R (Random, RandomGen, randomR, mkStdGen, StdGen)
import qualified System.IO as IO (Handle, hIsTerminalDevice, hSetEcho, hSetBuffering, hGetChar, BufferMode (NoBuffering, LineBuffering, BlockBuffering), stdin, stdout)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
--putStr already in Agda IO library
import Control.Concurrent (threadDelay)
#-}

data _⊥ {a} (A : Set a) : Set a where
  now   : (x : A) → A ⊥
  later : (x : ∞ (A ⊥)) → A ⊥

rev : Costring → String ⊥
rev = go []
  where
  go : List Char → Costring → String ⊥
  go acc []       = now (Data.String.fromList acc)
  go acc (x ∷ xs) = later (♯ go (x ∷ acc) (♭ xs))

{-# NON_TERMINATING #-}
getStr : String ⊥ → String
getStr (now s) = s
getStr (later s) = getStr (♭ s)

isWin : Bool
isWin = (Data.String.fromList (reverse (Data.String.toList (getStr (rev os))))) == "mingw32"

getCh : _
getCh = if isWin then ((♯ (lift (hIsTerminalDevice stdin))) >>= λ isTerm → ♯ (if isTerm then (♯ (lift c_getch)) >>= (λ ch → ♯ return (chr (fromEnum ch))) else (♯ return Data.Unit.tt >>= λ _ → ♯ lift (hGetChar stdin)))) else (♯ return Data.Unit.tt) >>= (λ _ → ♯ lift (hGetChar stdin))
chkKB : IO.IO Bool
chkKB = if isWin then (♯ lift c_kbhit >>= λ is → ♯ return (not ⌊ (toInteger (fromEnum is)) Data.Nat.≟ 0 ⌋)) else (♯ lift (select'' (stdInput ∷ []) [] [] (finite (fromInteger 0) (fromInteger 0)))) >>= λ x → ♯ return ⌊ (toInteger (fromEnum x)) Data.Nat.≟ 1 ⌋

cASE_ARROWKEYS : Char
cASE_ARROWKEYS = if isWin then '\xe0' else '\x1b'

cASE_ARROWKEYS_PRE : IO.IO Bool
cASE_ARROWKEYS_PRE = if isWin then return true else ((♯ getCh) >>= (λ ch → ♯ return ⌊ ch ≟ '[' ⌋))

uPARROW = if isWin then 'H' else 'A'
dOWNARROW = if isWin then 'P' else 'B'
rIGHTARROW = if isWin then 'M' else 'C'
lEFTARROW = if isWin then 'K' else 'D'

eSCVT100 : String
eSCVT100 = "\x1b[" --check infocmp vt100 and infocmp -Cr vt100
gET_CURSOR_POS : String
gET_CURSOR_POS = eSCVT100 ++ "6n"
cLEAR_SCREEN : String
cLEAR_SCREEN = eSCVT100 ++ "2J"
sET_CURSOR : ℕ → ℕ → String
sET_CURSOR x y = eSCVT100 ++ show x ++ ";" ++ show y ++ "H"
sET_MAX_CURSOR : String
sET_MAX_CURSOR = sET_CURSOR 999 999
cLR_BLACK  = 0
cLR_RED = 1
cLR_GREEN = 2
cLR_YELLOW = 3
cLR_BLUE = 4
cLR_MAGENTA = 5
cLR_CYAN = 6
cLR_WHITE = 7
sET_BKGND_COLOR : ℕ → String
sET_BKGND_COLOR x = eSCVT100 ++ "4" ++ show x ++ "m"
hIDE_CURSOR : String
hIDE_CURSOR = eSCVT100 ++ "?25l"
sHOW_CURSOR : String
sHOW_CURSOR = eSCVT100 ++ "?25h"

data Direction : Set where
  north : Direction
  south : Direction
  east : Direction
  west : Direction
  none : Direction

record World : Set where
  field
    snake : List (ℕ × ℕ)
    food : ℕ × ℕ
    lastFood : ℕ
    direction : Direction
    rand : RStdGen
    limits : ℕ × ℕ

{-# NON_TERMINATING #-}
setConsole : Bool → IORef PHANDLER_ROUTINE → IO.IO ⊤
setConsole bRestore ref = if isWin then ((♯ ((♯ (lift (hSetEcho stdin bRestore))) >> (if not bRestore then ♯ ((♯ (lift (system (toCostring " ")))) >> (♯ return false)) else ♯ return false))) >>
  ♯ ((♯ lift (readIORef ref)) >>= λ h → ♯ ((♯ lift (c_SetConsoleCtrlHandler h (not bRestore))) >>
--  (♯ IO.putStr∞ (showAddr h))) >>
  (if bRestore then ♯ lift (freeHaskellFunPtr h) else ♯ return _))))
  else (♯ ((♯ lift (hSetEcho stdin bRestore)) >> (♯ lift myThreadId)) >>= λ tid → ♯ (((♯ lift (installHandler sigINT (if bRestore then default else catch (posixhandler tid ref)) nothing))) >> ♯ ((♯ (lift (installHandler sigTERM (if bRestore then default else catch (posixhandler tid ref)) nothing))) >> ♯ return _)))
  where
    posixhandler : ThreadId → IORef PHANDLER_ROUTINE → Agda.Builtin.IO.IO ⊤
    posixhandler tid ref = run (♯ (return Data.Unit.tt) >> ♯ (♯ (♯ ((♯ putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)) >> (♯ setConsole true ref)) >> ♯ (lift (throwTo tid (exitFailure (fromInteger 0))))) >> ♯ return _))

initConsole : IO.IO (IORef PHANDLER_ROUTINE)
initConsole = (♯ lift (newIORef nullFunPtr)) >>=
  (λ ref → ♯ ((♯ (lift (mkHandler (handler ref)))) >>=
  (λ r → ♯ ((♯ lift (writeIORef ref r)) >> 
  ♯ ((♯ (setConsole false ref)) >> (♯ return ref))))))
  where
    --this needs to be constructed in the primitive IO context or the JS compiler will optimize out the reference...
    handler : IORef PHANDLER_ROUTINE → CtrlEvent → Agda.Builtin.IO.IO Bool
    handler ref _ = --IO.Primitive.return false
      run (♯ (return Data.Unit.tt) >> ♯ (♯ (♯ (setConsole true ref) >> (♯ putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR))) >>
      ♯ return false ))
    --  run (♯ ((♯ (putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR))) >> (♯ (setConsole true ref))) >>
    --  ♯ return false)
  

genList : (A : ℕ) → Colist ℕ
genList zero = []
genList (ℕ.suc n) = (ℕ.suc n) ∷ ♯ (genList n)


head : ∀ {a} {A : Set a} → List A → A → A
head []       def = def
head (x ∷ xs) _   = x

--e.g. drop 1
tail : ∀ {a} {A : Set a} → List A → List A
tail []       = []
tail (x ∷ xs) = xs

{-# TERMINATING #-}
split : List Char → List Char → List (List Char)
split d [] = []
split d s = (fst x×y) ∷ (split d (tail (snd x×y)))
  where
    x×y : Σ (List Char) (λ x → List Char)
    x×y = span (λ l → (any (λ c → ⌊ c ≟ l ⌋ ) d) Data.Bool.≟ false) s

drawBorder : World → IO.IO World
drawBorder w = -- >> ♯ return _
  (♯ (sequence′ ((mapM′ (whiteOut) (map (λ x → < 1 , x >) (genList (snd` (World.limits w ))))) ∷
    ♯ ((mapM′ (whiteOut) (map (λ x → < fst` (World.limits w) , x >) (genList (snd` (World.limits w))))) ∷
    ♯ ((mapM′ (whiteOut) (map (λ x → < x , 1 >) (genList (fst` (World.limits w))))) ∷
    ♯ ((mapM′ (whiteOut) (map (λ x → < x , snd` (World.limits w) >) (genList (fst` (World.limits w))))) ∷ (♯ []))))))) >>= λ _ → ♯ ((♯ putStr ((sET_CURSOR (fst` (head (World.snake w) < 0 , 0 > )) (snd` (head (World.snake w) < 0 , 0 > ))) ++ (sET_BKGND_COLOR cLR_GREEN) ++ " ")) >> (♯ return w))
  where
    whiteOut : ℕ × ℕ → _
    whiteOut < row , col > = putStr ((sET_CURSOR row col) ++ (sET_BKGND_COLOR cLR_WHITE) ++ " ")

{-# NON_TERMINATING #-}
mainLoop : World → IO.IO ⊤
mainLoop w = ♯ (♯ ((♯ (lift (hIsTerminalDevice stdin))) >>=
  (λ isTerm → if isTerm then ♯ chkKB else ♯ return true)) >>=
  (λ is → if is then
    ♯ ((♯ getCh) >>= (λ ch →
      if ⌊ ch ≟ 'x' ⌋ then ♯ return < none , true >
      else ♯ (if ⌊ ch ≟ cASE_ARROWKEYS ⌋ then
        ((♯ cASE_ARROWKEYS_PRE) >>= (λ nxt → ♯ (if nxt then
          ((♯ getCh) >>= (λ cn → ♯ return (
            if ⌊ cn ≟ uPARROW ⌋ then < north , false >
            else if ⌊ cn ≟ dOWNARROW ⌋ then < south , false >
            else if ⌊ cn ≟ rIGHTARROW ⌋ then < west , false >
            else if ⌊ cn ≟ lEFTARROW ⌋ then < east , false >
            else < none , false >)))
          else return < none , false >) ))
        else return < none , false > )))
  else ♯ ((♯ lift (threadDelay (fromInteger 100000))) >>= (λ _ → ♯ return < none , false >)))) >>= (λ newDir → if snd` newDir then ♯ return _ else
  let nextW : World
      nextW = record w { direction =
        case fst` newDir of λ {
        north → if (case World.direction w of λ { south → false; _ → true }) ∨ ⌊ (length (World.snake w)) Data.Nat.≟ 1 ⌋ then fst` newDir else World.direction w;
        south → if (case World.direction w of λ { north → false; _ → true }) ∨ ⌊ (length (World.snake w)) Data.Nat.≟ 1 ⌋ then fst` newDir else World.direction w;
        west → if (case World.direction w of λ { east → false; _ → true }) ∨ ⌊ (length (World.snake w)) Data.Nat.≟ 1 ⌋ then fst` newDir else World.direction w;
        east → if (case World.direction w of λ { west → false; _ → true }) ∨ ⌊ (length (World.snake w)) Data.Nat.≟ 1 ⌋ then fst` newDir else World.direction w;
        none → World.direction w } } in case World.direction nextW of λ { none → ♯ mainLoop w; _ →
  ♯ ((♯ (let foodW : World
             foodW = record nextW { lastFood = (World.lastFood nextW) + 1 } in
  if ⌊ (World.lastFood foodW) Data.Nat.≟ 50 ⌋ ∧ ⌊ (fst` (World.food foodW)) Data.Nat.≟ 0 ⌋ ∧ ⌊ (snd` (World.food foodW)) Data.Nat.≟ 0 ⌋ then
    ((♯ (genFood foodW)) >>= (λ genW → ♯
    (let --genW : World
         --genW = lift (genFood foodW)
         foodloc : ℕ × ℕ
         foodloc = World.food genW in
        (♯ putStr (hIDE_CURSOR ++ (sET_CURSOR (fst` foodloc) (snd` foodloc)) ++
          (sET_BKGND_COLOR cLR_YELLOW) ++ " ")) >>
          (♯ return genW))))
  else return foodW)) >>= (λ fW →
  let srsc : ℕ × ℕ
      srsc = head (World.snake fW) < 0 , 0 >
      sr : ℕ
      sr = fst` srsc
      sc : ℕ
      sc = snd` srsc
      upW : World
      upW = record fW { snake =
        (case World.direction fW of λ {
        north → < sr ∸ 1 , sc >;
        south → < sr + 1 , sc >;
        east → < sr , sc ∸ 1 >;
        west → < sr , sc + 1 >;
        _ → < sr , sc > }) ∷ (World.snake fW) }
      srupscup : ℕ × ℕ
      srupscup = head (World.snake upW) < 0 , 0 >
      srup = fst` srupscup
      scup = snd` srupscup
      foodloc : ℕ × ℕ
      foodloc = World.food upW in
      ♯ ((if ⌊ srup Data.Nat.≟ fst` foodloc ⌋ ∧ ⌊ scup Data.Nat.≟ snd` foodloc ⌋ then (♯ return record upW { lastFood = 0; food = < 0 , 0 > })
      else
        let oldsroldsc : ℕ × ℕ
            oldsroldsc = head (reverse (World.snake upW)) < 0 , 0 >
            oldsr = fst` oldsroldsc
            oldsc = snd` oldsroldsc in
            ♯ ((♯ (putStr (hIDE_CURSOR ++ (sET_CURSOR oldsr oldsc) ++ (sET_BKGND_COLOR cLR_BLACK) ++ " "))) >>
          (♯ return record upW { snake = reverse (tail (reverse (World.snake upW))) }))) >>= λ reduceW →
  let over : ℕ
      over = if (⌊ srup Data.Nat.≟ 1 ⌋ ∨ ⌊ srup Data.Nat.≟ (fst` (World.limits reduceW)) ⌋ ∨
             ⌊ scup Data.Nat.≟ 1 ⌋ ∨ ⌊ scup Data.Nat.≟ (snd` (World.limits reduceW)) ⌋ ∨
             (any (λ xy → ⌊ (fst` xy) Data.Nat.≟ srup ⌋ ∧ ⌊ (snd` xy) Data.Nat.≟ scup ⌋) (tail (World.snake reduceW)))) then 2 else 0 in
  ♯ (♯ putStr ((sET_CURSOR srup scup) ++ (sET_BKGND_COLOR cLR_GREEN) ++
          " " ++ (sET_CURSOR srup scup) ++ sHOW_CURSOR) >>
  (if ⌊ over Data.Nat.≟ 2 ⌋ then ♯ return _ else ♯ mainLoop reduceW))))) })
  where
    genFood : World → IO.IO World
    genFood w =
      (♯ (return (RrandomR (0 , ((fst` (World.limits w)) * (snd` (World.limits w)))) (World.rand w)))) >>=
      (λ foodlocg →
       (let
        a : ℕ
        a = ((case (snd` (World.limits w)) of (λ {zero → zero; (suc x) → (Pair.fst`` foodlocg) div (suc x) })) + 1)
        b : ℕ
        b = ((case (snd` (World.limits w)) of (λ {zero → zero; (suc x) → toℕ ((Pair.fst`` foodlocg) mod (suc x)) })) + 1) in
      --let foodlocg : Pair ℕ RStdGen
      --    foodlocg = RrandomR (0 , ((fst` (World.limits w)) * (snd` (World.limits w)))) (World.rand w)
      --    foodloc : ℕ
      --    foodloc = Pair.fst`` foodlocg
      --    a : ℕ --need to use pattern matching to get the witness, not if-then-else
      --    a = (case (snd` (World.limits w)) of (λ {zero → zero; (suc x) → foodloc div (suc x) })) + 1
      --    b : ℕ
      --    b = (case (snd` (World.limits w)) of (λ {zero → zero; (suc x) → toℕ (foodloc mod (suc x)) })) + 1 in
           (if ⌊ a Data.Nat.≟ 1 ⌋ ∨ ⌊ a Data.Nat.≟ (snd` (World.limits w)) ⌋ ∨ ⌊ b Data.Nat.≟ 1 ⌋ ∨ ⌊ b Data.Nat.≟ (fst` (World.limits w)) ⌋ ∨
                 (any (λ xy → ⌊ a Data.Nat.≟ (fst` xy) ⌋ ∧ ⌊ b Data.Nat.≟ (snd` xy) ⌋) (World.snake w)) then (♯ (genFood record w { rand = Pair.snd`` foodlocg }))
                else ♯ return (record w { food = < a , b >; rand = Pair.snd`` foodlocg }))))

read : List Char → MaybeAgda ℕ
read [] = nothing
read ds = fromDigits ∘ reverse <$> mapM charToDigit ds
  where
  open RawMonad monad
  open Data.List.Categorical.TraversableM monad
  charToDigit : Char → MaybeAgda (Digit 10)
  charToDigit '0' = just (# 0)
  charToDigit '1' = just (# 1)
  charToDigit '2' = just (# 2)
  charToDigit '3' = just (# 3)
  charToDigit '4' = just (# 4)
  charToDigit '5' = just (# 5)
  charToDigit '6' = just (# 6)
  charToDigit '7' = just (# 7)
  charToDigit '8' = just (# 8)
  charToDigit '9' = just (# 9)
  charToDigit _   = nothing

{-# NON_TERMINATING #-}
waitForEsc : List Char → Bool → IO.IO (List Char)
waitForEsc s h = ♯ getCh >>= λ c →
  ♯ (if not h ∧ not ⌊ c ≟ '\x1b' ⌋ then waitForEsc [] h
  else if not ⌊ c ≟ 'R' ⌋ then waitForEsc (c ∷ s) true else return (c ∷ s))

fixPutStr : String → IO.IO ⊤
fixPutStr val = ♯ (putStr val) >>= λ _ → ♯ (return _)

main : _
main = run ((♯ initConsole) >>= λ ref → ♯ (sequence′ (
  lift (hSetBuffering stdin noBuffering) ∷
  ♯ (lift (hSetBuffering stdout noBuffering) ∷
  ♯ ((fixPutStr (cLEAR_SCREEN ++ hIDE_CURSOR ++ sET_MAX_CURSOR ++ gET_CURSOR_POS)) ∷
  ♯ ((♯ (waitForEsc [] false) >>=
     λ str → let strs : List (List Char)
                 strs = split (toList "[;R" ) (reverse str) in
       ♯ (♯ (♯ (♯ (♯ (lift getPOSIXTime) >>=
     λ tm → ♯ return record { snake = < maybe′ (λ x → x) 0 (read (head (tail strs) [])) div 2 , maybe′ (λ x → x) 0 (read (head (tail (tail strs)) [])) div 2 > ∷ [] ; food = < 0 , 0 > ; lastFood = 0 ; rand = RmkStdGen (round tm); direction = none ; limits = < maybe′ (λ x → x) 0 (read (head (tail strs) [])) , maybe′ (λ x → x) 0 (read (head (tail (tail strs)) [] )) > } ) >>=
     λ w → ♯ (drawBorder w)) >>= λ w → ♯ (mainLoop w)) >> ♯ return _)) ∷
  ♯ (fixPutStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR) ∷
  ♯ ((setConsole true ref) ∷ ♯ []))))))))
