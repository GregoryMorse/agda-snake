--cd /D D:\OneDrive\Documents\Projects\ELTE\agda
--agda --ghc --ghc-flag=-cpp --ghc-flag=-threaded emuterm.agda
--agda --js emuterm.agda --compile-dir=emuterm

module emuterm where

open import IO using (run; lift; _>>=_; _>>_; return; putStr; sequence′; mapM′; IO)
open import Codata.Musical.Notation using (∞; ♯_; ♭)
open import Data.Bool using (Bool; false; true; not; _∧_; _∨_) -- _≟_)
open import Data.Bool.Base using (if_then_else_)
open import Relation.Binary.PropositionalEquality public using (_≡_; refl; _≢_)
open import Relation.Nullary.Decidable using (⌊_⌋)
open import Data.Nat.DivMod using (_div_; _mod_)
open import Data.Unit using (⊤)
open import Data.Unit.Polymorphic using () renaming (⊤ to Unit⊤)
open import Codata.Musical.Colist using (Colist; []; _∷_; map)
open import Data.List using (List; span; reverse; []; _∷_; any; length)
open import Data.List.Categorical using (monadT)
open import Codata.Musical.Costring using (Costring; toCostring)
open import Data.String using (String; fromList; toList; _==_; _++_)
open import Function using (_∘_; case_of_)
open import Category.Monad using (RawMonad)
open import Data.Maybe.Categorical using (monad)
open import Data.Maybe.Base using (nothing; just; maybe′) renaming (Maybe to MaybeAgda)
open import Data.Digit using (Digit; fromDigits)
open import Data.Fin using (#_; toℕ)
open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _*_) -- _≟_)
open import Data.Nat.Show using (show)
open import Data.Char using (Char; _≟_)
open import Data.Char.Base using (isDigit)
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
  instance IntegralNat : Integral ℕ
  toInteger : {a : Set} {{_ : Integral a}} → a → ℕ
  --fromEnum : CInt → Int
  Enum : Set → Set
  instance EnumCInt : Enum CInt
  fromEnum : {a : Set} {{_ : Enum a}} → a → Int
  
  FileHandle : Set
  stdin : FileHandle
  stdout : FileHandle
  hGetChar : FileHandle → Agda.Builtin.IO.IO Char
  hSetBuffering : FileHandle → BufferMode → Agda.Builtin.IO.IO ⊤
  hSetEcho : FileHandle → Bool → Agda.Builtin.IO.IO ⊤
  hIsTerminalDevice : FileHandle → Agda.Builtin.IO.IO Bool
  isEOF : Agda.Builtin.IO.IO Bool
  POSIXTime : Set
  instance RealFracPOSIXTime : RealFrac POSIXTime
  getPOSIXTime : Agda.Builtin.IO.IO POSIXTime
--  round : POSIXTime → Int
  round : {a b : Set} {{_ : RealFrac a}} {{_ : Integral b}} → a → b
  threadDelay : Int → Agda.Builtin.IO.IO ⊤
  os : Costring
  getArgs′ : Agda.Builtin.IO.IO (List Costring)

{-# COMPILE GHC RealFrac = type RealFracDict #-}
{-# COMPILE GHC Integral = type IntegralDict #-}
{-# COMPILE GHC toInteger = \ _ IntegralDict -> toInteger #-}
{-# COMPILE GHC IntegralInt = IntegralDict #-}
{-# COMPILE GHC IntegralNat = IntegralDict #-}
--{-# COMPILE GHC toInteger = toInteger #-}
{-# COMPILE GHC Enum = type EnumDict #-}
{-# COMPILE GHC EnumCInt = EnumDict #-}
--{-# COMPILE GHC fromEnum = fromEnum #-}
{-# COMPILE GHC fromEnum = \ _ EnumDict -> fromEnum #-}
{-# COMPILE GHC Pair = data AgdaPair ((,)) #-}
--Handle is needed even though not in Haskell due to the signatures here
{-# COMPILE GHC FileHandle = type IO.Handle #-}
{-# COMPILE GHC isEOF = IO.isEOF #-}
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
{-# COMPILE GHC getArgs′ = fmap (\ x -> map toColist x) System.Environment.getArgs #-}

{-# FOREIGN GHC
import qualified System.Environment (getArgs)
import qualified System.Info (os)
import qualified System.IO as IO (Handle, hIsTerminalDevice, hSetEcho, hSetBuffering, hGetChar, BufferMode (NoBuffering, LineBuffering, BlockBuffering), isEOF, stdin, stdout)
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
getCh = if isWin then ((♯ (lift (hIsTerminalDevice stdin))) >>= λ isTerm → ♯ (if isTerm then (♯ (lift c_getch)) >>= (λ ch → ♯ return (chr (fromEnum ch))) else (lift (hGetChar stdin)))) else lift (hGetChar stdin)
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
tURN_OFF_CHAR_ATTR = eSCVT100 ++ "0m"
bOLD_MODE_ON : String
bOLD_MODE_ON = eSCVT100 ++ "1m"
bLINKING_MODE_ON = eSCVT100 ++ "5m"
rEVERSE_MODE_ON = eSCVT100 ++ "7m"
cLR_BLACK  = 0
cLR_RED = 1
cLR_GREEN = 2
cLR_YELLOW = 3
cLR_BLUE = 4
cLR_MAGENTA = 5
cLR_CYAN = 6
cLR_WHITE = 7
sET_FORE_COLOR : ℕ -> String
sET_FORE_COLOR x = eSCVT100 ++ "3" ++ show x ++ "m"
sET_BKGND_COLOR : ℕ → String
sET_BKGND_COLOR x = eSCVT100 ++ "4" ++ show x ++ "m"
hIDE_CURSOR : String
hIDE_CURSOR = eSCVT100 ++ "?25l"
sHOW_CURSOR : String
sHOW_CURSOR = eSCVT100 ++ "?25h"

Position : Set
Position = ℕ × ℕ

CharInfo : Set
CharInfo = List Position --utf8, flag

record World : Set where
  field
    buf : CharInfo
    curPos : Position
    cursorShow : Bool
    charAttr : ℕ
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
    posixhandler tid ref = run (♯ (♯ ((♯ putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR)) >> (♯ setConsole true ref)) >> ♯ (lift (throwTo tid (exitFailure (fromInteger 0))))) >> ♯ return _)

handler : IORef PHANDLER_ROUTINE → CtrlEvent → Agda.Builtin.IO.IO Bool
handler ref _ = --IO.Primitive.return false
  run ((♯ (setConsole true ref)) >> ♯ ((♯ (putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR))) >>
  (♯ return false) ))
--  run (♯ ((♯ (putStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR))) >> (♯ (setConsole true ref))) >>
--  ♯ return false)

initConsole : IO.IO (IORef PHANDLER_ROUTINE)
initConsole = (♯ lift (newIORef nullFunPtr)) >>=
  (λ ref → ♯ ((♯ (lift (mkHandler (handler ref)))) >>=
  (λ r → ♯ ((♯ lift (writeIORef ref r)) >> 
  ♯ ((♯ (setConsole false ref)) >> (♯ return ref))))))

head : ∀ {a} {A : Set a} → List A → A → A
head []       def = def
head (x ∷ xs) _   = x

--e.g. drop 1
tail : ∀ {a} {A : Set a} → List A → List A
tail []       = []
tail (x ∷ xs) = xs

_!!_!_ : ∀ {a} {A : Set a} → List A → ℕ → A → A
_!!_!_ [] _ def = def
_!!_!_ (x ∷ xs) 0 _ = x
_!!_!_ (x ∷ xs) (suc n) def = xs !! n ! def

{-# TERMINATING #-}
split : List Char → List Char → List (List Char)
split d [] = []
split d s = (fst x×y) ∷ (split d (tail (snd x×y)))
  where
    x×y : Σ (List Char) (λ x → List Char)
    x×y = span (λ l → (any (λ c → ⌊ c ≟ l ⌋ ) d) Data.Bool.≟ false) s

complement : ℕ → ℕ → ℕ
complement _ 0 = 0
complement zero (suc n) = 1 + 2 * (complement zero n)
complement (suc m) (suc n) = (if ⌊ toℕ ((suc m) mod 2) Data.Nat.≟ 1 ⌋ then 0 else 1) + 2 * (complement ((suc m) div 2) n)

_shiftL_ : ℕ → ℕ → ℕ
_shiftL_ a zero = a
_shiftL_ a (suc n) = (a * 2) shiftL n

_shiftR_ : ℕ → ℕ → ℕ
_shiftR_ a zero = a
_shiftR_ a (suc n) = (a div 2) shiftR n

{-# TERMINATING #-}
_&&_ : ℕ → ℕ → ℕ
_&&_ a zero = zero
_&&_ zero b = zero
_&&_ (suc m) (suc n) = (if ⌊ toℕ ((suc m) mod 2) Data.Nat.≟ 1 ⌋ ∧ ⌊ toℕ ((suc n) mod 2) Data.Nat.≟ 1 ⌋ then 1 else 0) + 2 * (((suc m) div 2) && ((suc n) div 2))

{-# TERMINATING #-}
_||_ : ℕ → ℕ → ℕ
_||_ a zero = a
_||_ zero b = b
_||_ (suc m) (suc n) = (if ⌊ toℕ ((suc m) mod 2) Data.Nat.≟ 1 ⌋ ∨ ⌊ toℕ ((suc n) mod 2) Data.Nat.≟ 1 ⌋ then 1 else 0) + 2 * (((suc m) div 2) || ((suc n) div 2))

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
waitForNonDigit : List Char → IO.IO (Char × (List Char))
waitForNonDigit s = do
  c ← ♯ getCh
  if (isDigit c) then ♯ (waitForNonDigit (c ∷ s)) else ♯ (return < c , s >)

{-# NON_TERMINATING #-}
mainLoop : World → IO.IO World
mainLoop w = do
  eof ← ♯ (lift isEOF)
  do if eof then (♯ return w) else (♯ do
      ch ← ♯ getCh
      ♯ do
        nextW ← (if (⌊ ch ≟ '\x1b' ⌋) then ♯ (do
          c ← ♯ getCh
          if (⌊ c ≟ '[' ⌋) then (♯ do
                < nc , numbuf > ← ♯ (waitForNonDigit [])
                if (⌊ nc ≟ 'A' ⌋) then --cursor up
                  ♯ return (if (not ⌊ (fst` (World.curPos w)) Data.Nat.≟ 1 ⌋) then record w { curPos = < (fst` (World.curPos w)) ∸ 1 , snd` (World.curPos w) > } else w)
                  else if (⌊ nc ≟ 'B' ⌋) then --cursor down
                  ♯ return (if (not ⌊ (fst` (World.curPos w)) Data.Nat.≟ 1 ⌋) then record w { curPos = < (fst` (World.curPos w)) + 1 , snd` (World.curPos w) > } else w)
                  else if (⌊ nc ≟ 'C' ⌋) then --cursor right
                  ♯ return (if (not ⌊ (fst` (World.curPos w)) Data.Nat.≟ 1 ⌋) then record w { curPos = < fst` (World.curPos w) , (snd` (World.curPos w)) + 1 > } else w)
                  else if (⌊ nc ≟ 'D' ⌋) then --cursor left
                  ♯ return (if (not ⌊ (fst` (World.curPos w)) Data.Nat.≟ 1 ⌋) then record w { curPos = < fst` (World.curPos w) , (snd` (World.curPos w)) ∸ 1 > } else w)
                  else if (⌊ nc ≟ 'J' ⌋ ∧ ((Data.String.fromList numbuf) == (Data.String.fromList ('2' ∷ [])))) then --clear screen
                  ♯ return record w { buf = Data.List.map (λ _ → < Data.Char.toℕ ' ' , World.charAttr w >) (World.buf w) }
                  else if (⌊ nc ≟ 'K' ⌋ ∧ ((Data.String.fromList numbuf) == (Data.String.fromList ('2' ∷ [])))) then --clear line
                  (let ht : _
                       ht = Data.List.splitAt (((fst` (World.curPos w)) ∸ 1) * (snd` (World.limits w))) (World.buf w)
                       snip : _
                       snip = Data.List.splitAt (snd` (World.limits w)) (snd ht) in
                  ♯ return record w { buf = fst ht Data.List.++ (Data.List.map (λ _ → < Data.Char.toℕ ' ' , World.charAttr w >) (fst snip)) Data.List.++ (snd snip) })
                  else if (⌊ nc ≟ '?' ⌋) then ♯ (do
                    nn ← ♯ (waitForNonDigit [])
                    ♯ (let nnc : _
                           nnc = fst` nn
                           nnumbuf : _
                           nnumbuf = snd` nn in
                      (if (⌊ nnc ≟ 'l' ⌋ ∧ ((Data.String.fromList (reverse nnumbuf)) == (Data.String.fromList ('2' ∷ '5' ∷ [])))) then --hide cursor
                        return record w { cursorShow = false }
                      else if (⌊ nnc ≟ 'h' ⌋ ∧ ((Data.String.fromList (reverse nnumbuf)) == (Data.String.fromList ('2' ∷ '5' ∷ [])))) then --show cursor
                        return record w { cursorShow = true }
                      else
                        return w)))
                  else if (⌊ nc ≟ 'm' ⌋) then --character attributes off=0, bold=1, blinking=5, reverse color=7, 3x=fore color, 4x=back color
                  (let num : _
                       num = maybe′ (λ x → x) 0 (read (reverse numbuf)) in
                  ♯ return record w { charAttr =
                    if (⌊ num Data.Nat.≟ 0 ⌋) then (World.charAttr w) && (complement 0x7 64)
                    else if (⌊ num Data.Nat.≟ 1 ⌋) then (World.charAttr w) || 1
                    else if (⌊ num Data.Nat.≟ 1 ⌋) then (World.charAttr w) || 1
                    else if (⌊ num Data.Nat.≟ 5 ⌋) then (World.charAttr w) || 2
                    else if (⌊ num Data.Nat.≟ 7 ⌋) then (World.charAttr w) || 4
                    else if (⌊ 30 Data.Nat.≤? num ⌋ ∧ ⌊ num Data.Nat.≤? 37 ⌋) then ((World.charAttr w) && (complement 0x38 64)) || ((num ∸ 30) shiftL 3)
                    else if (⌊ 40 Data.Nat.≤? num ⌋ ∧ ⌊ num Data.Nat.≤? 47 ⌋) then ((World.charAttr w) && (complement 0x1C0 64)) || ((num ∸ 40) shiftL 6)
                    else World.charAttr w })
                  else if (⌊ nc ≟ 'n' ⌋ ∧ ((Data.String.fromList numbuf) == (Data.String.fromList ('6' ∷ [])))) then ♯ (do --get cursor position
                    ♯ putStr("\x1b[" ++ (show (fst` (World.curPos w))) ++ ";" ++ (show (snd` (World.curPos w))) ++ "R")
                    ♯ return w)
                  else if (⌊ nc ≟ ';' ⌋) then ♯ (do
                    nn ← ♯ (waitForNonDigit [])
                    (let nnc : _
                         nnc = fst` nn
                         nnumbuf : _
                         nnumbuf = snd` nn in
                     (if (⌊ nnc ≟ 'H' ⌋) then
                        (let mW : _
                             mW = record w { curPos = < maybe′ (λ x → x) 0 (read (reverse numbuf)) , maybe′ (λ x → x) 0 (read (reverse nnumbuf)) > }
                             mW1 : _
                             mW1 = if (⌊ (fst` (World.curPos mW)) Data.Nat.≟ 0 ⌋) then record mW { curPos = < (fst` (World.curPos mW)) + 1 , snd` (World.curPos mW) > } else mW
                             mW2 : _
                             mW2 = if (⌊ (snd` (World.curPos mW1)) Data.Nat.≟ 0 ⌋ ) then record mW1 { curPos = < fst` (World.curPos mW1) , (snd` (World.curPos mW1)) + 1 > } else mW1
                             mW3 : _
                             mW3 = if (not ⌊ (fst` (World.curPos mW2)) Data.Nat.≤? (fst` (World.limits w)) ⌋) then record mW2 { curPos = < fst` (World.limits w) , snd` (World.curPos mW2) > } else mW2
                             mW4 : _
                             mW4 = if (not ⌊ (snd` (World.curPos mW3)) Data.Nat.≤? (snd` (World.limits w)) ⌋) then record mW3 { curPos = < fst` (World.curPos mW3) , snd` (World.limits w) > } else mW3 in
                        ♯ return mW4)
                     else
                       ♯ return w)))
                  else ♯ return w)
              else ♯ return w)
          else
            let sp : _
                sp = Data.List.splitAt (((fst` (World.curPos w)) ∸ 1) * (snd` (World.limits w)) + (snd` (World.curPos w)) ∸ 1) (World.buf w) in ♯ (do
            chW ← if (isWin ∨ ⌊ (Data.Char.toℕ ch) Data.Nat.≤? 0xBF ⌋) then ♯ return record w { buf = (fst sp) Data.List.++ (< Data.Char.toℕ (if ⌊ ch ≟ '\n' ⌋ then ' ' else ch) , World.charAttr w > ∷ (tail (snd sp))) }
                    else if ⌊ 0xC0 Data.Nat.≤? (Data.Char.toℕ ch) ⌋ ∧ ⌊ (Data.Char.toℕ ch) Data.Nat.≤? 0xDF ⌋ then ♯ (do
                      cb ← ♯ getCh
                      ♯ return record w { buf = (fst sp) Data.List.++ (< (Data.Char.toℕ ch) || ((Data.Char.toℕ cb) shiftL 8) , World.charAttr w > ∷ (tail (snd sp))) })
                    else if ⌊ 0xE0 Data.Nat.≤? (Data.Char.toℕ ch) ⌋ ∧ ⌊ (Data.Char.toℕ ch) Data.Nat.≤? 0xEF ⌋ then ♯ (do
                      cb ← ♯ getCh
                      ♯ (do
                        cc ← ♯ getCh
                        ♯ return record w { buf = (fst sp) Data.List.++ (< (Data.Char.toℕ ch) || (((Data.Char.toℕ cb) shiftL 8) || ((Data.Char.toℕ cc) shiftL 16)) , World.charAttr w > ∷ (tail (snd sp))) }))
                    else ♯ (do
                      cb ← ♯ getCh
                      ♯ (do
                        cc ← ♯ getCh
                        ♯ (do
                          cd ← ♯ getCh
                          ♯ return record w { buf = (fst sp) Data.List.++ (< (Data.Char.toℕ ch) || (((Data.Char.toℕ cb) shiftL 8) || (((Data.Char.toℕ cc) shiftL 16) || ((Data.Char.toℕ cd) shiftL 24))) , World.charAttr w > ∷ (tail (snd sp))) })))
            if ((⌊ ch ≟ '\n' ⌋) ∨ (⌊ (snd` (World.curPos w)) Data.Nat.≟ (snd` (World.limits w)) ⌋)) then
               (let nW : _
                    nW = record chW { curPos = < snd` (World.curPos w) , 1 > }
                    nW1 : _
                    nW1 = if not ⌊ (fst` (World.curPos nW)) Data.Nat.≟ (fst` (World.limits w)) ⌋ then record nW { curPos = < (snd` (World.curPos nW)) + 1 , fst` (World.curPos nW) > } else nW in
               ♯ return nW1)
              else ♯ return record chW { curPos = < fst` (World.curPos w) , (snd` (World.curPos w)) + 1 > }))
        ♯ mainLoop nextW)

{-# TERMINATING #-}
drawOutput : World → ℕ → IO.IO ⊤
drawOutput w i = if ⌊ i Data.Nat.≟ snd` (World.limits w) * fst` (World.limits w) ⌋ then return _ else do
  ♯ (if not ⌊ ((snd` ((World.buf w) !! i ! < 0 , 0 >)) && 1) Data.Nat.≟ 0 ⌋ then (putStr bOLD_MODE_ON) else (return _))
  ♯ do
    ♯ (if not ⌊ ((snd` ((World.buf w) !! i ! < 0 , 0 >)) && 2) Data.Nat.≟ 0 ⌋ then (putStr bLINKING_MODE_ON) else (return _))
    ♯ do
      ♯ (if not ⌊ ((snd` ((World.buf w) !! i ! < 0 , 0 >)) && 4) Data.Nat.≟ 0 ⌋ then (putStr rEVERSE_MODE_ON) else (return _))
      ♯ do
        ♯ putStr ((sET_CURSOR (case (snd` (World.limits w)) of (λ { zero → 1; (suc x) → ((i div (suc x)) + 1) })) (case (snd` (World.limits w)) of (λ { zero → 1; (suc x) →  ((toℕ (i mod (suc x))) + 1) }))) ++ (sET_FORE_COLOR (toInteger ((snd` ((World.buf w) !! i ! < 0 , 0 >) && 0x38) shiftR 3))) ++ (sET_BKGND_COLOR (toInteger ((snd` ((World.buf w) !! i ! < 0 , 0 >) && 0x1C0) shiftR 6))) ++ Data.String.fromList ((if ⌊ (fst` ((World.buf w) !! i ! < 0 , 0 >)) Data.Nat.≟ 0 ⌋ then ' ' else chr (fromInteger ((fst` ((World.buf w) !! i ! < 0 , 0 >)) && 255))) ∷ []))
        ♯ do
          if (not isWin ∧ ⌊ 0xC0 Data.Nat.≤? (fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF ⌋) then ♯ putStr (Data.String.fromList (Data.Char.fromℕ (((fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF00) shiftR 8) ∷ [])) else ♯ return _
          ♯ do
            if (not isWin ∧ ⌊ 0xE0 Data.Nat.≤? (fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF ⌋) then ♯ putStr (Data.String.fromList (Data.Char.fromℕ (((fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF0000) shiftR 16) ∷ [])) else ♯ return _
            ♯ do
              if (not isWin ∧ ⌊ 0xF0 Data.Nat.≤? (fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF ⌋) then ♯ putStr (Data.String.fromList (Data.Char.fromℕ (((fst` ((World.buf w) !! i ! < 0 , 0 >)) && 0xFF000000) shiftR 24) ∷ [])) else ♯ return _
              ♯ do
                ♯ (if not ⌊ ((snd` ((World.buf w) !! i ! < 0 , 0 >)) && 7) Data.Nat.≟ 0 ⌋ then (putStr tURN_OFF_CHAR_ATTR) else (return _))
                ♯ drawOutput w (i + 1)

getArgs : IO.IO (List String)
getArgs = ♯ lift getArgs′ >>= (λ l → ♯ return (Data.List.map (λ x → Data.String.fromList (reverse (Data.String.toList (getStr (rev x))))) l))
  where open IO.IO

{-# NON_TERMINATING #-}
waitForEsc : List Char → Bool → IO.IO (List Char)
waitForEsc s h = ♯ getCh >>= λ c →
  ♯ (if not h ∧ not ⌊ c ≟ '\x1b' ⌋ then waitForEsc [] h
  else if not ⌊ c ≟ 'R' ⌋ then waitForEsc (c ∷ s) true else return (c ∷ s))

duplicate : {A : Set} → A → ℕ → List A
duplicate _ zero = []
duplicate val (suc n) = val ∷ (duplicate val n)

fixPutStr : String → IO.IO ⊤
fixPutStr val = ♯ (putStr val) >>= λ _ → ♯ (return _)

main : _
main = run (((♯ getArgs) >>= λ args →
  ♯ ((♯ initConsole) >>= λ ref → ♯ (sequence′ (
  lift (hSetBuffering stdin noBuffering) ∷
  ♯ (lift (hSetBuffering stdout noBuffering) ∷
  ♯ (((if ⌊ length args Data.Nat.≟ 2 ⌋ then ♯ return < < 1 , 1 > , < maybe′ (λ x → x) 0 (read (Data.String.toList (head args ""))) , maybe′ (λ x → x) 0 (read (Data.String.toList (head (tail args) ""))) > > else ♯ do
    ♯ putStr gET_CURSOR_POS
    ♯ do
      st ← ♯ waitForEsc [] false
      ♯ do
        ♯ putStr (sET_MAX_CURSOR ++ gET_CURSOR_POS)
        ♯ do
          str ← ♯ waitForEsc [] false
          (let ss : _
               ss = split (toList "[;R") (reverse st)
               strs : List (List Char)
               strs = split (toList "[;R") (reverse str) in
           ♯ return < < maybe′ (λ x → x) 0 (read (head (tail ss) [])) , maybe′ (λ x → x) 0 (read (head (tail (tail ss)) [] )) > , < maybe′ (λ x → x) 0 (read (head (tail strs) [])) , maybe′ (λ x → x) 0 (read (head (tail (tail strs)) [] )) > >)) >>=
     λ cycxhw → let curposy : _
                    curposy = fst` (fst` cycxhw)
                    curposx : _
                    curposx = snd` (fst` cycxhw)
                    height : _
                    height = fst` (snd` cycxhw)
                    width : _
                    width = snd` (snd` cycxhw) in
       ♯ (♯ (♯ (♯ (♯ (lift getPOSIXTime) >>=
     λ tm → ♯ return record { buf = duplicate < 0 , 0 > (height * width) ; curPos = < curposy , curposx > ; charAttr = cLR_WHITE shiftL 3 ; cursorShow = true ; limits = < height , width > } ) >>=
     λ w → ♯ (mainLoop w)) >>= λ w → ♯ (♯ putStr hIDE_CURSOR >> ♯ (drawOutput w 0))) >> ♯ return _)) ∷
  ♯ (fixPutStr ((sET_BKGND_COLOR cLR_BLACK) ++ (sET_FORE_COLOR cLR_WHITE) ++ (sET_CURSOR 999 999) ++ "\n" ++ sHOW_CURSOR) ∷
  ♯ ((setConsole true ref) ∷ ♯ [])))))))))
