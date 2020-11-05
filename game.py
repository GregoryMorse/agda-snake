import os
import sys

#def set_nonblock():
#  fd = sys.stdin.fileno()
#  if os.name != "nt" and os.isatty(fd):
#    import fcntl
#    fcntl.fcntl(fd, fcntl.F_SETFL, fcntl.fcntl(fd, fcntl.F_GETFL, 0) | os.O_NONBLOCK)
    
def setconsole(bRestore):
  if os.name == "nt":
    if not bRestore: os.system(" ") #VT100 in Windows
    import ctypes
    from ctypes import wintypes
    global h #prevent garbage collection
    def on_exit(event):
    	print((SET_BKGND_COLOR(0) + SET_FORE_COLOR(7) + SET_CURSOR_FMT + CLEAR_LINE + SHOW_CURSOR) % (height, 0))
    	setconsole(True, False)
    	return 0
    _kernel32 = ctypes.WinDLL('kernel32', use_last_error=True)
    _HandlerRoutine = ctypes.WINFUNCTYPE(wintypes.BOOL, wintypes.DWORD)
    _kernel32.SetConsoleCtrlHandler.argtypes = (_HandlerRoutine, wintypes.BOOL)
    h = _HandlerRoutine(on_exit)
    _kernel32.SetConsoleCtrlHandler(h, True)
    import subprocess
    hStdin = _kernel32.GetStdHandle(subprocess.STD_INPUT_HANDLE)
    ENABLE_LINE_INPUT      = 0x0002
    ENABLE_ECHO_INPUT      = 0x0004
    mode = wintypes.DWORD()
    _kernel32.GetConsoleMode(hStdin, ctypes.byref(mode))
    _kernel32.SetConsoleMode(hStdin, mode.value & (~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT)) if not bRestore else mode.value | ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT)
  else:
    def termination_handler(signum, frame):
      global height
      print((SET_BKGND_COLOR(0) + SET_FORE_COLOR(7) + SET_CURSOR_FMT + CLEAR_LINE + SHOW_CURSOR) % (height, 0))
      setconsole(True, False)
      sys.exit()
    fd = sys.stdin.fileno()
    import termios
    attr = [0,0,0,0] if not os.isatty(fd) else termios.tcgetattr(fd)[:]
    import fcntl
    CurFl = fcntl.fcntl(fd, fcntl.F_GETFL, 0)
    if not bRestore:
      import signal
      signal.signal(signal.SIGFPE, termination_handler)
      signal.signal(signal.SIGILL, termination_handler)
      signal.signal(signal.SIGSEGV, termination_handler)
      signal.signal(signal.SIGBUS, termination_handler)
      signal.signal(signal.SIGABRT, termination_handler)
      signal.signal(signal.SIGTRAP, termination_handler)
      signal.signal(signal.SIGSYS, termination_handler)
      signal.signal(signal.SIGINT, termination_handler)
      signal.signal(signal.SIGTERM, termination_handler)
      signal.signal(signal.SIGQUIT, termination_handler)
      signal.signal(signal.SIGHUP, termination_handler)
      attr[3] &= ~(termios.ICANON | termios.ECHO)
      #if nonblock and os.isatty(fd): fcntl.fcntl(fd, fcntl.F_SETFL, CurFl | os.O_NONBLOCK) # STDIN_FILENO=0, set to nonblocking reads
    else:
      attr[3] |= termios.ICANON | termios.ECHO
      #if (CurFl & os.O_NONBLOCK): fcntl.fcntl(fd, fcntl.F_SETFL, CurFl & ~os.O_NONBLOCK)
    if os.isatty(fd): termios.tcsetattr(fd, termios.TCSANOW, attr)

def _find_getch():
    try:
        import termios
    except ImportError:
        # Non-POSIX. Return msvcrt's (Windows') getch.
        def _getch():
            return sys.stdin.read(1)
        import msvcrt, sys
        return msvcrt.getch if os.isatty(sys.stdin.fileno()) else _getch

    # POSIX system. Create and return a getch that manipulates the tty.
    import sys
    def _getch():
        return sys.stdin.read(1)

    return _getch
def _find_kbhit():
    try:
        import termios
    except ImportError:
        # Non-POSIX. Return msvcrt's (Windows') kbhit.
        import msvcrt
        return msvcrt.kbhit

    # POSIX system. Create and return a kbhit that selects the tty.
    import sys, select
    def _kbhit():
        return select.select([sys.stdin,],[],[],0.0)[0]

    return _kbhit
	
def read():
  return sys.stdin.read(1) if not os.isatty(sys.stdin.fileno()) else (getch() if kbhit() else '\xff')
def CASE_ARROWKEYS(ch):
  return (ch != "" and ord(ch) == (0xe0 if os.name == "nt" else 0x1b)) and (os.name == "nt" or chr(ord(getch())) == '[')

getch = _find_getch()
kbhit = _find_kbhit()

ESCVT100 = "\x1b["
GET_CURSOR_POS = ESCVT100 + "6n"
def SET_CURSOR(x, y): return ESCVT100 + str(x) + ";" + str(y) + "H"
SET_CURSOR_FMT = ESCVT100 + "%u;%uH"
SET_MAX_CURSOR = SET_CURSOR(999, 999)
CLEAR_SCREEN = ESCVT100 + "2J"
CLEAR_LINE = ESCVT100 + "2K"
TURN_OFF_CHAR_ATTR = ESCVT100 + "0m"
BOLD_MODE_ON = ESCVT100 + "1m"
BLINKING_MODE_ON = ESCVT100 + "5m"
REVERSE_MODE_ON = ESCVT100 + "7m"
HIDE_CURSOR = ESCVT100 + "?25l"
SHOW_CURSOR = ESCVT100 + "?25h"
CLR_BLACK = 0
CLR_RED = 1
CLR_GREEN = 2
CLR_YELLOW = 3
CLR_BLUE = 4
CLR_MAGENTA = 5
CLR_CYAN = 6
CLR_WHITE = 7
def SET_FORE_COLOR(x): return ESCVT100 + "3" + str(x) + "m"
SET_FORE_COLOR_FMT = ESCVT100 + "3%um"
def SET_BKGND_COLOR(x): return ESCVT100 + "4" + str(x) + "m"
SET_BKGND_COLOR_FMT = ESCVT100 + "4%um"
UPARROW = 'H' if os.name == "nt" else 'A'
DOWNARROW = 'P' if os.name == "nt" else 'B'
RIGHTARROW = 'M' if os.name == "nt" else 'C'
LEFTARROW = 'K' if os.name == "nt" else 'D'

def get_screen_info():
  global height, width
  print(SET_MAX_CURSOR + GET_CURSOR_POS, end='')
  sys.stdout.flush()
  while (ord(getch()) != 0x1b): True
  result = []
  while (True):
	  result += chr(ord(getch()))
	  if result[-1] == 'R': break
  result = ''.join(result).replace("[", ";").replace("R", ";").split(";")[1:3]
  height = int(result[0])
  width = int(result[1])
  #sscanf(result, "[%d;%dR", &height, &width)