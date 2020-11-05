#pragma once
#include <stdio.h>
#include <stdlib.h>

#define ESCVT100 "\x1b[" //check infocmp vt100 and infocmp -Cr vt100
#define GET_CURSOR_POS ESCVT100 "6n"
#define SET_CURSOR(x, y) ESCVT100 #x ";" #y "H"
#define SET_CURSOR_FMT ESCVT100 "%u;%uH"
#define SET_MAX_CURSOR SET_CURSOR(999, 999)
#define CLEAR_SCREEN ESCVT100 "2J"
#define CLEAR_LINE ESCVT100 "2K"
#define TURN_OFF_CHAR_ATTR ESCVT100 "0m"
#define BOLD_MODE_ON ESCVT100 "1m"
#define BLINKING_MODE_ON ESCVT100 "5m"
#define REVERSE_MODE_ON ESCVT100 "7m"
#define HIDE_CURSOR ESCVT100 "?25l"
#define SHOW_CURSOR ESCVT100 "?25h"
#define CLR_BLACK 0
#define CLR_RED 1
#define CLR_GREEN 2
#define CLR_YELLOW 3
#define CLR_BLUE 4
#define CLR_MAGENTA 5
#define CLR_CYAN 6
#define CLR_WHITE 7
#define SET_FORE_COLOR(x) ESCVT100 "3" #x "m"
#define SET_FORE_COLOR_FMT ESCVT100 "3%um"
#define SET_BKGND_COLOR(x) ESCVT100 "4" #x "m"
#define SET_BKGND_COLOR_FMT ESCVT100 "4%um"
#define CURSOR_UP(x) ESCVT100 #x "A"
#define CURSOR_DOWN(x) ESCVT100 #x "B"
#define CURSOR_RIGHT(x) ESCVT100 #x "C"
#define CURSOR_LEFT(x) ESCVT100 #x "D"
#define SET_CURSOR_STYLE(x) ESCVT100 #x " q" //1 is blinking block (default), 3 is blinking underline, 5 is blinking bar

//Octaves are rows: DoublePedal, Pedal, Deep, Low, Middle, Tenor, High, DoubleHigh
//Notes are columns: C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B
const int Frequency[] = {
    16,17,18,19,21,22,23,24,26,27,29,31,
    33,35,37,39,41,44,46,49,52,55,58,62,
    65,69,73,78,82,87,92,98,104,110,116,123,
    131,139,147,155,165,175,185,196,208,220,233,245,
    262,277,294,311,330,349,370,392,415,440,466,494,
    523,554,587,622,659,698,740,784,831,880,932,988,
    1046,1109,1175,1244,1328,1397,1480,1568,1661,1760,1865,1975,
    2093,2217,2349,2489,2637,2794,2960,3136,3322,3520,3729,3951,
    22000 //pause
};
#define NOTE_C 0
#define NOTE_D 2
#define NOTE_E 4
#define NOTE_F 5
#define NOTE_G 7
#define NOTE_GS 8
#define NOTE_A 9
#define NOTE_B 11
#define NOTE_PAUSE 8
#define OCTAVE_LOW 3
#define OCTAVE_MID 4
#define OCTAVE_TNR 5

//Whole, Half, Quarter, Eighth, Sixteenth
const int Duration[] = { 1000, 500, 250, 125, 62 };
#define DUR_HALF 1
#define DUR_QUARTER 2
#define DUR_EIGHTH 3

int width, height;

int closethread = 0;

void (*termFunc)() = NULL;
void setconsole(int);
void restoreconsole() { setconsole(1); }
#ifdef USING_THREADS
void setconsolethread(int, void(*)());
void restoreconthread() { setconsolethread(1, NULL); }
#endif

#ifdef WIN32
//SET PATH=%ProgramFiles(x86)%\codeblocks\mingw\bin;%PATH%

#include <windows.h>

#ifdef USING_THREADS
HANDLE hThread = NULL;

DWORD WINAPI threadFunc(LPVOID lpParam)
{
	((void(*)())lpParam)();
	return 0;
}

void setconsolethread(int bRestore, void(*pFunc)()) {
	setconsole(bRestore);
	DWORD dwThreadId;
	if (!bRestore) {
		if (pFunc != NULL) hThread = CreateThread(NULL, 0, threadFunc, pFunc, 0, &dwThreadId);
			termFunc = restoreconthread;
	} else {
    	closethread = 1;
    	if (hThread != NULL) { WaitForSingleObject(hThread, INFINITE); CloseHandle(hThread); }
	}
}
#endif

BOOL WINAPI CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType) {        
    case CTRL_C_EVENT:// Handle the CTRL-C signal.
    case CTRL_CLOSE_EVENT:// CTRL-CLOSE: confirm that the user wants to exit.
    case CTRL_BREAK_EVENT:// Pass other signals to the next handler.
      //return TRUE;
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
    default:
    	if (termFunc != NULL) termFunc();
    	printf(SET_BKGND_COLOR(0) SET_FORE_COLOR(7) SET_CURSOR_FMT CLEAR_LINE SHOW_CURSOR, height, 0);
      return FALSE;
    }
}

void setconsole(int bRestore) {
	SetConsoleCtrlHandler(CtrlHandler, bRestore ? FALSE : TRUE);
	termFunc = restoreconsole;
	HANDLE hStdin = GetStdHandle(STD_INPUT_HANDLE); 
  DWORD mode = 0;
  GetConsoleMode(hStdin, &mode);
	if (!bRestore) {
		system(" "); // Start VT100 support
    SetConsoleMode(hStdin, mode & (~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT)));
	} else {
    SetConsoleMode(hStdin, mode | ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT);
	}
}

#include <conio.h>
#include <io.h>
//#define read(x,y,z) *(y) = x==0 && z==1 && kbhit() ? getch() : -1 //not stdin, underlying hardware - too low level for redirection unfortunately
//int nxChar = -1; //global variable to handle the double byte read conversion
//#define getch() nxChar == -1 ? (!_isatty(STDIN_FILENO) ? fgetc(stdin) : _getch()) : nxChar
#define getch() !_isatty(STDIN_FILENO) ? fgetc(stdin) : _getch()
//#define MAPVK_VK_TO_CHAR    2
//static int nxChar = -1; if (nxChar != -1) { *y = nxChar; nxChar = -1; return; }
//#define read(x,y,z) *y = !_isatty(STDIN_FILENO) ? fgetc(stdin) : (x==0 && z==1 && kbhit() ? getch() : -1)
//this code not only does not apparently work as it is apparently still keyboard bound, but the simpler solution is perfect here
	/*INPUT_RECORD rec; DWORD numRead = 0; HANDLE hnd = GetStdHandle(STD_INPUT_HANDLE); \
	*y = -1; nxChar = -1; GetNumberOfConsoleInputEvents(hnd, &numRead); \
	INPUT_RECORD* eventBuf = calloc(sizeof(INPUT_RECORD), numRead); \
	ReadConsoleInput(hnd, eventBuf, numRead, &numRead); \
	for (DWORD n = 0; n < numRead; n++) { if (eventBuf[n].EventType == KEY_EVENT && eventBuf[n].Event.KeyEvent.bKeyDown) { \
		*y = MapVirtualKey(eventBuf[n].Event.KeyEvent.wVirtualKeyCode, MAPVK_VK_TO_CHAR); \
		if (eventBuf[n].Event.KeyEvent.wVirtualKeyCode == VK_LEFT) { *y = 0xe0; nxChar = LEFTARROW; } \
		else if (eventBuf[n].Event.KeyEvent.wVirtualKeyCode == VK_RIGHT) { *y = 0xe0; nxChar = RIGHTARROW; } \
		else if (eventBuf[n].Event.KeyEvent.wVirtualKeyCode == VK_DOWN) { *y = 0xe0; nxChar = DOWNARROW; } \
		else if (eventBuf[n].Event.KeyEvent.wVirtualKeyCode == VK_UP) { *y = 0xe0; nxChar = UPARROW; } \
		else if (*y == 0) *y = eventBuf[n].Event.KeyEvent.wVirtualKeyCode;\
	} } \
	free(eventBuf); }*/
#define UPARROW 'H'
#define DOWNARROW 'P'
#define RIGHTARROW 'M'
#define LEFTARROW 'K'
#define CASE_ARROWKEYS case 0xe0:
//DOS code page 437
#define SPADE   "\x06"
#define CLUB    "\x05"
#define HEART   "\x03"
#define DIAMOND "\x04"
#define LIGHTSHADE "\xb0"
#define LTDBANGLE "\xae"
#define RTDBANGLE "\xaf" 
#else
#include <termios.h>
//#define _BSD_SOURCE
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <linux/kd.h>

#ifdef USING_THREADS
#include <pthread.h> //requires -lpthread in linker

pthread_t hThread = 0;

void* threadFunc(void* ptr)
{
	((void(*)())ptr)();
	return NULL;
}

void setconsolethread(int bRestore, void(*pFunc)())
{
	setconsole(bRestore);
	if (!bRestore) {
		int iret;
		if (pFunc != NULL) iret = pthread_create(&hThread, NULL, threadFunc, pFunc);
		termFunc = restoreconthread;
	}	else {
		closethread = 1;
		pthread_join(hThread, NULL);
	}
}
#endif

void termination_handler(int signum)
{
	if (termFunc != NULL) termFunc();
	printf(SET_BKGND_COLOR(0) SET_FORE_COLOR(7) SET_CURSOR_FMT CLEAR_LINE SHOW_CURSOR, height, 0);
	exit(-1);
}

void setconsole(int bRestore)
{
	struct termios term;
	tcgetattr(STDIN_FILENO, &term);
	int CurFl = fcntl(STDIN_FILENO, F_GETFL, 0);
	signal(SIGFPE, bRestore ? SIG_DFL : termination_handler);
	signal(SIGILL, bRestore ? SIG_DFL : termination_handler);
	signal(SIGSEGV, bRestore ? SIG_DFL : termination_handler);
	signal(SIGBUS, bRestore ? SIG_DFL : termination_handler);
	signal(SIGABRT, bRestore ? SIG_DFL : termination_handler);
	signal(SIGTRAP, bRestore ? SIG_DFL : termination_handler);
	signal(SIGSYS, bRestore ? SIG_DFL : termination_handler);
	signal(SIGINT, bRestore ? SIG_DFL : termination_handler);
	signal(SIGTERM, bRestore ? SIG_DFL : termination_handler);
	signal(SIGQUIT, bRestore ? SIG_DFL : termination_handler);
	signal(SIGHUP, bRestore ? SIG_DFL : termination_handler);
	if (!bRestore) {
		termFunc = restoreconsole;
		//if (nonblock && isatty(STDIN_FILENO)) fcntl(STDIN_FILENO, F_SETFL, CurFl | O_NONBLOCK); //STDIN_FILENO=0, set to nonblocking reads
		setvbuf(stdout, NULL, _IONBF, 0); //change stdout to unbuffered mode
		term.c_lflag &= ~(ICANON | ECHO);
	} else {
		
		//if (CurFl & O_NONBLOCK) fcntl(STDIN_FILENO, F_SETFL, CurFl & ~O_NONBLOCK);
		term.c_lflag |= ICANON | ECHO;
	} 
	if (isatty(STDIN_FILENO)) tcsetattr(STDIN_FILENO, TCSANOW, &term);
}
#define _isatty isatty
int kbhit() {
	struct timeval tv = {0, 0};
	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(STDIN_FILENO, &fds);
	select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
	return (FD_ISSET(0, &fds));
}
#define getch() fgetc(stdin)
#define Sleep(x) usleep(1000*x)
//#define Sleep(x) nanosleep((const struct timespec[]){{0, 1000000L*x}},NULL);
#define min(X,Y) ((X) < (Y) ? (X) : (Y))
//#include <curses.h>
#define UPARROW 'A'
#define DOWNARROW 'B'
#define RIGHTARROW 'C'
#define LEFTARROW 'D'
#define CASE_ARROWKEYS case 0x1b: getch() == '[';
//utf-8
#define SPADE   "\xE2\x99\xA0"
#define CLUB    "\xE2\x99\xA3"
#define HEART   "\xE2\x99\xA5"
#define DIAMOND "\xE2\x99\xA6"
#define LIGHTSHADE "\xE2\x96\x92" //"\xE2\x96\x91"
#define LTDBANGLE "\xC2\xAB"
#define RTDBANGLE "\xC2\xBB"

#include <linux/input.h>

#define CLOCK_TICK_RATE 1193180 //PC mainboard timer 8254 clocked at this precise rate
//sudo chown root <file>, sudo chgrp root <file>, sudo chmod 4755 root <file> are needed for permission to beep
void Beep(int freq, int Dur) {
	int console_fd = STDOUT_FILENO;
	//if (ioctl(console_fd, KDMKTONE, 0))
	//	console_fd = open("/dev/console", O_WRONLY);
	//if (console_fd == -1)
		console_fd = open("/dev/tty0", O_WRONLY); //beep uses "/dev/console" while newer versions "/dev/tty0" and "/dev/vc/0"
	if (console_fd == -1)
		console_fd = open("/dev/vc/0", O_WRONLY);
	//if (console_fd == -1)
		//console_fd = open("/dev/tty", O_WRONLY); //STDOUT_FILENO and "/dev/tty" have permission but are inappropriate
	if (console_fd != -1) {
		if (ioctl(console_fd, EVIOCGSND(0)) != -1) {
			struct input_event e;
			e.type = EV_SND;
			e.code = SND_TONE;
			e.value = freq;
			write(console_fd, &e, sizeof(struct input_event));
		} else {
			ioctl(console_fd, KIOCSOUND, freq == 0 ? freq : (int)(CLOCK_TICK_RATE/freq));
			Sleep(Dur);
			//ioctl(console_fd, KDMKTONE, (freq == 0 ? freq : (int)(CLOCK_TICK_RATE/freq)) | (Dur << 16)); //synchronous for some reason does not work
		}
		if (console_fd != STDOUT_FILENO && console_fd != -1) close(console_fd);
	}// else printf("\a");
}
#endif

void get_screen_info()
{
	printf(SET_MAX_CURSOR GET_CURSOR_POS);

	while (fgetc(stdin) != 0x1b) {}
	char result[1+3+1+3+1+1];
	char* ptr = result;
	while ((*(ptr++) = fgetc(stdin)) != 'R') {}
	*ptr = 0;
	sscanf(result, "[%d;%dR", &height, &width);	
}