#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "game.h"

int main(int argc, char* argv[])
{
	int charattr = CLR_WHITE << 3, cursorshow = 1; //bits 1-3=bold, blinking, reverse color, bits 4-6=fore color, bits 7-9= back color
	int curposx = 1, curposy = 1;
	if (argc == 3) {
		sscanf(argv[1], "%d", &height);	
		sscanf(argv[2], "%d", &width);
	} else {
		//argc = 3; height = 25; width = 80;
		setconsole(0);
		printf(GET_CURSOR_POS);
		while (getch() != 0x1b) {}
		char result[1+3+1+3+1+1];
		char* ptr = result;
		while ((*(ptr++) = getch()) != 'R') {}
		*ptr = 0;
		sscanf(result, "[%d;%dR", &curposy, &curposx);	
		get_screen_info();
		printf(SET_CURSOR_FMT, curposy, curposx);
	}
	struct charinfo
	{
		int utf8;
		char flag;
	};
	struct charinfo* buf = (struct charinfo*)calloc(width * height, sizeof(struct charinfo));
	while (1) {
		unsigned char ch = fgetc(stdin);
		if (ch == (unsigned char)EOF && !feof(stdin)) continue;
		if (ch == (unsigned char)EOF) break;
		else if (ch == 0x1b) {
			ch = fgetc(stdin);
			if (ch == '[') {
				char numbuf[256];
				char* ptr = numbuf;
				do {
					ch = fgetc(stdin);
					*(ptr++) = ch;
				} while (isdigit(ch));
				*ptr = 0;
				if (ch == 'A') { //cursor up
					if (curposy != 1) curposy--;
				} else if (ch == 'B') { //cursor down
					if (curposy != height) curposy++;
				} else if (ch == 'C') { //cursor right
					if (curposx != width) curposx++;
				} else if (ch == 'D') { //cursor left
					if (curposx != 1) curposx--;
				} else if (ch == 'J' && strcmp(numbuf, "2") == 0) { //clear screen
					for (int i = 0; i < width * height; i++) { buf[i].utf8 = ' '; buf[i].flag = charattr; }
				} else if (ch == 'K' && strcmp(numbuf, "2") == 0) { //clear line
					for (int i = 0; i < width; i++) { buf[(curposy-1) * width + i].utf8 = ' '; buf[(curposy-1) * width + i].flag = charattr; }
				} else if (ch == '?') {
					ptr = numbuf;
					do {
						ch = fgetc(stdin);
						*(ptr++) = ch;
					} while (isdigit(ch));
					*ptr = 0;
					if (ch == 'l' && strcmp(numbuf, "25") == 0) { //hide cursor
						cursorshow = 0;
					} else if (ch == 'h' && strcmp(numbuf, "25") == 0) { //show cursor
						cursorshow = 1;
					}
				} else if (ch == 'm') { //character attributes off=0, bold=1, blinking=5, reverse color=7, 3x=fore color, 4x=back color
					int num;
					sscanf(numbuf, "%d", &num);
					if (num == 0) charattr &= ~0x7;
					else if (num == 1) charattr |= 1;
					else if (num == 5) charattr |= 2;
					else if (num == 7) charattr |= 4;
					else if (num >= 30 && num <= 37) charattr = charattr & ~0x38 | ((num - 30) << 3);
					else if (num >= 40 && num <= 47) charattr = charattr & ~0x1C0 | ((num - 40) << 6);
				} else if (ch == 'n' && strcmp(numbuf, "6") == 0) { //get cursor position
					if (argc != 3) printf("\x1b[%u;%uR", curposy, curposx);
				} else if (ch == ';') {
					int num;
					sscanf(numbuf, "%d", &num);
					ptr = numbuf;
					do {
						ch = fgetc(stdin);
						*(ptr++) = ch;
					} while (isdigit(ch));
					*ptr = 0;
					if (ch == 'H') {
						curposy = num;
						sscanf(numbuf, "%d", &curposx);
						if (curposy == 0) curposy++;
						if (curposx == 0) curposx++;
						if (curposy > height) curposy = height;
						if (curposx > width) curposx = width;
					}
				}
			}
		} else {
			buf[(curposy-1) * width + curposx-1].flag = charattr;
#ifndef WIN32 //windows still uses codepages and not utf-8
			if (ch <= 0xBF) //check utf8 length but not validity
#endif
				buf[(curposy-1) * width + curposx-1].utf8 = ch == '\n' ? ' ' : ch;
#ifndef WIN32
			else if (ch >= 0xC0 && ch <= 0xDF)
				buf[(curposy-1) * width + curposx-1].utf8 = ch | (fgetc(stdin) << 8);
			else if (ch >= 0xE0 && ch <= 0xEF)
				buf[(curposy-1) * width + curposx-1].utf8 = ch | (fgetc(stdin) << 8) | (fgetc(stdin) << 16);
			else
				buf[(curposy-1) * width + curposx-1].utf8 = ch | (fgetc(stdin) << 8) | (fgetc(stdin) << 16) | (fgetc(stdin) << 24);
#endif
			if (ch == '\n' || curposx == width) {
				curposx = 1; if (curposy != height) curposy++;
			} else curposx++;
		}
	}
	if (argc == 3) {
		for (int i = 0; i < width * height; i++) {
			printf("%c", buf[i].utf8 == 0 ? ' ' : buf[i].utf8 & 0xFF);
	#ifndef WIN32
			if ((buf[i].utf8 & 0xFF) >= 0xC0)
				printf("%c", (buf[i].utf8 & 0xFF00) >> 8);
			if ((buf[i].utf8 & 0xFF) >= 0xE0)
				printf("%c", (buf[i].utf8 & 0xFF0000) >> 16);
			if ((buf[i].utf8 & 0xFF) >= 0xF0)
				printf("%c", (buf[i].utf8 & 0xFF000000) >> 24);
	#endif
			if ((i % width) == width-1) printf("\n");
		}
		printf("\n");
		for (int i = 0; i < width * height; i++) {
			printf("%c", '0' + ((buf[i].flag & 0x38) >> 3));
			if ((i % width) == width-1) printf("\n");
		}
		printf("\n");
		for (int i = 0; i < width * height; i++) {
			printf("%c", '0' + ((buf[i].flag & 0x1C0) >> 6));
			if ((i % width) == width-1) printf("\n");
		}
		printf("\n");
		for (int i = 0; i < width * height; i++) {
			printf("%c", '0' + (buf[i].flag & 0x7));
			if ((i % width) == width-1) printf("\n");
		}
  } else {
		printf(HIDE_CURSOR);
		for (int i = 0; i < width * height; i++) {
			if (buf[i].flag & 1) printf(BOLD_MODE_ON);
			if (buf[i].flag & 2) printf(BLINKING_MODE_ON);
			if (buf[i].flag & 4) printf(REVERSE_MODE_ON);
			printf(SET_CURSOR_FMT SET_FORE_COLOR_FMT SET_BKGND_COLOR_FMT "%c",
				(i / width)+1, (i % width)+1, (buf[i].flag & 0x38) >> 3,
				(buf[i].flag & 0x1C0) >> 6, buf[i].utf8 == 0 ? ' ' : buf[i].utf8 & 0xFF);
	#ifndef WIN32
			if ((buf[i].utf8 & 0xFF) >= 0xC0)
				printf("%c", (buf[i].utf8 & 0xFF00) >> 8);
			if ((buf[i].utf8 & 0xFF) >= 0xE0)
				printf("%c", (buf[i].utf8 & 0xFF0000) >> 16);
			if ((buf[i].utf8 & 0xFF) >= 0xF0)
				printf("%c", (buf[i].utf8 & 0xFF000000) >> 24);
	#endif
			if (buf[i].flag & 7) printf(TURN_OFF_CHAR_ATTR);
		}
		printf(SET_BKGND_COLOR(0) SET_FORE_COLOR(7) SET_CURSOR_FMT CLEAR_LINE SHOW_CURSOR, height, 0);
		setconsole(1);
  }
	free(buf);
}