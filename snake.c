#include "game.h"

#include <stdio.h>
#include <stdlib.h> //for abs, rand
#include <math.h> //for sqrt, must compile with -lm
#include <time.h>

int snake()
{
	struct position {
		int x;
		int y;
	};
	struct position* snake = (struct position*)calloc(sizeof(struct position), width * height);
	int i, j, snakelen = 1, dir = 0;
	snake[0].x = width / 2;
	snake[0].y = height / 2;
	printf(SET_CURSOR_STYLE(2) SET_BKGND_COLOR(0) CLEAR_SCREEN);
	int over = 0, lastfood = 0, foodloc = 0;
	time_t t; srand((unsigned)time(&t));
	for (i = 1; i <= height; i += 1) {
		printf(SET_CURSOR_FMT SET_BKGND_COLOR(7) " ", i, 1);
		printf(SET_CURSOR_FMT SET_BKGND_COLOR(7) " ", i, width);
	}
	for (i = 1 + 1; i <= width - 1; i += 1) {
		printf(SET_CURSOR_FMT SET_BKGND_COLOR(7) " ", 1, i);
		printf(SET_CURSOR_FMT SET_BKGND_COLOR(7) " ", height, i);
	}
	printf(SET_BKGND_COLOR(2) SET_CURSOR_FMT " " SET_CURSOR_FMT, snake[0].y, snake[0].x, snake[0].y, snake[0].x);
	do {
		int ch = 0; // = getch();
		//if (_isatty(STDIN_FILENO)) read(STDIN_FILENO, &ch, 1); else ch = getch();
		ch = !_isatty(STDIN_FILENO) ? fgetc(stdin) : (kbhit() ? getch() : -1);
		switch (ch) {
		CASE_ARROWKEYS
			switch (getch()) {
			case UPARROW: if (dir != 2 || snakelen == 1) dir = 1; break;
			case DOWNARROW: if (dir != 1 || snakelen == 1) dir = 2; break;
			case RIGHTARROW: if (dir != 4 || snakelen == 1) dir = 3; break;
			case LEFTARROW: if (dir != 3 || snakelen == 1) dir = 4; break;
			}
			break;
		}
		if (ch == -1) Sleep(100);
		if (dir == 0) continue;
		lastfood++;
		if (lastfood == 50 && foodloc == 0) {
			do {
				foodloc = rand() % (width * height);
				if (foodloc % width == 0 || foodloc % width == width - 1 || foodloc / width == 0 || foodloc / width == height - 1) continue;
				for (i = 0; i < snakelen; i++)
					if (snake[i].x == foodloc % width +1 && snake[i].y == foodloc / width +1) break;
			} while (i != snakelen);
			printf(HIDE_CURSOR SET_CURSOR_FMT SET_BKGND_COLOR(3) " ", foodloc / width +1, foodloc % width +1);
		}
		for (i = snakelen; i > 0; i--) snake[i] = snake[i - 1];
		if (dir == 1) snake[0].y--;
		else if (dir == 2) snake[0].y++;
		else if (dir == 3) snake[0].x++;
		else if (dir == 4) snake[0].x--;
		for (i = snakelen; i > 0; i--) if (snake[i].x == snake[0].x && snake[i].y == snake[0].y) over = 2; //snake ran into itself
		if (snake[0].x == foodloc % width +1 && snake[0].y == foodloc / width + 1) { snakelen++; lastfood = 0; foodloc = 0; }
		else printf(HIDE_CURSOR SET_CURSOR_FMT SET_BKGND_COLOR(0) " ", snake[snakelen].y, snake[snakelen].x);
		printf(SET_CURSOR_FMT SET_BKGND_COLOR(2) " " SET_CURSOR_FMT SHOW_CURSOR, snake[0].y, snake[0].x, snake[0].y, snake[0].x);
		if (snake[0].y == 1 || snake[0].y == height || snake[0].x == 1 || snake[0].x == width) over = 2;
	} while (!over);
	free(snake);
	return over;
}

int main(int argc, char* argv[])
{
	setconsole(0);
	get_screen_info();
	snake();
	printf(SET_BKGND_COLOR(0) SET_FORE_COLOR(7) SET_CURSOR_FMT CLEAR_LINE SHOW_CURSOR, height, 1);
	setconsole(1);
	return 0;
}
