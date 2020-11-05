from game import *
import game
import sys
import time
import random

def snake():
	snakelen = 1
	dir = 0
	snake = [[width // 2, height // 2]]
	print(SET_BKGND_COLOR(0) + CLEAR_SCREEN, end='')
	over = 0
	lastfood = 0
	foodloc = 0
	random.seed(int(time.time()))
	for i in range(1, height + 1, 1):
		print((SET_CURSOR_FMT + SET_BKGND_COLOR(7) + " ") % (i, 1), end='')
		print((SET_CURSOR_FMT + SET_BKGND_COLOR(7) + " ") % (i, width), end='')
	for i in range(1 + 1, width, 1):
		print((SET_CURSOR_FMT + SET_BKGND_COLOR(7) + " ") % (1, i), end='')
		print((SET_CURSOR_FMT + SET_BKGND_COLOR(7) + " ") % (height, i), end='')
	print((SET_BKGND_COLOR(2) + SET_CURSOR_FMT + " " + SET_CURSOR_FMT) % (snake[0][1], snake[0][0], snake[0][1], snake[0][0]), end='')
	while (not over):
		ch = read()
		if (CASE_ARROWKEYS(ch)):
			ch = chr(ord(getch()))
			if ch == UPARROW  and (dir != 2 or snakelen == 1): dir = 1
			if ch == DOWNARROW and (dir != 1 or snakelen == 1): dir = 2
			if ch == RIGHTARROW and (dir != 4 or snakelen == 1): dir = 3
			if ch == LEFTARROW and (dir != 3 or snakelen == 1): dir = 4
		sys.stdout.flush()
		if (ch == -1 or ch == "" or ch == '\xff'): time.sleep(100 / 1000)
		if (dir == 0): continue
		lastfood += 1
		if (lastfood == 50 and foodloc == 0):
			while (True):
				foodloc = random.randint(0, width * height - 1) % (width * height)
				if (foodloc % width == 0 or foodloc % width == width - 1 or foodloc // width == 0 or foodloc // width == height - 1): continue
				for i in range(0, snakelen, 1):
					if (snake[i][0] == foodloc % width +1 and snake[i][1] == foodloc // width +1): break
				else: break
			print((HIDE_CURSOR + SET_CURSOR_FMT + " ") % (foodloc // width +1, foodloc % width +1), end='')
		for i in range(snakelen, 0, -1):
			if i == len(snake): snake.append(snake[i - 1][:])
			else: snake[i] = snake[i - 1][:]
		if (dir == 1): snake[0][1] -= 1
		elif (dir == 2): snake[0][1] += 1
		elif (dir == 3): snake[0][0] += 1
		elif (dir == 4): snake[0][0] -= 1
		if (snake[0][0] == foodloc % width +1 and snake[0][1] == foodloc // width + 1):
			snakelen += 1
			lastfood = 0
			foodloc = 0
		else: print((HIDE_CURSOR + SET_CURSOR_FMT + SET_BKGND_COLOR(0) + " ") % (snake[snakelen][1], snake[snakelen][0]), end='')
		print((SET_CURSOR_FMT + SET_BKGND_COLOR(2) + " " + SET_CURSOR_FMT + SHOW_CURSOR) % (snake[0][1], snake[0][0], snake[0][1], snake[0][0]), end='')
		if (snake[0][1] == 1 or snake[0][1] == height or snake[0][0] == 1 or snake[0][0] == width): over = 2
	return over

setconsole(False)
get_screen_info()
width = game.width
height = game.height
snake()
print((SET_BKGND_COLOR(0) + SET_CURSOR_FMT + CLEAR_LINE + SHOW_CURSOR) % (height, 0), end='')
setconsole(True)
