from game import *
import game
import sys
import time
import random

oldseed = random.seed
def newseed(a):
	oldseed(randseed)
random.seed = newseed

#python3 snakeextend.py 101
#echo -e "\E[24;80R..." | python3 snakeextend.py 101 | emuterm 24 80
inread = False
lastval = -1
oldgetch = getch
def newgetch():
	global lastval, inread
	val = oldgetch()
	if (not inread and val != -1 and val != "" and val != '\xff'):
		fp = open("log.in", "ab")
		fp.write(val.encode('ascii') if isinstance(val, str) else val)
		fp.flush()
		fp.close()
		if os.name == "nt" and (lastval == b'\xe0'): lastval += val.encode('ascii') if isinstance(val, str) else val
		elif os.name != "nt" and (lastval == b'\x1b' or (lastval == b'\x1b[')): lastval += val.encode('ascii') if isinstance(val, str) else val
		else: lastval = val.encode('ascii') if isinstance(val, str) else val
	return val
getch = newgetch
game.getch = newgetch

oldread = read
def newread():
	global lastval, inread
	inread = True
	val = oldread()
	inread = False
	if (val != -1 and val != "" and val != '\xff'):
		fp = open("log.in", "ab")
		fp.write(val.encode('ascii') if isinstance(val, str) else val)
		fp.flush()
		fp.close()
		if os.name == "nt" and (lastval == b'\xe0'): lastval += val.encode('ascii') if isinstance(val, str) else val
		elif os.name != "nt" and (lastval == b'\x1b' or (lastval == b'\x1b[')): lastval += val.encode('ascii') if isinstance(val, str) else val
		else: lastval = val.encode('ascii') if isinstance(val, str) else val
	return val
read = newread
game.read = newread

oldtime = time.sleep
def newtime(a):
	global lastval
	if (lastval != -1):
		fp = open("log.in", "ab")
		fp.write(lastval)
		fp.flush()
		fp.close()
	oldtime(a)
time.sleep = newtime

def snake():
	global lastval
	lives = 3
	random.seed(int(time.time()))
	while lives != 0:
		lastval = -1
		snakelen = 1
		dir = 0
		snake = [[width // 2, height // 2]]
		print(SET_BKGND_COLOR(0) + CLEAR_SCREEN, end='')
		over = 0
		lastfood = 0
		foodloc, foodloc2, foodloc3 = 0, 0, 0
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
				lastfood = 0
				if (foodloc2 == 0):
					foodloc2 = foodloc
					foodloc = 0
				if (foodloc3 == 0):
					foodloc3 = foodloc2
					foodloc2 = foodloc
					foodloc = 0
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
			elif (snake[0][0] == foodloc2 % width +1 and snake[0][1] == foodloc2 // width + 1):
				snakelen += 1
				lastfood = 0
				foodloc2 = foodloc
				foodloc = 0
			elif (snake[0][0] == foodloc3 % width +1 and snake[0][1] == foodloc3 // width + 1):
				snakelen += 1
				lastfood = 0
				foodloc3 = foodloc2
				foodloc2 = foodloc
				foodloc = 0
			else: print((HIDE_CURSOR + SET_CURSOR_FMT + SET_BKGND_COLOR(0) + " ") % (snake[snakelen][1], snake[snakelen][0]), end='')
			print((SET_CURSOR_FMT + SET_BKGND_COLOR(2) + " " + SET_CURSOR_FMT + SHOW_CURSOR) % (snake[0][1], snake[0][0], snake[0][1], snake[0][0]), end='')
			if (snake[0][1] == 1 or snake[0][1] == height or snake[0][0] == 1 or snake[0][0] == width): over = 2
		lives -= 1
	return over

randseed = int(sys.argv[1])
setconsole(False)
get_screen_info()
width = game.width
height = game.height
snake()
print((SET_BKGND_COLOR(0) + SET_CURSOR_FMT + CLEAR_LINE + SHOW_CURSOR) % (height, 0), end='')
setconsole(True)
