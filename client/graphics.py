# Contains functions to draw stuff on the terminal. Currently using ncurses - see graphics_curses.py.
# See line 77. Bad error.

from graphics_curses import *
import time, sys
from base import *

def test():
    scr = curses.newwin(24, 80, 0, 0)
    #scr.idlok(1)
    scr.leaveok(1)
    scr.scrollok(0)
    scr.insch(23, 79, 'b')
    scr.insch(23, 79, 'a')
    #scr.move(0,0)
    sys.stderr.write('Testing')
    #scr.scroll(-1)
    scr.getch()


def init_graphics(stdscr, graphics_config_file):
    # Initialise graphics stuff.
    curses.curs_set(0)
    #test()
    init_colours()
    load_graphics_file(graphics_config_file)
    global debug
    global map_scr
    debug = Debug_screen(stdscr) 
    map_scr = Map_screen(stdscr)


def init_colours():
    # Initialise colour pairs and fill in some global dict.
    for i in range(8):
        for j in range(0,8):
            curses.init_pair((i*9 + j + 2), i, j)
            colour_pair_map[(i,j)] = curses.color_pair((i*9+j+2))

def use_map(map):
    # Switch to displaying the map object @map.
    map_scr.init_map_pad(map.height, map.width)
    for pos in positions(map.height, map.width):
        map_scr.set_map_pad(pos, map.get_tile_sprite(pos), map.get_visibility(pos))

def draw_map(center):
    # Draw map centered at @center.
    map_scr.draw(center)

def update_map(map, update_pos_list):
    # Update from the map object at position in the list.
    for pos in update_pos_list:
        map_scr.set_map_pad(pos, map.get_tile_sprite(pos), map.get_visibility(pos))

def getch():
    # Input a character.
    return map_scr.getch()

def debug_output(*args):
    # Pass on to debug object.
    debug.output(*args)
