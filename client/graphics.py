# Contains functions to draw stuff on the terminal. Currently using ncurses.

import curses
import time, sys
from base import *

#map_pads = {}               # Stores map pads against map names. Optimisation for later.

colour_pair_map = {}        # Maps pairs of colours to colour pair numbers, as initialised by init_colours().
creature_sprites = {}       # Maps from object name to colour pairs. These three filled in from config file by load_graphics_file().
item_sprites = {}
terrain_sprites = {}

colours = {'black':curses.COLOR_BLACK, 'blue':curses.COLOR_BLUE, 'cyan':curses.COLOR_CYAN, 'green':curses.COLOR_GREEN, \
        'magenta':curses.COLOR_MAGENTA, 'red':curses.COLOR_RED, 'white':curses.COLOR_WHITE, 'yellow':curses.COLOR_YELLOW, 'background': curses.COLOR_BLACK}

debug_scr_height = 2
debug = None
map_scr = None

class Sprite():
    # Store display information for an entity.
    def __init__(self, char = '.', colour_pair = 0):
        self.char = ord(char)      # Note that the @char that is given is to be a character.
        self.colour_pair = colour_pair

@singleton
class Debug_screen(object):
    # To print debug messages.
    def __init__(self, start, limits):
        self.start = start 
        self.limits = limits                                                         # (height, width). This is what will change when more windows come in.

        self.screen = curses.newwin(self.limits.y, self.limits.x, self.start.y, self.start.x)
        
    def output(self, *args):
        string = ''
        for arg in args:
            string += str(arg)
            string += '\t'
        string = string[:-1]

        self.screen.erase()
        self.screen.addstr(0, 0, string)
        self.screen.refresh()
        
@singleton
class Map_screen(object):
    # To draw the map. Only the map - other windows are other classes. 
    def __init__(self, start, limits):
        self.map_pad = None
        self.map_height = 0
        self.map_width = 0
        self.start = start 
        self.limits = limits                                                         # (height, width). This is what will change when more windows come in.

    def init_map_pad(self, height, width):
        # Set map pad to that of Map object @map.
        self.map_pad = curses.newpad(height, width)
        self.map_height = height
        self.map_width = width

    def addch(self, window, pos, char, attr):
        # Because curses' addch cannot write to bottom-right cell.
        if (pos.y, pos.x) == (window.getmaxyx()[0]-1, window.getmaxyx()[1]-1):
            window.insch(pos.y, pos.x, char, attr)
        else:
            window.addch(pos.y, pos.x, char, attr)
    
    def set_map_pad(self, pos, sprite, bold = False):
        if bold:
            self.addch(self.map_pad, pos, sprite.char, sprite.colour_pair | curses.A_BOLD)
        else:
            #debug.print('%d %d %c %d %d %d' % (pos.y, pos.x, sprite.char, self.map_pad.getmaxyx()[0], self.map_pad.getmaxyx()[1], sprite.colour_pair))
            self.addch(self.map_pad, pos, sprite.char, sprite.colour_pair)
        
    def draw(self, center):
        # Draw from the current pad to screen, centered around @center on the pad. 
        # Check the values.
        # Center of screen is at start + limits/2.
        
        start_y = max(0, min(center.y - self.limits.y/2, self.map_height - self.limits.y))     # Starting position on the pad.
        start_x = max(0, min(center.x - self.limits.x/2, self.map_width - self.limits.x))     
        screen_start_y = self.start.y + max(0, (self.limits.y/2 - self.map_height/2))      # Starting position on the window.
        screen_start_x = self.start.x + max(0, (self.limits.x/2 - self.map_width/2))
        screen_end_y = screen_start_y + min(self.limits.y-1, self.map_height)
        screen_end_x = screen_start_x + min(self.limits.x-1, self.map_width)
        debug_output(center.y, center.x, start_y, start_x, screen_start_y, screen_start_x, screen_end_y, screen_end_x)
        self.map_pad.refresh(start_y, start_x, screen_start_y, screen_start_x, screen_end_y, screen_end_x)

    def getch(self):
        # Temporary, till we get an input class.
        return self.map_pad.getch()

def load_graphics_file(filename):
    # Loads graphics config info from the file @filename.
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    
    for line in lines:
        #line = line.split('#')[0]
        if line.strip() != '' and line.strip()[0] == '#':
            continue

        line = [part.strip() for part in line.split(':')]

        if len(line) not in [4,5]:
            continue
        
        if len(line) == 4:
            line.append('background')

        if line[3] not in colours or line[4] not in colours:
            continue

        name = line[1]
        char = line[2][0]
        colour_pair = colour_pair_map[(colours[line[3]], colours[line[4]])]
        sprite = Sprite(char, colour_pair)
        
        if line[0] == 'terrain':
            terrain_sprites[name] = sprite
        elif line[0] == 'creature':
            creature_sprites[name] = sprite
        elif line[0] == 'item':
            item_sprites[name] = sprite

def init_graphics(stdscr, graphics_config_file):
    # Initialise graphics stuff.
    curses.curs_set(0)
    init_colours()
    load_graphics_file(graphics_config_file)

    global debug
    global map_scr

    std_begy, std_begx = stdscr.getbegyx()
    std_maxy, std_maxx = stdscr.getmaxyx()

    debug = Debug_screen(Position(std_begy, std_begx), Position(debug_scr_height, std_maxx-1)) 
    map_scr = Map_screen(Position(std_begy + debug_scr_height, std_begx), Position(std_maxy - debug_scr_height, std_maxx-1)) 


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
