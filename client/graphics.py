# Contains functions to draw stuff on the terminal. Using ncurses.
# See line 77. Bad error.

import curses
import time, sys
from base import *

colour_pair_map = {}        # Maps pairs of colours to colour pair numbers, as initialised by init_colours().
creature_sprites = {}       # Maps from object name to colour pairs. These three filled in from config file by load_graphics_file().
item_sprites = {}
terrain_sprites = {}
map_pads = {}               # Stores map pads against map names.

colours = {'black':curses.COLOR_BLACK, 'blue':curses.COLOR_BLUE, 'cyan':curses.COLOR_CYAN, 'green':curses.COLOR_GREEN, \
        'magenta':curses.COLOR_MAGENTA, 'red':curses.COLOR_RED, 'white':curses.COLOR_WHITE, 'yellow':curses.COLOR_YELLOW, 'background': curses.COLOR_BLACK}

debug_scr_height = 2
debug_scr_width = 80
debug_scr = None

class Sprite():
    # Store display information for an entity.
    def __init__(self, char = '.', colour_pair = 0):
        self.char = ord(char)      # Note that the @char that is given is to be a character.
        self.colour_pair = colour_pair

class Map_screen(object):
    # To draw the map. Only the map - other windows are other classes. 
    __instance = None
    __instantiated = False
    
    def __new__(cls, *args, **kwargs):
        # To make sure only one instance of this class exists.
        if not cls.__instance:
            cls.__instance = super(Map_screen, cls).__new__(cls, *args, **kwargs)
        return cls.__instance

    def __init__(self, screen):
        if not self.__instantiated:
            self.map_pad = None
            self.map_screen_start = Position(screen.getbegyx()[0] + debug_scr_height, screen.getbegyx()[1])
            self.map_screen_limits = Position((screen.getmaxyx()[0] - screen.getbegyx()[0] - debug_scr_height), \
                                              (screen.getmaxyx()[1] - screen.getbegyx()[1]))          # (height, width). This is what will change when more windows come in.
            self.__instantiated = True
        
    def init_map_pad(self, name, height, width):
        # Retrieve map pad from dict if it exists, else create new one.
        #self.map_pad = pad
        if name in map_pads.keys():
            self.map_pad = map_pads[name]
        else:
            self.map_pad = curses.newpad(height, width)
            map_pads[name] = self.map_pad

    def addch(self, window, y, x, char, attr):
        # Because curses' addch cannot write to bottom-right cell.
        if (y, x) == (window.getmaxyx()[0]-1, window.getmaxyx()[1]-1):
            window.insch(y, x, char, attr)
        else:
            window.addch(y, x, char, attr)
    
    def set_map_pad(self, pos, sprite, bold = False):
        if bold:
            self.addch(self.map_pad, pos.y, pos.x, sprite.char, sprite.colour_pair | curses.A_BOLD)
        else:
            #curses_debug('%d %d %c %d %d %d' % (pos.y, pos.x, sprite.char, self.map_pad.getmaxyx()[0], self.map_pad.getmaxyx()[1], sprite.colour_pair))
            self.addch(self.map_pad, pos.y, pos.x, sprite.char, sprite.colour_pair)
        
    def draw(self, center):
        # Draw from the current pad to screen, centered around @center on the pad. 
        # Check the values.
        start_y = max(0, min(center.y - self.map_screen_limits.y/2, self.map_pad.getmaxyx()[0] - self.map_screen_limits.y))     # Starting position on the pad.
        start_x = max(0, min(center.x - self.map_screen_limits.x/2, self.map_pad.getmaxyx()[1] - self.map_screen_limits.x))     
        screen_start_y = self.map_screen_start.y + max(0, (self.map_screen_limits.y/2 - self.map_pad.getmaxyx()[0]/2))      # Starting position on the window.
        screen_start_x = self.map_screen_start.x + max(0, (self.map_screen_limits.x/2 - self.map_pad.getmaxyx()[1]/2))
        screen_end_y = screen_start_y + min(self.map_screen_limits.y-1, self.map_pad.getmaxyx()[0])
        screen_end_x = screen_start_x + min(self.map_screen_limits.x-1, self.map_pad.getmaxyx()[1])
        #curses_debug('%3d %3d %3d %3d %3d %3d %3d %3d' % (center.y, center.x, start_y, start_x, screen_start_y, screen_start_x, screen_end_y, screen_end_x))   
        debug_scr.refresh() # Weird error if this is not there. Check out why.
        self.map_pad.refresh(start_y, start_x, screen_start_y, screen_start_x, screen_end_y, screen_end_x)

def curses_debug(str):
    debug_scr.addstr(0, 0, str)
    debug_scr.refresh()

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


def init_graphics(graphics_config_file):
    curses.curs_set(0)
    global debug_scr 
    debug_scr = curses.newwin(debug_scr_height, debug_scr_width, 0, 0)
    #test()
    init_colours()
    load_graphics_file(graphics_config_file)


def init_colours():
    # Initialise colour pairs and fill in some global dict.
    for i in range(8):
        for j in range(0,8):
            curses.init_pair((i*9 + j + 2), i, j)
            colour_pair_map[(i,j)] = curses.color_pair((i*9+j+2))


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


