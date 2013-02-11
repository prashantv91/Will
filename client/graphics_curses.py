# curses classes for drawing stuff.

import curses
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
    def __init__(self, screen = None):
        if screen != None:
            self.start = Position(screen.getbegyx()[0], screen.getbegyx()[1])
            self.limits = Position(debug_scr_height, (screen.getmaxyx()[1] - screen.getbegyx()[1]))          # (height, width). This is what will change when more windows come in.
        else:
            self.start = Position(0, 0)
            self.limits = Position(0, 0)

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
    def __init__(self, screen = None):
        self.map_pad = None
        self.map_height = 0
        self.map_width = 0

        if screen != None:
            self.start = Position(screen.getbegyx()[0] + debug_scr_height, screen.getbegyx()[1])
            self.limits = Position((screen.getmaxyx()[0] - screen.getbegyx()[0] - debug_scr_height), \
                                              (screen.getmaxyx()[1] - screen.getbegyx()[1]))          # (height, width). This is what will change when more windows come in.
        else:
            self.start = Position(0, 0)
            self.limits = Position(0, 0)

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
        start_y = max(0, min(center.y - self.limits.y/2, self.map_height - self.limits.y))     # Starting position on the pad.
        start_x = max(0, min(center.x - self.limits.x/2, self.map_width - self.limits.x))     
        screen_start_y = self.start.y + max(0, (self.limits.y/2 - self.map_height/2))      # Starting position on the window.
        screen_start_x = self.start.x + max(0, (self.limits.x/2 - self.map_width/2))
        screen_end_y = screen_start_y + min(self.limits.y-1, self.map_height)
        screen_end_x = screen_start_x + min(self.limits.x-1, self.map_width)
        #debug.print('%3d %3d %3d %3d %3d %3d %3d %3d' % (center.y, center.x, start_y, start_x, screen_start_y, screen_start_x, screen_end_y, screen_end_x))   
        #debug_scr.refresh() # Weird error if this is not there. Check out why.
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


