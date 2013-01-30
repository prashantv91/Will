# Contains functions to draw stuff on the terminal. Using ncurses.

import curses
from base import *

colour_pair_map = {}        # Maps pairs of colours to colour pair numbers, as initialised by init_colours().
creature_sprites = {}       # Maps from object name to colour pairs. These three filled in from config file by load_graphics_file().
item_sprites = {}
terrain_sprites = {}

colours = {'black':curses.COLOR_BLACK, 'blue':curses.COLOR_BLUE, 'cyan':curses.COLOR_CYAN, 'green':curses.COLOR_GREEN, \
        'magenta':curses.COLOR_MAGENTA, 'red':curses.COLOR_RED, 'white':curses.COLOR_WHITE, 'yellow':curses.COLOR_YELLOW, 'background': -1}

class Sprite():
    # Store display information for an entity.
    def init(self, char = '.', colour_pair = 0):
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

    def __init__(self, stdscr):
        if not self.__instantiated:
            self.map_pad = None
            self.map_screen_yx = stdscr.getmayx()          # This is what will change when more windows come in.
            self.__instantiated = True
        
    def init_map_pad(self, pad):
        # Initialises map_pad to pad.
        self.map_pad = pad

    def set_map_pad(self, pos, sprite):
        # Currently not used. Use Map's set_map_pad() instead.
        self.map_pad.addch(pos.y, pos.x, sprite.char, sprite.colour_pair)

    def refresh(self):
        pass

    def draw(self):
        pass

def init_graphics(graphics_config_file):
    curses.curs_set(0)
    init_colours()
    load_graphics_file(graphics_config_file)


def init_colours():
    # Initialise colour pairs and fill in some global dict.
    for i in range(8):
        for j in range(-1,8):
            curses.init_pair((i*9 + j + 1), i, j)
            colour_pair_map[(i,j)] = curses.colour_pair((i*9+j+1))

def load_graphics_file(filename):
    # Loads graphics config info from the file @filename.
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()

    for line in lines:
        line = line.split('#')[0]
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

