# Contains functions to draw stuff on the terminal. Using ncurses.

import curses
from base import *

colour_pair_map = {}        # Maps pairs of colours to colour pair numbers, as initialised by init_colours().

class Sprite():
    # Store display information for an entity.
    def __init__(self, _char = '@', _colours = (7, 0)):
        if colour_pair_map == {}:
            init_colours()

        self.char = ord(_char)      # Note that the @_char that is given is to be a character.
        self.colour_pair = _colours

class Screen(object):
    # Has various windows that display stuff. 
    __instance = None
    __instantiated = False
    
    def __new__(cls, *args, **kwargs):
        # To make sure only one instance of this class exists.
        if not cls.__instance:
            cls.__instance = super(Screen, cls).__new__(cls, *args, **kwargs)
        return cls.__instance

    def __init__(self, stdscr):
        if not self.__instantiated:
            self.map_pad = None
            self.main_screen = curses.newwin(stdscr.getmayx()[0], stdscr.getmayx()[1], 0, 0)
            self.__instantiated = True

            curses.curs_set(0)
            if colour_pair_map == {}:
                init_colours()
        
    def init_map_pad(self, height, width):
        # Initialises map_pad.
        self.map_pad = curses.newpad(height, width)

    def set_map_pad(self, pos, sprite):
        self.map_pad.addch(pos.y, pos.x, sprite.char, sprite.colour_pair)

    def refresh(self):
        pass


def init_colours():
    # Initialise colour pairs and fill in some global dict.
    pass
