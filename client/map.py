# Contains declarations for storing map info.

from graphics import *
import random

class Creature():
    # Stores the type of craeture, it's sprite and an annotation.
    def __init__(self, name = '', annotation = ''):
        self.name = name
        self.sprite = creature_sprites[name]
        self.annotation = annotation

class Item():
    # Stores the type of item, it's sprite and an annotation.
    def __init__(self, name = '', annotation = ''):
        self.name = name
        self.sprite = item_sprites[name]
        self.annotation = annotation

class Terrain():
    # To store info about a particular terrain. Might be more useful later.
    def __init__(self, name):
        self.name = name
        self.sprite = terrain_sprites[name]

class Tile():
    # Info for a map tile.
    def __init__(self, terrain = None, creature = None, items = [], visible = False):
        self.terrain = terrain
        self.creature = creature
        self.items = items
        self.visible = visible

class Map():
    # A rectangular collection of tiles.
    def __init__(self, name, height, width, screen, player_pos = Position(0, 0), player_creature = None):
        self.name = name
        self.height = height
        self.width = width
        self.map = [[Tile() for i in range(self.width)] for j in range(self.height)]
        self.screen = Map_screen(screen)
        self.player_pos = player_pos                  # Decide on how to maintain this.
        self.player_creature = player_creature        # And this. Currently there for testing.  
        #self.map_pad = curses.newpad(self.height, self.width)           # BAD

    def set_map_pad(self, pos, sprite, visible = False):
        self.screen.set_map_pad(pos, sprite, visible)

    def update_tile(self, pos, terrain = -1, creature = -1, items = -1, visible = False):
        # Updates tile at (pos.y, pos.x). Couldn't use None as invalid indicator.
        if terrain != -1:
            self.map[pos.y][pos.x].terrain = terrain
        if items != -1:
            self.map[pos.y][pos.x].items = items
        if creature != -1:
            self.map[pos.y][pos.x].creature = creature
        self.map[pos.y][pos.x].visible = visible
        
        if self.map[pos.y][pos.x].items == [] and self.map[pos.y][pos.x].creature == None:
            self.set_map_pad(pos, self.map[pos.y][pos.x].terrain.sprite, visible)
        elif self.map[pos.y][pos.x].items != [] and self.map[pos.y][pos.x].creature == None:
            self.set_map_pad(pos, self.map[pos.y][pos.x].items[0].sprite)
        else:
            self.set_map_pad(pos, self.map[pos.y][pos.x].creature.sprite)

    def set_tile(self, pos, terrain, creature = None, items = []):
        # Sets tile at (pos.y, pos.x) and updates this instance's map_pad.
        self.map[pos.y][pos.x] = Tile(terrain, creature, items)
        if self.map[pos.y][pos.x].items == [] and self.map[pos.y][pos.x].creature == None:
            self.set_map_pad(pos, self.map[pos.y][pos.x].terrain.sprite)
        elif self.map[pos.y][pos.x].items != [] and self.map[pos.y][pos.x].creature == None:
            self.set_map_pad(pos, self.map[pos.y][pos.x].items[0].sprite)
        else:
            self.set_map_pad(pos, self.map[pos.y][pos.x].creature.sprite)

    def set_player(self, player_pos, player_creature):
        # Temporary, for testing.
        self.player_pos.y = player_pos.y
        self.player_pos.x = player_pos.x
        self.player_creature = player_creature
        self.update_tile(self.player_pos, creature = self.player_creature)
    
    def update_player_pos(self, player_pos):
        # Temporary, for testing.
        self.update_tile(self.player_pos, creature = None)
        self.player_pos.y = player_pos.y
        self.player_pos.x = player_pos.x
        self.update_tile(self.player_pos, creature = self.player_creature)
    
    def sync_screen(self):
        # Synchronises screen.map_pad with this map.
        self.screen.init_map_pad(self.name, self.height, self.width)
        
    def draw(self):
        # Get the map updated on the screen.
        self.screen.draw(self.player_pos)
    

def map_test(stdscr):
    init_graphics('graphics_config')
    pos = Position(25, 60)
    M = Map(name = 'map0', height = 50, width = 120, screen = stdscr, player_pos = Position(5, 5))
    M.sync_screen()
    for i in range(50):
        for j in range(120):
            if random.randint(0,1) == 0:
                M.set_tile(Position(i,j), Terrain('grass'))
            else:
                M.set_tile(Position(i,j), Terrain('wall'))  

    M.set_player(pos, Creature('dog'))
    M.draw()
    
    while True:
        key = M.screen.map_pad.getch()
        if key == ord('d'):
            pos.e()
        elif key == ord('a'):
            pos.w()
        elif key == ord('w'):
            pos.n()
        elif key == ord('s'):
            pos.s()
        else:
            break
          
        M.update_player_pos(pos)
        M.draw()
        #curses_debug('%d %d' % (M.player_pos.y, M.player_pos.x))
        
    time.sleep(1)

#print curses.color_pair(0)

curses.wrapper(map_test)
#map_test(None)

