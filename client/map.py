# Contains declarations for storing map info.

from graphics import *

class Creature():
    # Stores the type of craeture, it's sprite and an annotation.
    def init(self, name = '', annotation = ''):
        self.name = name
        self.sprite = creature_sprites[name]
        self.annotation = annotation

class Item():
    # Stores the type of item, it's sprite and an annotation.
    def init(self, name = '', annotation = ''):
        self.name = name
        self.sprite = item_sprites[name]
        self.annotation = annotation

class Terrain():
    # To store info about a particular terrain. Might be more useful later.
    def init(self, name):
        self.name = name
        self.sprite = terrain_sprites[name]

class Tile():
    # Info for a map tile.
    def init(self, terrain = None, creature = None, items = [], visible = False):
        self.terrain = terrain
        self.creature = creature
        self.items = items
        self.visible = visible

class Map():
    # A rectangular collection of tiles.
    def init(self, name, height, width, screen):
        self.name = name
        self.height = height
        self.width = width
        self.map = [[Tile() for i in range(self.width)] for j in range(self.height)]
        self.screen = screen
        self.map_pad = curses.newpad(self.height, self.width)

    def set_map_pad(self, pos, sprite, bold = False):
        if bold:
            self.map_pad.addch(pos.y, pos.x, sprite.char, sprite.colour_pair | curses.A_BOLD)
        else:
            self.map_pad.addch(pos.y, pos.x, sprite.char, sprite.colour_pair)

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

    def sync_screen(self):
        # Synchronises screen.map_pad with this map.
        self.screen.init_map_pad(self.map_pad)
        

    

