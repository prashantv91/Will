# Contains declarations for storing map info.

from graphics import *

class Object():
    # Stores the type of object, it's sprite and an annotation.
    def __init__(self, _type = None, _sprite = Sprite(), _annotation = ''):
        self.type = _type
        self.sprite = _sprite
        self.annotation = _annotation

class Terrain():
    # To store info about a particular terrain. Might be more useful later.
    def __init__(self, _sprite = Sprite()):
        self.sprite = _sprite

class Tile():
    # Info for a map tile.
    def __init__(self, _terrain = None, _creature = None, _items = []):
        self.terrain = _terrain
        self.creature = _creature
        self.items = _items

class Map():
    # A rectangular collection of tiles.
    def __init__(self, _height, _width, _screen):
        self.height = _height
        self.width = _width
        self.map = [[Tile() for i in range(self.width)] for j in range(self.height)]
        self.screen = _screen

    def update_tile(self, pos, terrain = -1, creature = -1, items = -1):
        # Updates tile at (pos.y, pos.x). Couldn't use None as invalid indicator.
        if terrain != -1:
            self.map[pos.y][pos.x].terrain = terrain
        if items != -1:
            self.map[pos.y][pos.x].items = items
        if creature != -1:
            self.map[pos.y][pos.x].creature = creature
        
        if self.map[pos.y][pos.x].items == [] and self.map[pos.y][pos.x].creature == None:
            self.screen.set_map_pad(pos, self.map[pos.y][pos.x].terrain.sprite)
        elif self.map[pos.y][pos.x].items != [] and self.map[pos.y][pos.x].creature == None:
            self.screen.set_map_pad(pos, self.map[pos.y][pos.x].items[0].sprite)
        else:
            self.screen.set_map_pad(pos, self.map[pos.y][pos.x].creature.sprite)

    def set_tile(self, pos, terrain, creature = None, items = []):
        # Sets tile at (pos.y, pos.x). 
        # Note that this doesn't call the screen's update function. This is to save on locking time.
        self.map[pos.y][pos.x] = Tile(terrain, creature, items)

    def update_screen(self):
        # Synchronises screen.map_pad with this map. To be called immediately after many calls to set_tile.
        self.screen.init_map_pad(self.height, self.width)
        for y in range(self.height):
            for x in range(self.width):
                if self.map[pos.y][pos.x].items == [] and self.map[pos.y][pos.x].creature == None:
                    self.screen.set_map_pad(pos, self.map[pos.y][pos.x].terrain.sprite)
                elif self.map[pos.y][pos.x].items != [] and self.map[pos.y][pos.x].creature == None:
                    self.screen.set_map_pad(pos, self.map[pos.y][pos.x].items[0].sprite)
                else:
                    self.screen.set_map_pad(pos, self.map[pos.y][pos.x].creature.sprite)

        

    

