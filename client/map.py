# Contains declarations for storing map info.

import graphics, curses
from base import *
import random, time

class Monster():
    # Stores the type of monster, it's sprite and an annotation.
    def __init__(self, name = '', annotation = ''):
        self.name = name
        self.sprite = graphics.monster_sprites[name]
        self.annotation = annotation

class Item():
    # Stores the type of item, it's sprite and an annotation.
    def __init__(self, name = '', annotation = ''):
        self.name = name
        self.sprite = graphics.item_sprites[name]
        self.annotation = annotation

class Terrain():
    # To store info about a particular terrain. Might be more useful later.
    def __init__(self, name):
        self.name = name
        self.sprite = graphics.terrain_sprites[name]

class Tile():
    # Info for a map tile.
    def __init__(self, terrain = None, monster = None, items = [], visible = False):
        self.terrain = terrain
        self.monster = monster
        self.items = items
        self.visible = visible

class Map():
    # A rectangular collection of tiles.
    def __init__(self, name, height, width, player_pos = Position(0, 0), player_monster = None):
        self.name = name
        self.height = height
        self.width = width
        self.map = [[Tile() for i in range(self.width)] for j in range(self.height)]
        self.player_pos = player_pos                  # Decide on how to maintain this.
        self.player_monster = player_monster        # And this. Currently there for testing.  

    def update_tile(self, pos, terrain = -1, monster = -1, items = -1, visible = False):
        # Updates tile at (pos.y, pos.x). Couldn't use None as invalid indicator.
        if terrain != -1:
            self.map[pos.y][pos.x].terrain = terrain
        if items != -1:
            self.map[pos.y][pos.x].items = items
        if monster != -1:
            self.map[pos.y][pos.x].monster = monster
        self.map[pos.y][pos.x].visible = visible
    
    def get_visibility(self, pos):
        # Returns True if tile at @pos is visible.
        return self.map[pos.y][pos.x].visible

    def get_tile_sprite(self, pos):
        # Returns sprite for the tile at @pos.
        if self.map[pos.y][pos.x].items == [] and self.map[pos.y][pos.x].monster == None:
            return self.map[pos.y][pos.x].terrain.sprite
        elif self.map[pos.y][pos.x].items != [] and self.map[pos.y][pos.x].monster == None:
            return self.map[pos.y][pos.x].items[0].sprite
        else:
            return self.map[pos.y][pos.x].monster.sprite

    def set_tile(self, pos, terrain, monster = None, items = []):
        # Sets tile at (pos.y, pos.x) and updates this instance's map_pad.
        self.map[pos.y][pos.x] = Tile(terrain, monster, items)

    def set_player(self, player_pos, player_monster):
        # Temporary, for testing.
        self.player_pos.y = player_pos.y
        self.player_pos.x = player_pos.x
        self.player_monster = player_monster
        self.update_tile(self.player_pos, monster = self.player_monster)
    
    def update_player_pos(self, player_pos):
        # Temporary, for testing.
        self.update_tile(self.player_pos, monster = None)
        self.player_pos.y = player_pos.y
        self.player_pos.x = player_pos.x
        self.update_tile(self.player_pos, monster = self.player_monster)
    

def map_test(stdscr):
    graphics.init_graphics(stdscr, 'graphics_config')
    map_height = 50
    map_width = 120
    player_pos = Position(map_height/2, map_width/2)
    M = Map(name = 'map0', height = map_height, width = map_width, player_pos = Position(5, 5))
    
    for pos in positions(map_height, map_width):
        if random.randint(0,1) == 0:
            M.set_tile(pos, Terrain('grass'))
        else:
            M.set_tile(pos, Terrain('wall'))  

    M.set_player(player_pos, Monster('dog'))
    
    graphics.use_map(M)
    old_pos = Position(0, 0)

    graphics.draw_map(player_pos)
    while True:
        key = graphics.getch()
        old_pos.y = player_pos.y
        old_pos.x = player_pos.x
        if key == ord('d') and player_pos.x < map_width-1:
            player_pos.e()
        elif key == ord('a') and player_pos.x > 0:
            player_pos.w()
        elif key == ord('w') and player_pos.y > 0:
            player_pos.n()
        elif key == ord('s') and player_pos.y < map_height-1:
            player_pos.s()
        else:
            if chr(key) not in ['w', 'a', 's', 'd']:
                break
          
        M.update_player_pos(player_pos)
        graphics.update_map(M, [old_pos, player_pos])
        graphics.draw_map(player_pos)
        #graphics.debug_output(M.player_pos.y, M.player_pos.x, old_pos.y, old_pos.x)
        
    time.sleep(1)


curses.wrapper(map_test)

