# Contains fundamental definitions added in the spur of many moments.

class Config():
    # Dummy class to store global constants from config file.
    pass

class Position():
    # To store position of something.
    def __init__(self, y, x):     # (y,x) in curses convention.
        self.y = y
        self.x = x

    def n(self):                      # Movement functions. Abbreviated.
        self.y -= 1
    def e(self):
        self.x += 1
    def s(self):
        self.y += 1
    def w(self):
        self.x -= 1

    def move(self, dir):
        # For convenience while testing.
        {0:self.n, 1:self.e, 2:self.s, 3:self.w}[dir%4]()



def is_int(string):
    # Does @string represent an integer?
    try:
        int(string)
        return True
    except ValueError:
        return False

config = Config()

def load_config_file(filename):
    # Loads constants from the file @filename as attributes of the config object.
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()

    for line in lines:
        line = line.split('#')[0]
        line = line.split('=')

        if len(line) != 2:
            continue

        key = line[0].strip()
        val = line[1].strip()

        if is_int(val):
            val = int(val)

        try:
            setattr(config, key, val)
        except:
            continue

def int2str(num, length, base = 256):
    # Converts @num to a string corresponding to ascii values in little(big?) endian format of length @length.
    ret = ''
    while len(ret) < length:
        ret += chr(num%base)
        num /= base
    return ret

def str2int(string, base = 256):
    # Inverts int2str.
    ret = 0
    for i in range(len(string)):
        ret += ord(string[i]) * (base ** i)
    return ret

def positions(height, width):
    # Generator to iterate through positions from (0, 0) to (height-1, width-1).
    for y in range(0, height):
        for x in range(0, width):
            yield Position(y, x)


# The following is to implement singleton pattern.
instances = {}
def get_instance(Class, *args):
    # Store one instance of each class in a dict.
    if Class not in instances.keys():
        instances[Class] = Class(*args)
    return instances[Class]

def singleton(Class):
    # Decorator.
    def on_call(*args):
        return get_instance(Class, *args)
    return on_call

