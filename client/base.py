# Contains fundamental definitions added in the spur of many moments.

class Config():
    # Dummy class to store global constants from config file.
    pass

class Position():
    # To store position of something.
    def __init__(self, _y, _x):     # (y,x) in curses convention.
        self.y = _y
        self.x = _x



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


