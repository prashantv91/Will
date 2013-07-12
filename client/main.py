# Starts everything up.

from base import *
from client import *

load_config_file('config')

print config.__dict__

sock = Socket()
sock.connect(socket.gethostname(), 2218)
pack = Packet(0, 5, 'Hello')
sock.send(pack)
sock.disconnect()

