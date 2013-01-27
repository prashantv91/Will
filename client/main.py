# Starts everything up.

from base import *
from client import *

load_config_file('config')

print config.__dict__

sock = connect_to_server(socket.gethostname(), 2218)
pack = Packet(4, 5, 'Hello')
send(sock, pack)
sock.close()

