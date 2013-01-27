# Contains functions that handle networking for the client.

import socket
from base import *

class Packet:
    ''' Structure to store network packets. '''
    def __init__(self, _type = None, _length = 0, _data = ''):
        self.type = _type
        self.length = _length
        self.data = _data

def receive_full(socket, length):
    ''' To ensure everything is received. '''
    msg = ''
    while len(msg) < length:
        temp = socket.recv(length - len(msg))
        if temp == '':
            raise RuntimeError("Socket connection broken!")
        msg = msg + temp
    return msg
    
def send_full(socket, msg):
    # To ensure everything is sent.
    sent = 0
    while sent < len(msg):
        sent_now = socket.send(msg[sent:])
        if sent_now == 0:
            raise RuntimeError("Socket connection broken!")
        sent += sent_now
    return sent

def connect_to_server(server, server_port):
    # Connects to specified port on the specified server and returns the corresponding socket.
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((server, server_port))
    return sock

def disconnect(socket):
    # For completeness.
    socket.close()

def send(socket, packet):
    # Sends a packet as specified by the Packet object @packet.
    if packet.type == None:
        return

    socket.send(int2str(packet.type, 1))
    socket.send(int2str(packet.length, 4))
    send_full(socket, packet.data)

def receive(socket):
    # Receives a packet and returns appropriate Packet object.
    packet = Packet()

    packet.type = str2int(socket.recv(1))
    packet.length = str2int(socket.recv(4))
    packet.data = receive_full(socket, packet.length)

    return packet

