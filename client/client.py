# Contains functions that handle networking for the client.

import socket
from base import *

class Packet:
    ''' Structure to store network packets. '''
    def __init__(self, _type = None, _length = 0, _data = ''):
        self.type = _type
        self.length = _length
        self.data = _data


class Socket():
    def __init__(self):
        self.sock = None

    def receive_full(self, length):
        ''' To ensure everything is received. '''
        msg = ''
        while len(msg) < length:
            temp = self.sock.recv(length - len(msg))
            if temp == '':
                raise RuntimeError("Socket connection broken!")
            msg = msg + temp
        return msg
        
    def send_full(self, msg):
        # To ensure everything is sent.
        sent = 0
        while sent < len(msg):
            sent_now = self.sock.send(msg[sent:])
            if sent_now == 0:
                raise RuntimeError("Socket connection broken!")
            sent += sent_now
        return sent
    
    def connect(self, server, server_port):
        # Connects to specified port on the specified server.
        if self.sock != None:
            self.disconnect()

        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((server, server_port))
    
    def disconnect(self):
        # For completeness.
        self.sock.close()
        self.sock = None
    
    def send(self, packet):
        # Sends a packet as specified by the Packet object @packet.
        if packet.type == None:
            return
    
        self.sock.send(int2str(packet.type, 1))
        self.sock.send(int2str(packet.length, 4))
        self.send_full(packet.data)
    
    def receive(self):
        # Receives a packet and returns appropriate Packet object.
        packet = Packet()
    
        packet.type = str2int(self.sock.recv(1))
        packet.length = str2int(self.sock.recv(4))
        packet.data = self.receive_full(packet.length)
    
        return packet

