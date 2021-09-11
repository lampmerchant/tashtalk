from itertools import chain
import logging
import os
import select
import socket
import struct
import time

from ..util import hexdump_lines, TashTalkReceiver, TashTalkTransmitter, NodeIdSet


LTOUDP_GROUP = '239.192.76.84'  # the last two octets spell 'LT'
LTOUDP_PORT = 1954


class LtoudpDaemon:
  
  def __init__(self, serial_obj):
    self.serial_obj = serial_obj
    self.receiver = None
    self.sender = None
    self.socket = None
    self.sender_id = None
    self.node_id_set = None
  
  def initialize(self):
    '''Set up the serial and socket connections the daemon will use.'''
    
    self.receiver = TashTalkReceiver(self.serial_obj)
    self.sender = TashTalkTransmitter(self.serial_obj)
    self.sender.initialize()
    
    self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
    self.socket.bind((LTOUDP_GROUP, LTOUDP_PORT))
    self.socket.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 1)
    self.socket.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP,
                           struct.pack('=4sL', socket.inet_aton(LTOUDP_GROUP), socket.INADDR_ANY))
    #TODO add an option for this instead of forcing INADDR_ANY?
    
    self.sender_id = struct.pack('>L', os.getpid())
    self.node_id_set = NodeIdSet(self.sender)
  
  def service_tashtalk(self):
    '''Called when there is data from TashTalk that could finish one or more frames.'''
    
    for frame in self.receiver.get_frames():
      
      frame_dest = frame[0]
      frame_type = frame[2]
      if frame_type in (0x84, 0x85):
        logging.debug('not retransmitting RTS/CTS frame')
        continue
      if frame_type == 0x81 and frame_dest in self.node_id_set:
        logging.debug('not retransmitting ENQ frame that TashTalk has already responded to')
        continue
      
      self.socket.sendto(self.sender_id + frame[:-2], (LTOUDP_GROUP, LTOUDP_PORT))
  
  def service_udp(self):
    '''Called when there is a UDP datagram to be parsed and potentially forwarded.'''
    
    data, sender_addr = self.socket.recvfrom(65507)
    
    if logging.getLogger().isEnabledFor(logging.DEBUG):
      logging.debug('\n'.join(chain(('received UDP datagram from %s:' % str(sender_addr),),
                                    (line for line in hexdump_lines(data)))))
    
    if len(data) < 7:
      logging.info('ignoring invalid too-small UDP datagram')
      return
    
    if data[0:4] == self.sender_id:  #TODO check sender_addr too
      logging.info('ignoring echoed UDP datagram')
      return
    
    frame = data[4:]
    frame_dest = frame[0]
    frame_src = frame[1]
    frame_type = frame[2]
    if not (frame_type == 0x81 and frame_dest == frame_src): self.node_id_set.touch(frame_src)
    
    self.sender.send_frame(frame)
  
  def run(self):
    '''Run the daemon.'''
    
    last_check_expiration = time.monotonic()
    
    while True:
      rlist, wlist, xlist = select.select((self.receiver, self.socket), (), (), 10)
      if self.receiver in rlist: self.service_tashtalk()
      if self.socket in rlist: self.service_udp()
      
      now = time.monotonic()
      if now - last_check_expiration >= 10:
        self.node_id_set.check_expiration()
        last_check_expiration = now
