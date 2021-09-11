'''Code to abstract TashTalk transmitter side.'''

from itertools import chain
import logging

from .lt_crc import CrcCalculator
from .misc import hexdump_lines


class TashTalkTransmitter:
  '''Abstracted TashTalk transmitter that deals in commands rather than raw serial port data.'''
  
  def __init__(self, serial_obj):
    self.serial_obj = serial_obj
  
  def initialize(self):
    '''Initialize TashTalk by bringing it to a known-good state and setting it not to respond to any node IDs.'''
    self.known_state()
    self.set_node_ids(())
  
  def known_state(self):
    '''Return TashTalk to the known state where it's awaiting a command byte.'''
    self.serial_obj.write(b'\x00' * 1024)
    logging.debug('return to known state sent to TashTalk')
  
  def send_frame(self, frame):
    '''Accept a frame without CRC bytes, check it for validity, and send it to TashTalk with the correct CRC bytes.'''
    if len(frame) < 3:
      raise ValueError('frame must be 3 bytes minimum')
    elif len(frame) == 3 and not (frame[2] & 0x80):
      raise ValueError('a 3-byte frame must be a control frame')
    elif (frame[2] & 0x80) and not len(frame) == 3:
      raise ValueError('a control frame must be 3 bytes in length')
    elif len(frame) == 4:
      raise ValueError('invalid frame length')
    elif len(frame) > 603:
      raise ValueError('frame may be 603 bytes maximum')
    elif len(frame) >= 5 and (((frame[3] & 0x3) << 8) | frame[4]) != len(frame) - 3:
      raise ValueError('frame length does not match frame length field')
    crc = CrcCalculator()
    crc.feed(frame)
    frame += bytes((crc.byte1(), crc.byte2()))
    self.serial_obj.write(b'\x01' + frame)
    if logging.getLogger().isEnabledFor(logging.DEBUG):
      logging.debug('\n'.join(chain(('frame sent to TashTalk:',), (line for line in hexdump_lines(frame)))))
  
  def set_node_ids(self, ids):
    '''Set the node IDs for which TashTalk should respond to RTS, CTS, and ENQ control frames.'''
    id_bitmap = bytearray(32)
    for id in ids:
      byte_num, bit_num = divmod(id, 8)
      id_bitmap[byte_num] |= (1 << bit_num)
    self.serial_obj.write(b'\x02' + id_bitmap)
    logging.debug('ID bitmap sent to TashTalk: %s', ', '.join(hex(id) for id in ids) if ids else '(empty)')
