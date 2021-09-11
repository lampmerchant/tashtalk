'''Code to abstract TashTalk receiver side.'''

from collections import deque
from itertools import chain
import logging

from .lt_crc import CrcCalculator
from .misc import hexdump_lines


class TashTalkReceiver:
  '''Abstracted TashTalk receiver that deals in frames rather than raw serial port data.'''
  
  def __init__(self, serial_obj):
    self.serial_obj = serial_obj
    self.crc = CrcCalculator()
    self.next_frame = deque()
    self.escape_on = False
    self.frames = deque()
  
  def _reset(self):
    self.crc.reset()
    self.next_frame.clear()
    self.escape_on = False
  
  def _feed(self, data):
    for byte in data:
      if self.escape_on:
        if byte == 0xFF:  # literal 0 byte
          self.next_frame.append(0)
          self.crc.feed_byte(0)
        elif byte == 0xFD:  # Frame Delimiter
          if self.crc.is_okay() and len(self.next_frame) >= 5:
            frame = bytes(self.next_frame)
            if logging.getLogger().isEnabledFor(logging.DEBUG):
              logging.debug('\n'.join(chain(('TashTalk received good frame:',), (line for line in hexdump_lines(frame)))))
            self.frames.append(frame)
          else:
            if logging.getLogger().isEnabledFor(logging.INFO):
              frame = bytes(self.next_frame)
              logging.info('\n'.join(chain(('TashTalk received bad frame:',), (line for line in hexdump_lines(frame)))))
          self._reset()
        elif byte == 0xFE:  # Framing Error
          if logging.getLogger().isEnabledFor(logging.INFO):
            frame = bytes(self.next_frame)
            logging.info('\n'.join(chain(('TashTalk detected framing error:',), (line for line in hexdump_lines(frame)))))
          self._reset()
        elif byte == 0xFA:  # Frame Aborted
          if logging.getLogger().isEnabledFor(logging.INFO):
            frame = bytes(self.next_frame)
            logging.info('\n'.join(chain(('TashTalk detected aborted frame:',), (line for line in hexdump_lines(frame)))))
          self._reset()
        self.escape_on = False
      elif byte == 0x00:
        self.escape_on = True
      else:
        self.next_frame.append(byte)
        self.crc.feed_byte(byte)
  
  def _service_serial_port(self):
    while True:
      data = self.serial_obj.read(16384)
      if not data: break
      logging.debug('read %d bytes from TashTalk', len(data))
      self._feed(data)
  
  def get_frames(self):
    '''Get data from TashTalk and yield any frames that were completed since the last time this was called.'''
    self._service_serial_port()
    for frame in self.frames:
      yield frame
    self.frames.clear()
  
  def fileno(self):
    '''This function makes it possible to pass a receiver into select.select, at least on POSIX platforms.'''
    return self.serial_obj.fileno()
