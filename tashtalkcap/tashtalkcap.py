#!/usr/bin/env python3

import argparse
from base64 import b64encode
from collections import deque
import struct
import sys
from threading import Thread, Event
import time

from serial import Serial


LT_CRC_LUT = (
  0x0000, 0x1189, 0x2312, 0x329B, 0x4624, 0x57AD, 0x6536, 0x74BF, 0x8C48, 0x9DC1, 0xAF5A, 0xBED3, 0xCA6C, 0xDBE5, 0xE97E, 0xF8F7,
  0x1081, 0x0108, 0x3393, 0x221A, 0x56A5, 0x472C, 0x75B7, 0x643E, 0x9CC9, 0x8D40, 0xBFDB, 0xAE52, 0xDAED, 0xCB64, 0xF9FF, 0xE876,
  0x2102, 0x308B, 0x0210, 0x1399, 0x6726, 0x76AF, 0x4434, 0x55BD, 0xAD4A, 0xBCC3, 0x8E58, 0x9FD1, 0xEB6E, 0xFAE7, 0xC87C, 0xD9F5,
  0x3183, 0x200A, 0x1291, 0x0318, 0x77A7, 0x662E, 0x54B5, 0x453C, 0xBDCB, 0xAC42, 0x9ED9, 0x8F50, 0xFBEF, 0xEA66, 0xD8FD, 0xC974,
  0x4204, 0x538D, 0x6116, 0x709F, 0x0420, 0x15A9, 0x2732, 0x36BB, 0xCE4C, 0xDFC5, 0xED5E, 0xFCD7, 0x8868, 0x99E1, 0xAB7A, 0xBAF3,
  0x5285, 0x430C, 0x7197, 0x601E, 0x14A1, 0x0528, 0x37B3, 0x263A, 0xDECD, 0xCF44, 0xFDDF, 0xEC56, 0x98E9, 0x8960, 0xBBFB, 0xAA72,
  0x6306, 0x728F, 0x4014, 0x519D, 0x2522, 0x34AB, 0x0630, 0x17B9, 0xEF4E, 0xFEC7, 0xCC5C, 0xDDD5, 0xA96A, 0xB8E3, 0x8A78, 0x9BF1,
  0x7387, 0x620E, 0x5095, 0x411C, 0x35A3, 0x242A, 0x16B1, 0x0738, 0xFFCF, 0xEE46, 0xDCDD, 0xCD54, 0xB9EB, 0xA862, 0x9AF9, 0x8B70,
  0x8408, 0x9581, 0xA71A, 0xB693, 0xC22C, 0xD3A5, 0xE13E, 0xF0B7, 0x0840, 0x19C9, 0x2B52, 0x3ADB, 0x4E64, 0x5FED, 0x6D76, 0x7CFF,
  0x9489, 0x8500, 0xB79B, 0xA612, 0xD2AD, 0xC324, 0xF1BF, 0xE036, 0x18C1, 0x0948, 0x3BD3, 0x2A5A, 0x5EE5, 0x4F6C, 0x7DF7, 0x6C7E,
  0xA50A, 0xB483, 0x8618, 0x9791, 0xE32E, 0xF2A7, 0xC03C, 0xD1B5, 0x2942, 0x38CB, 0x0A50, 0x1BD9, 0x6F66, 0x7EEF, 0x4C74, 0x5DFD,
  0xB58B, 0xA402, 0x9699, 0x8710, 0xF3AF, 0xE226, 0xD0BD, 0xC134, 0x39C3, 0x284A, 0x1AD1, 0x0B58, 0x7FE7, 0x6E6E, 0x5CF5, 0x4D7C,
  0xC60C, 0xD785, 0xE51E, 0xF497, 0x8028, 0x91A1, 0xA33A, 0xB2B3, 0x4A44, 0x5BCD, 0x6956, 0x78DF, 0x0C60, 0x1DE9, 0x2F72, 0x3EFB,
  0xD68D, 0xC704, 0xF59F, 0xE416, 0x90A9, 0x8120, 0xB3BB, 0xA232, 0x5AC5, 0x4B4C, 0x79D7, 0x685E, 0x1CE1, 0x0D68, 0x3FF3, 0x2E7A,
  0xE70E, 0xF687, 0xC41C, 0xD595, 0xA12A, 0xB0A3, 0x8238, 0x93B1, 0x6B46, 0x7ACF, 0x4854, 0x59DD, 0x2D62, 0x3CEB, 0x0E70, 0x1FF9,
  0xF78F, 0xE606, 0xD49D, 0xC514, 0xB1AB, 0xA022, 0x92B9, 0x8330, 0x7BC7, 0x6A4E, 0x58D5, 0x495C, 0x3DE3, 0x2C6A, 0x1EF1, 0x0F78,
)


class CrcCalculator:
  '''Utility class to calculate the CRC of a LocalTalk frame.'''
  
  def __init__(self):
    self.reset()
  
  def reset(self):
    '''Reset the CRC calculator as though no data had been fed into it.'''
    self.reg = 0xFFFF
  
  def feed_byte(self, byte):
    '''Feed a single byte (an integer between 0 and 255) into the CRC calculator.'''
    index = (self.reg & 0xFF) ^ byte
    self.reg = LT_CRC_LUT[index] ^ (self.reg >> 8)
  
  def feed(self, data):
    '''Feed a bytes-like object into the CRC calculator.'''
    for byte in data:
      self.feed_byte(byte)
  
  def byte1(self):
    '''Returns the first byte of the frame CRC.'''
    return (self.reg & 0xFF) ^ 0xFF
  
  def byte2(self):
    '''Returns the second byte of the frame CRC.'''
    return (self.reg >> 8) ^ 0xFF
  
  def is_okay(self):
    '''If the CRC has been fed into the calculator and is correct, this will return True.'''
    return True if self.reg == 61624 else False  # this is the binary constant on B-22 of Inside Appletalk, but backwards


class LocalTalkFileWriter:
  '''Class to write LocalTalk frames to one or more files.'''
  
  PCAP_MAGIC = 0xA1B2C3D4
  PCAP_VER_MAJ = 2
  PCAP_VER_MIN = 4
  PCAP_SNAP_LENGTH = 1023
  PCAP_LINKTYPE_LT = 114
  
  def __init__(self, b64_filename=None, pcap_filename=None):
    self._b64 = open(b64_filename, 'w') if b64_filename else None
    self._pcap = open(pcap_filename, 'wb') if pcap_filename else None
    if self._pcap:
      self._pcap.write(struct.pack('<LHHLLLL', self.PCAP_MAGIC, self.PCAP_VER_MAJ, self.PCAP_VER_MIN, 0, 0, self.PCAP_SNAP_LENGTH,
                                   self.PCAP_LINKTYPE_LT))
  
  def close(self):
    '''Close associated files.'''
    if self._b64: self._b64.close()
    if self._pcap: self._pcap.close()
  
  def write_frame(self, frame, error_str=None):
    '''Write a frame to files.'''
    ts = time.time()
    ts_seconds = int(ts)
    ts_microseconds = int((ts - ts_seconds) * 1000000)
    if self._b64:
      self._b64.write(b64encode(frame).decode('ascii'))
      self._b64.write('\n')
    if self._pcap:
      self._pcap.write(struct.pack('<LLLL', ts_seconds, ts_microseconds, len(frame), len(frame)))
      self._pcap.write(frame)


class LocalTalkActivityReader:
  '''Device to take in frames and errors from a TashTalk and interpret/distribute them as requested.'''
  
  LT_TYPE_ENQ = 0x81
  LT_TYPE_ACK = 0x82
  LT_TYPE_RTS = 0x84
  LT_TYPE_CTS = 0x85
  
  TT_FRAMING_ERROR = 0xFE
  TT_FRAME_CRC_ERROR = 0xFC
  TT_FRAME_ABORTED = 0xFA
  
  def __init__(self, file_writer, verbosity, dest_filter, src_filter, discard_rts_cts, discard_enq_ack, discard_error):
    self._file_writer = file_writer
    self._verbosity = verbosity
    self._dest_filter = dest_filter
    self._src_filter = src_filter
    self._discard_rts_cts = discard_rts_cts
    self._discard_enq_ack = discard_enq_ack
    self._discard_error = discard_error
    self._frame_count = 0
    self._error_count = 0
  
  @staticmethod
  def _dump_frame_to_stdout(prefix, frame, abbreviated=False):
    if len(frame) < 5:
      sys.stdout.write('%13s: %s' % (prefix, ' '.join(('%02X' % i) for i in frame)))
    else:
      sys.stdout.write('%13s: %3d %3d %02X  ' % (prefix, frame[0], frame[1], frame[2]))
      num_bytes = min(15, len(frame)) if abbreviated else len(frame)
      sys.stdout.write(' '.join(('%02X' % i) for i in frame[3:3 + num_bytes]))
      if num_bytes < len(frame): sys.stdout.write(' ...')
      sys.stdout.write('\n')
      sys.stdout.flush()
  
  def _update_frame_count(self):
    sys.stderr.write(f'{self._frame_count} frames, {self._error_count} errors\r')
    sys.stderr.flush()
  
  def frame(self, frame):
    if len(frame) >= 5:
      if self._discard_rts_cts and frame[2] in (self.LT_TYPE_RTS, self.LT_TYPE_CTS): return
      if self._discard_enq_ack and frame[2] in (self.LT_TYPE_ENQ, self.LT_TYPE_ACK): return
      if self._dest_filter and frame[0] not in self._dest_filter: return
      if self._src_filter and frame[1] not in self._src_filter: return
    elif self._discard_error:
      return
    else:
      self._error_count += 1
    self._frame_count += 1
    if self._verbosity == 2:
      self._dump_frame_to_stdout('Good frame', frame, True)
    elif self._verbosity > 2:
      self._dump_frame_to_stdout('Good frame', frame, False)
    self._file_writer.write_frame(frame)
    if self._verbosity: self._update_frame_count()
  
  def error(self, error_code, frame):
    if self._discard_error: return
    self._frame_count += 1
    self._error_count += 1
    if not self._verbosity:
      return
    else:
      if error_code == self.TT_FRAMING_ERROR:
        self._dump_frame_to_stdout('Framing error', frame, False)
      elif error_code == self.TT_FRAME_CRC_ERROR:
        self._dump_frame_to_stdout('CRC error', frame, False)
      elif error_code == self.TT_FRAME_ABORTED:
        self._dump_frame_to_stdout('Frame aborted', frame, False)
      else:
        self._dump_frame_to_stdout('Unknown %02X' % error_code, frame, False)
      self._update_frame_count()


class TashTalkReader(Thread):
  '''Thread to read frames from a TashTalk.'''
  
  BAUDRATE = 1000000
  SERIAL_TIMEOUT = 0.25  # seconds
  
  TT_ESCAPE_BYTE = 0x00
  TT_LITERAL_ZERO = 0xFF
  TT_FRAME_DONE = 0xFD
  TT_FRAME_CRC_ERROR = 0xFC
  
  def __init__(self, port, frame_func, error_func, do_init=True):
    super().__init__()
    self._port = port
    self._frame_func = frame_func
    self._error_func = error_func
    self._do_init = do_init
    self._stop_event = Event()
    self._stopped_event = Event()
  
  def run(self):
    '''Run the thread, read frames from TashTalk until told to stop.'''
    
    self._stopped_event.set()  # don't hang the program if we blow up on the launchpad
    
    if self._stop_event.is_set(): raise RuntimeError('TashTalkReaders can only be started once')
    
    s = Serial(port=self._port, baudrate=self.BAUDRATE, rtscts=True, timeout=self.SERIAL_TIMEOUT)
    if self._do_init:
      s.write(b'\x00' * 1024)  # make sure TashTalk is in a good state
      s.write(b'\x02' + b'\x00' * 32)  # ensure it doesn't respond on any node address
      s.write(b'\x03\x00')  # turn off all optional features
    
    self._stopped_event.clear()
    
    q = deque()
    escaped = False
    crc = CrcCalculator()
    
    while not self._stop_event.is_set():
      data = s.read(1024)
      for byte in data:
        if byte == self.TT_ESCAPE_BYTE:
          escaped = True
        elif not escaped:
          q.append(byte)
          crc.feed_byte(byte)
        else:
          escaped = False
          if byte == self.TT_LITERAL_ZERO:  # literal 0x00
            q.append(0x00)
            crc.feed_byte(0x00)
          else:
            if byte == self.TT_FRAME_DONE:  # frame done
              if crc.is_okay():
                self._frame_func(bytes(q))
              else:
                self._error_func(self.TT_FRAME_CRC_ERROR, bytes(q))
            else:  # error
              self._error_func(byte, bytes(q))
            q = deque()
            crc.reset()
    
    s.close()
    self._stopped_event.set()
  
  def stop(self):
    '''Signal the thread to stop and wait until it does before returning.'''
    self._stop_event.set()
    self._stopped_event.wait()


def main(argv):
  '''Entry point.'''
  
  parser = argparse.ArgumentParser(description='Use TashTalk to capture LocalTalk traffic.')
  parser.add_argument('-p', metavar='PORT', required=True, help='serial port where TashTalk is connected')
  parser.add_argument('-n', action='store_true', help="don't initialize TashTalk, passively receive data only")
  parser.add_argument('-d', metavar='NODE', nargs='+', type=int, help='only capture traffic to these destination nodes')
  parser.add_argument('-s', metavar='NODE', nargs='+', type=int, help='only capture traffic from these source nodes')
  parser.add_argument('-r', action='store_true', help='discard RTS and CTS')
  parser.add_argument('-a', action='store_true', help='discard ENQ and ACK')
  parser.add_argument('-e', action='store_true', help='discard error frames')
  parser.add_argument('-b', metavar='FILE', help='base64 lines file to write captured traffic to')
  parser.add_argument('-c', metavar='FILE', help='pcap file to write captured traffic to')
  parser.add_argument('-v', action='count', help='increase verbosity (may be specified once or twice)')
  parser.add_argument('-q', action='store_true', help='write nothing at all to stdout or stderr')
  args = parser.parse_args(argv[1:])
  
  fw = LocalTalkFileWriter(args.b, args.c)
  verbosity = 0 if args.q else 1 + (args.v if args.v else 0)
  ltar = LocalTalkActivityReader(fw, verbosity, set(int(i) for i in args.d) if args.d else (), 
                                 set(int(i) for i in args.s) if args.s else (), args.r, args.a, args.e)
  ttr = TashTalkReader(args.p, ltar.frame, ltar.error, not args.n)
  ttr.start()
  
  try:
    while True: time.sleep(0.25)
  except KeyboardInterrupt:
    ttr.stop()


if __name__ == '__main__': sys.exit(main(sys.argv))
