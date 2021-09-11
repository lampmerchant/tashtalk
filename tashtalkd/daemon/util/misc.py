'''Miscellaneous functions and classes.'''

import time


def hexdump_lines(data):
  '''Utility function to make a hex dump of a bytes-like object.'''
  for index in range(0, len(data), 16):
    line_data = data[index:index+16]
    yield '%06X  %-47s  %-16s' % (index,
                                  ' '.join(('%02X' % i) for i in line_data),
                                  ''.join((chr(i) if 32 <= i < 127 else '.') for i in line_data))


class NodeIdSet:
  
  def __init__(self, sender, timeout=600):
    self.sender = sender
    self.timeout = timeout
    self.node_ids = {}
  
  def touch(self, node_id):
    if node_id in self.node_ids:
      self.node_ids[node_id] = time.monotonic()
    else:
      self.node_ids[node_id] = time.monotonic()
      self.sender.set_node_ids(self.node_ids.keys())
  
  def check_expiration(self):
    expired = set()
    now = time.monotonic()
    for node_id, last_time in self.node_ids.items():
      if now - last_time >= self.timeout: expired.add(node_id)
    if expired:
      for node_id in expired: self.node_ids.pop(node_id)
      self.sender.set_node_ids(self.node_ids.keys())
  
  def __contains__(self, item):
    return True if item in self.node_ids else False
