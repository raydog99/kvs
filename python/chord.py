import sys
import socket
import threading
import random
import time
import mutex

class Node(object):
	def __init__(self):
		pass

if __name__ == "__main__":
	import sys
	if len(sys.argv) == 2:
		local = Node(Address("127.0.0.1", sys.argv[1]))
	else:
		local = Node(Address("127.0.0.1", sys.argv[1]), Address("127.0.0.1", sys.argv[2]))
	local.start()