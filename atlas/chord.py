import sys
import socket
import threading
import random
import time
import mutex
import hashlib

class Node(object):
	def __init__(self, ip_addr, finger = [], successor = None, predecessor = None):
		self.ip_addr = ip_addr
		self.finger = finger
		self.successor = successor
		self.predecessor = predecessor

	def find_successor(self, id):
		node = find_successor(id)
		return node.successor

	def find_predecessor(self, id):
		n_prime = self
		while (id != [n_prime, n_prime.successor]):
			n_prime = n_prime.closest_preceding_finger(id)
		return n_prime

	def closest_preceding_finger(self, id):
		for i in range(m, 0, -1):
			if finger[i].node in [n, id]:
				return finger[i].node
		return n

if __name__ == "__main__":
	import sys
	if len(sys.argv) == 2:
		local = Node(Address("127.0.0.1", sys.argv[1]))
	else:
		local = Node(Address("127.0.0.1", sys.argv[1]), Address("127.0.0.1", sys.argv[2]))
	local.start()