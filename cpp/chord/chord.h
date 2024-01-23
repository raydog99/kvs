#ifndef _CHORD_H
#define _CHORD_H

class ChordNode{
	ChordNode find_successor(id);
	ChordNode find_predecessor(id);
	ChordNode closest_preceding_finger(id);

	ChordNode successor;
	ChordNode predecessor;
private:
	// 48 bits for IPv4 (32 bits) + port (16 bits)
	// 144 bits for IPv6 (128 bits) + port (16 bits)
	string ipaddr;
	vector<ChordNode>finger_table;
}

#endif