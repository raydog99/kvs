#include "chord.h"
#include <stdio.h>

Chord::Chord(){
	ChordNode find_successor(string id){
		ChordNode predecessor = find_predecessor(id);
		return predecessor.successor;
	}

	ChordNode find_predecessor(string id){
		ChordNode node = *this;
		while ( (id > node.ipaddr) && (id < node.successor.ipaddr) == false){
			node = node.closest_preceding_finger(id);
		}
	}

	ChordNode closest_preceding_finger(string id){
		for (int i = 32; i >= 1; i--){
			if ( (*this.ipaddr < finger_table[i].node) && (finger_table[i].node < id)){
				return finger_table[i].node
			}
		}
		return *this;
	}
}