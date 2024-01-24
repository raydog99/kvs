#include "server.h"
#include <stdio.h>

// ChordNode's identifier should be SHA-1 hash of ipAddr, i.e. ChordNode(sha1hash(ipAddr))
Server::Server(const std::string& ipAddr) : ChordNode(ipAddr), ipAddr(ipAddr) { }

ChordNode* Server::find_successor(std::string id) {
    ChordNode* predecessor = find_predecessor(id);
    return predecessor->successor;
}

ChordNode* Server::find_predecessor(std::string id) {
    ChordNode* node = static_cast<ChordNode*>(this);
    while ((id > node->get_identifier()) && (id < node->successor->get_identifier())) {
        node = node->closest_preceding_finger(id);
    }
    return node;
}

ChordNode* Server::closest_preceding_finger(std::string id) {
    for (int i = 32; i >= 1; i--) {
    	// Abstract out finger table
        if ((this->get_identifier() < get_finger_table(i)->get_identifier()) && (get_finger_table(i)->get_identifier() < id)) {
            return get_finger_table(i);
        }
    }
    return this;
}