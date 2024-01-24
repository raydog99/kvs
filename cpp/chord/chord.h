#ifndef _CHORD_H
#define _CHORD_H

#include "string"
#include <vector>

class ChordNode {
public:
	ChordNode(const std::string& identifier) : identifier(identifier) { }
	virtual ~ChordNode() = default;

	virtual ChordNode* find_successor(std::string id) = 0;
	virtual ChordNode* find_predecessor(std::string id) = 0;
	virtual ChordNode* closest_preceding_finger(std::string id) = 0;

	ChordNode* successor;
	ChordNode* predecessor;

	std::string get_identifier() const {
        return identifier;
    }

    ChordNode* get_finger_table(int idx){
    	return this->finger_table[idx];
    }

private:
	std::string identifier;
	std::vector<ChordNode*>finger_table;
};

#endif