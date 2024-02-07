#include "fingerTable.h"

FingerTable::FingerTable(const int fingerTableSize, ChordNode::Client nodeCapability, const std::string nodeIdentifier){
	this->fingerTableSize = fingerTableSize;
	this->nodeIdentifier = nodeIdentifier;

	for (int i = 0; i < this->fingerTableSize; i++){
		this->nodes.emplace_back(nodeCapability);
	}
}

std::string FingerTable::getStart(int idx){
	int n = std::stoi(nodeIdentifier);
    int k = idx;
    int m = fingerTableSize;

    int startIndex = (n + (1 << (k - 1))) % (1 << m);
    return std::to_string(startIndex);
}

Interval FingerTable::getInterval(int idx){
	std::string leftBound = getStart(idx);
	std::string rightBound = getStart( (idx + 1) % this->fingerTableSize);
	return Interval(leftBound, rightBound, true, false);
}

ChordNode::Client FingerTable::getSuccessor(){
	return this->successor;
}

ChordNode::Client FingerTable::setSuccessor(FingerEntry successor){
	this->successor = successor;
}