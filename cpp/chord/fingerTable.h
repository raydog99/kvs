#ifndef _FINGERTABLE_H
#define _FINGERTABLE_H

#include "server.h"
#include "helperFunctions.h"
#include <vector>
#include <string>

class ChordNodeImpl;

struct FingerTable{
	FingerTable(const int fingerTableSize, ChordNode::Client nodeCapability, const std::string nodeIdentifier);

	std::string getStart(int idx);
	Interval getInterval(int idx);
	ChordNode::Client getSuccessor();

	int fingerTableSize;
	std::string nodeIdentifier;
	std::vector<ChordNode::Client> nodes;
};

#endif