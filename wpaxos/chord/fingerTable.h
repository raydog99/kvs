#ifndef _FINGERTABLE_H
#define _FINGERTABLE_H

#include "server.h"
#include "helperFunctions.h"
#include <vector>
#include <string>

#define successor nodes[1];

class ChordNodeImpl;

struct FingerEntry{
	ChordNode::Client nodeRef;
	std::string nodeIdentifier;
}

struct FingerTable{
	FingerTable(const int fingerTableSize, ChordNode::Client nodeCapability, const std::string nodeIdentifier);

	std::string getStart(int idx);
	Interval getInterval(int idx);
	
	ChordNode::Client getSuccessor();
	ChordNode::Client setSuccessor(FingerEntry successor);

	int fingerTableSize;
	std::string nodeIdentifier;
	std::vector<FingerEntry> nodes;


	// fingeriterator to abstract fault tolerance
	std::vector<FingerEntry> successors;
};

#endif