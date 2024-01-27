#include "finger_table.h"

FingerTable::FingerTable(const int tableSize, const std::string ipAddress) : tableSize(tableSize), ipAddress(ipAddress) {
	for (int i = 0; i < tableSize; i++){
		fingerTable.push_back(ipAddress);
	}
}

FingerTable::~FingerTable() { };

std::string FingerTable::start(int fingerIndex){
	// sha1hash
	const position = fingerTable[fingerIdx] + pow(2, k - 1);
	return position % pow(2, tableSize)
}

NodeInfo FingerTable::node(int fingerIndex){
	// first node >= fingerTable[fingerIdx].start
	return fingerTable[fingerIdx];
}