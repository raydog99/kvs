#ifndef _FINGER_TABLE_H
#define _FINGER_TABLE_H

#include <vector>
#include "NodeInfo.capnp.h"
#include <string>
#include <stdio.h>

class FingerTable {
public:
    FingerTable(const int tableSize, const std::string ipAddress);
    ~FingerTable();

    std::string start(int fingerIndex);
    NodeInfo node(int fingerIndex);
private:
    const int tableSize;
    const std::string ipAddress;
    vector<NodeInfo::Reader> fingerTable;
}

#endif