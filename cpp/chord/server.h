#ifndef _SERVER_H
#define _SERVER_H

#include "chord.h"
#include <string>
#include <stdio.h>

class ChordNodeImpl final: public ChordNode::Server {
public:
    explicit ChordNodeImpl(const std::string& nodeId);
    kj::Promise<NodeInfo::Reader> findSuccessor(FindSuccessorContext context) override;
    kj::Promise<NodeInfo::Reader> findPredecessor(FindPredecessorContext context) override;
    kj::Promise<NodeInfo::Reader> closestPrecedingFinger(ClosestPrecedingFingerContext context) override;
    kj::Promise<NodeInfo::Reader> updateFingerTable(UpdateFingerTableContext context) override;
    void join(NodeInfo node) override;
    void initFingerTable(InitFingerTableContext context) override;

    NodeInfo getSuccessor();
    NodeInfo getPredecessor();
private:
	NodeInfo nodeInfo;
	std::string ipAddress;
	std::string identifier;
	vector<NodeInfo::Reader> fingerTable;
}

#endif