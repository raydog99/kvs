#ifndef _SERVER_H
#define _SERVER_H

#include <string>
#include "ChordProtocol.capnp.h"
#include <stdio.h>

class ChordNodeImpl final: public ChordNode::Server {
public:
    explicit ChordNodeImpl(const std::string& nodeId);
    kj::Promise<void> findSuccessor(FindSuccessorContext context) override;
    kj::Promise<void> findPredecessor(FindPredecessorContext context) override;
    kj::Promise<void> closestPrecedingFinger(ClosestPrecedingFingerContext context) override;
    kj::Promise<void> updateFingerTable(UpdateFingerTableContext context) override;
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