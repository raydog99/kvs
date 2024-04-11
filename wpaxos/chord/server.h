#ifndef _SERVER_H
#define _SERVER_H

#include "ChordProtocol.capnp.h"
#include "stabilize.h"
#include "fingerTable.h"
#include "helperFunctions.h"

#include <string>
#include <stdio.h>
#include <iostream>

#include <capnp/rpc.h>
#include <capnp/message.h>
#include <capnp/rpc-twoparty.h>

#include <kj/debug.h>
#include <kj/thread.h>
#include <kj/compat/gtest.h>
#include <kj/async-io.h>

struct FingerTable;

class ChordNodeImpl final: public ChordNode::Server {
public:
    ChordNodeImpl(std::string ipAddress);

    kj::Promise<void> findSuccessor(FindSuccessorContext context) override;
    kj::Promise<void> findPredecessor(FindPredecessorContext context) override;
    kj::Promise<void> closestPrecedingFinger(ClosestPrecedingFingerContext context) override;

    kj::Promise<void> join(JoinContext context) override;
    kj::Promise<void> notify(NotifyContext context) override;
private:
    void stabilize();
    void fixFingers();

    std::string ipAddress;
    std::string identifier;

    FingerTable *fingerTable;
	ChordNode::Client predecessor;
};

#endif