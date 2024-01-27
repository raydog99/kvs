#include "server.h"
#include <capnp/ez-rpc.h>
#include <ChordProtocol.capnp.h>
#include <iostream>

ChordNodeImpl::ChordNodeImpl(const std::string& ipAddress) : ipAddress(ipAddress), identifier(ipAddress) { }

kj::Promise<NodeInfo> ChordNodeImpl::findSuccessor(FindSuccessorContext params) {
    Text key = params.getKey();

    auto identifier = params.getParams()

    capnp::EzRpcClient client(ipAddress);
    ChordProtocol::Client chord_client = client.getMain<ChordProtocol>();

    auto& waitScope = client.getWaitScope();

    std::cout << "Finding successor...";
    std::cout.flush();

    auto predecessor_request = chord_client.find_predecessor_request();
    predecessor_request.setOp(key);

    auto successor_promise = predecessor_request.send().get_successor();
    auto successor = successor_promise.wait(waitScope);

    KJ_ASSERT(response.getValue() == 123);

    std::cout << "PASS" << std::endl;
}

kj::Promise<void> ChordNodeImpl::join(){
    for (int i = 0; i < m; i++){
        finger_table[i] = nodeInfo;
    }
    predecessor = nodeInfo;
}

kj::Promise<void> ChordNodeImpl::join(NodeInfo node){
    initFingerTable(node);
    updateOthers();
    // move keys in (predecessor, n] from successor
}

void ChordNodeImpl::initFingerTable(NodeInfo node){
    std::string nodeIpAddress = node.ipAddress;

    capnp::EzRpcClient client(ipAddress);
    ChordProtocol::Client chord_client = client.getMain<ChordProtocol>();

    auto& waitScope = client.getWaitScope();

    std::cout << "Initializing finger table...";
    std::cout.flush();

    auto successor_request = find_successor(finger[i].start);

    return successor_request.then([this, i, myNodeId](NodeInfo successor) mutable {
        fingerTable[i + 1] = successor;

        if (successor.getId() == myNodeId) {
            finger[i + 1].node = finger[i].node;
        } else {
            auto predecessor_request = find_predecessor_request(finger[i + 1].start);
            return predecessor_request.send().get_predecessor().then([this, i](NodeInfo predecessor) {
                finger[i + 1].node = predecessor;
            });
        }
    });

    std::cout << "PASS" << std::endl;

    for(int i = 0; i < m; i++){
        if (true) {
            finger[i + 1].node = finger[i].node;
        }
        else {
            finger[i + 1].node = node.findSuccessor(finger[i + 1].start)
        }
    }
}

kj::Promise<void> ChordNodeImpl::updateOthers(){
    for(int i = 0; i < m; i++){
        p = findPredecessor(n - 2^(i-1))
        p.updateFingerTable(n, i);
    }
}

kj::Promise<void> ChordNodeImpl::updateFingerTable(){
    if (true){
        finger[i].node = s;
        p = predecessor;
        p.update_finger_table(s, i);
    }
}