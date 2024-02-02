#include "server.h"
#include "helper_functions.h"
#include "ChordProtocol.capnp.h"
#include <capnp/message.h>
#include <iostream>

ChordNodeImpl::ChordNodeImpl(const std::string& ipAddress) : ipAddress(ipAddress), identifier(ipAddress) { }

kj::Promise<NodeInfo> ChordNodeImpl::findSuccessor(FindSuccessorContext params) {
    auto identifier = params.getParams();

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

kj::Promise<NodeInfo> ChordNodeImpl::findPredecessor(const std::string& id) {
    NodeInfo currentNode = *this;

    NodeInfo closestPredecessor = currentNode;

    while (!is_in_interval(id, closestPredecessor.getId(), closestPredecessor.getSuccessor(), true, false)) {
        auto request = currentNode.find_closest_preceding_finger_request();
        request.setParams(id);

        auto responsePromise = request.send().get_result();
        auto response = responsePromise.wait(client.getWaitScope());

        closestPredecessor = response.getClosestPrecedingFinger();
    }

    return closestPredecessor;
}

kj::Promise<NodeInfo> ChordNodeImpl::closestPrecedingFinger(const std::string& id) {
    auto identifier = params.getParams();
    NodeInfo n = *this;

    for (int i = m - 1; i > 0; i--){
        if (is_in_interval(finger_table[i].getId(), n.getId(), identifier)){
            return finger_table[i].nodeInfo;
        }
    }
    return n;
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

    auto successor_request = findSuccessor(finger[i].start);

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

kj::Promise<void> ChordNodeImpl::updateFingerTable(UpdateFingerContext params){
    std::string sIdentifier = params.getParams();
    NodeInfo n = *this;

    if (is_in_interval(sIdentifier, n.getId(), fingerTable[fingerIdx].getId())){
        finger[i].node = s;
        p = predecessor;
        p.update_finger_table(s, i);
    }
}