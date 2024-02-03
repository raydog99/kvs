#include "server.h"

ChordNodeImpl::ChordNodeImpl(std::string ipAddress) {
    this->ipAddress = ipAddress;
    this->identifier = ipAddress; // (!)
    this->fingerTableSize = 32;

    this->successor = this;
    this->predecessor = this;

    for (int i = 0; i < this->fingerTableSize; i++){
        fingerTable.push_back(this);
    }
}

kj::Promise<void> ChordNodeImpl::findSuccessor(FindSuccessorContext context) {
    std::cout << "Finding successor...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    auto io = kj::setupAsyncIo();
    auto server = kj::heap<ChordNodeImpl>(this->ipAddress);
    ChordNode::Client capability = kj::mv(server);

    auto predecessorRequest = capability.findPredecessorRequest();
    predecessorRequest.setKey(key);
    auto predecessor = predecessorRequest.send().wait(io.waitScope);

    auto successor = predecessor.getResult().getSuccessorRequest().send().wait(io.waitScope).getResult();
    context.getResults().setResult(successor);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::findPredecessor(FindPredecessorContext context) {
    std::cout << "Finding predecessor...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    auto node = this;
    while(true){
        auto io = kj::setupAsyncIo();
        auto server = kj::heap<ChordNodeImpl>(node->ipAddress);
        ChordNode::Client capability = kj::mv(server);

        auto closestPrecedingFingerRequest = capability.closestPrecedingFingerRequest();
        closestPrecedingFingerRequest.setKey(key);
        auto node = closestPrecedingFingerRequest.send().wait(io.waitScope);
        auto nodeIdentifier = node.getResult().getIdentifierRequest().send().wait(io.waitScope).getResult();

        auto nodeInRangeRequest = node.getResult().inRangeRequest();
        nodeInRangeRequest.setKey(key);
        nodeInRangeRequest.setPreviousNodeIdentifier(nodeIdentifier);
        nodeInRangeRequest.setLeftInclusive(false);
        nodeInRangeRequest.setRightInclusive(true);

        auto nodeInRange = nodeInRangeRequest.send().wait(io.waitScope).getResult();
        if (nodeInRange){
            context.getResults().setResult(node.getResult());
        }
        return kj::READY_NOW;
    }
}

kj::Promise<void> ChordNodeImpl::closestPrecedingFinger(ClosestPrecedingFingerContext context) {
    std::cout << "Finding closest preceding finger...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    for (int i = this->fingerTableSize - 1; i > 0; i--){
        auto io = kj::setupAsyncIo();
        auto server = kj::heap<ChordNodeImpl>(this->fingerTable[i]->ipAddress);
        ChordNode::Client capability = kj::mv(server);

        auto nodeIdentifier = capability.getIdentifierRequest().send().wait(io.waitScope).getResult();

        bool isClosestPrecedingFinger = is_in_interval(nodeIdentifier, this->identifier, key, false, false);
        if(isClosestPrecedingFinger){

            auto closestPrecedingServer = kj::heap<ChordNodeImpl>(fingerTable[i]->ipAddress);
            ChordNode::Client closestPrecedingCapability = kj::mv(closestPrecedingServer);
            context.getResults().setResult(closestPrecedingCapability);
            return kj::READY_NOW;
        }
    }

    auto closestPrecedingServer = kj::heap<ChordNodeImpl>(this->ipAddress);
    ChordNode::Client closestPrecedingCapability = kj::mv(closestPrecedingServer);
    context.getResults().setResult(closestPrecedingCapability);
    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::join(JoinContext context){
    std::cout << "Joining...";
    std::cout.flush();

    auto node = context.getParams().getNode();

    this->predecessor = nullptr;

    auto io = kj::setupAsyncIo();
    auto successorRequest = node.findSuccessorRequest();
    successorRequest.setKey(this->identifier);
    auto successor = successorRequest.send().wait(io.waitScope);

    this->successor = successor.getResult();

    return kj::READY_NOW;
}


kj::Promise<void> ChordNodeImpl::notify(NotifyContext context){
    std::cout << "Joining...";
    std::cout.flush();

    return kj::READY_NOW;
}

void stabilize() { }

void fixFingers() { }


kj::Promise<void> ChordNodeImpl::getSuccessor(GetSuccessorContext context){
    std::cout << "Getting successor...";
    std::cout.flush();

    // Keep a copy of client to ensure non-zero ref count?
    auto server = kj::heap<ChordNodeImpl>(this->successor->ipAddress);
    ChordNode::Client capability = kj::mv(server);
    context.getResults().setResult(capability);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::getPredecessor(GetPredecessorContext context){
    std::cout << "Getting predecessor...";
    std::cout.flush();

    auto server = kj::heap<ChordNodeImpl>(this->predecessor->ipAddress);
    ChordNode::Client capability = kj::mv(server);
    context.getResults().setResult(capability);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::inRange(InRangeContext context){
    std::cout << "Checking in range...";
    std::cout.flush();

    auto key = context.getParams().getKey();
    auto previousNodeIdentifier = context.getParams().getPreviousNodeIdentifier();
    auto leftInclusive = context.getParams().getLeftInclusive();
    auto rightInclusive = context.getParams().getRightInclusive();

    bool isInRange = is_in_interval(key, previousNodeIdentifier, this->identifier, leftInclusive, rightInclusive);

    context.getResults().setResult(isInRange);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::getIdentifier(GetIdentifierContext context){
    std::cout << "Getting identifier...";
    std::cout.flush();

    context.getResults().setResult(this->identifier);

    return kj::READY_NOW;
}