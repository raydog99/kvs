#include "server.h"

ChordNodeImpl::ChordNodeImpl(std::string ipAddress) : fingerTable(), predecessor(this->thisCap()) {
    this->ipAddress = ipAddress;
    this->identifier = ipAddress; // (!)

    stabilize();    // run every 30s
}

void ChordNodeImpl::stabilize() {
    auto successor = this->fingerTable->getSuccessor();

    auto io = kj::setupAsyncIo();
    auto newSuccessorPromise = successor.getPredecessorRequest().send().wait(io.waitScope);

    auto newSuccessor = newSuccessorPromise.getPredecessor();
    auto newSuccessorIdentifier = newSuccessorPromise.getPredecessor().getIdentifierRequest().send().wait(io.waitScope).getIdentifier();

    Interval newSuccessorRange = Interval(
        this->identifier,
        newSuccessorIdentifier,
        false,
        false
        );

    bool foundNewSuccessor = newSuccessorRange.inRange(newSuccessorIdentifier);
    if (foundNewSuccessor == true){
        this->fingerTable->nodes[1] = this->thisCap();
    }

    auto nodeServer = kj::heap<ChordNodeImpl>(this->ipAddress);
    ChordNode::Client nodeCapability = kj::mv(nodeServer);

    auto notifyPromise = successor.notifyRequest();
    notifyPromise.setNode(nodeCapability);
    notifyPromise.send().wait(io.waitScope);
}

kj::Promise<void> ChordNodeImpl::notify(NotifyContext context){
    std::cout << "Notify called...";
    std::cout.flush();

    auto node = context.getParams().getNode();

    auto io = kj::setupAsyncIo();
    std::string nodeIdentifier = node.getIdentifierRequest().send().wait(io.waitScope).getIdentifier();

    std::string predecessorIdentifier = this->predecessor.getIdentifierRequest().send().wait(io.waitScope).getIdentifier();

    Interval predecessorRange = Interval(
        predecessorIdentifier,
        this->identifier,
        false,
        false
        );

    bool predecessorIsNull = false;     // call to check if predecessor is alive
    bool nodeIsPredecessor = predecessorRange.inRange(nodeIdentifier);

    if (predecessorIsNull == true || nodeIsPredecessor == true){
        this->predecessor = node;
    }

    return kj::READY_NOW;
}


void ChordNodeImpl::fixFingers() {
    auto randomIndex = 0; // [0, m-1]

    auto io = kj::setupAsyncIo();
    auto server = kj::heap<ChordNodeImpl>(this->ipAddress);
    ChordNode::Client capability = kj::mv(server);

    auto indexSuccessorRequest = capability.findSuccessorRequest();
    indexSuccessorRequest.setKey(this->fingerTable->getStart(randomIndex));
    auto indexSuccessor = indexSuccessorRequest.send().wait(io.waitScope).getSuccessor();

    this->fingerTable->nodes[randomIndex] = indexSuccessor;
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

    auto successor = predecessor.getPredecessor().getSuccessorRequest().send().wait(io.waitScope).getSuccessor();
    context.getResults().setSuccessor(successor);

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
        auto node = closestPrecedingFingerRequest.send().wait(io.waitScope).getClosestPrecedingNode();
        auto nodeIdentifier = node
            .getIdentifierRequest()
            .send()
            .wait(io.waitScope)
            .getIdentifier();

        auto nodeInRangeRequest = node.inRangeRequest();
        nodeInRangeRequest.setKey(key);
        nodeInRangeRequest.setPreviousNodeIdentifier(nodeIdentifier);
        nodeInRangeRequest.setLeftInclusive(false);
        nodeInRangeRequest.setRightInclusive(true);

        auto nodeInRange = nodeInRangeRequest.send().wait(io.waitScope).getResult();
        if (nodeInRange){
            context.getResults().setPredecessor(node);
        }
        return kj::READY_NOW;
    }
}

kj::Promise<void> ChordNodeImpl::closestPrecedingFinger(ClosestPrecedingFingerContext context) {
    std::cout << "Finding closest preceding finger...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    for (int i = this->fingerTable->fingerTableSize - 1; i > 0; i--){
        auto io = kj::setupAsyncIo();

        auto nodeIdentifier = this->fingerTable->nodes[i].getIdentifierRequest().send().wait(io.waitScope).getIdentifier();

        Interval closestPrecedingRange = Interval(
            this->identifier,
            key,
            false,
            false
            );

        bool isClosestPrecedingFinger = closestPrecedingRange.inRange(key);
        if(isClosestPrecedingFinger){
            context.getResults().setClosestPrecedingNode(this->fingerTable->nodes[i]);
            return kj::READY_NOW;
        }
    }

    context.getResults().setClosestPrecedingNode(this->thisCap());
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
    auto successor = successorRequest.send().wait(io.waitScope).getSuccessor();

    this->fingerTable->nodes[1] = successor;

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::getSuccessor(GetSuccessorContext context){
    std::cout << "Getting successor...";
    std::cout.flush();

    context.getResults().setSuccessor(this->fingerTable->getSuccessor());

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::getPredecessor(GetPredecessorContext context){
    std::cout << "Getting predecessor...";
    std::cout.flush();

    context.getResults().setPredecessor(this->predecessor);

    return kj::READY_NOW;
}

// scrap, redundant (remote call to getIdentifier, check result)
kj::Promise<void> ChordNodeImpl::inRange(InRangeContext context){
    std::cout << "Checking in range...";
    std::cout.flush();

    auto key = context.getParams().getKey();
    auto previousNodeIdentifier = context.getParams().getPreviousNodeIdentifier();
    auto leftInclusive = context.getParams().getLeftInclusive();
    auto rightInclusive = context.getParams().getRightInclusive();

    Interval intervalRange(
        key,
        previousNodeIdentifier,
        leftInclusive,
        rightInclusive
        );

    bool inRange = intervalRange.inRange(key);
    context.getResults().setResult(inRange);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::getIdentifier(GetIdentifierContext context){
    std::cout << "Getting identifier...";
    std::cout.flush();

    context.getResults().setIdentifier(this->identifier);

    return kj::READY_NOW;
}