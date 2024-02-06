#include "stabilize.h"

void ChordNodeImpl::stabilize() {
    std::cout << "Stabilizing...";
    std::cout.flush();

    auto io = kj::setupAsyncIo();

    auto successorRef = this->fingerTable->getSuccessor().nodeRef;
    auto newSuccessor, newSuccessorIdentifier = successorRef
        .getPredecessorRequest()
        .send()
        .wait(io.waitScope);

    Interval newSuccessorRange = Interval(
        this->identifier,
        newSuccessorIdentifier,
        false,
        false
        );

    bool foundNewSuccessor = newSuccessorRange.inRange(newSuccessorIdentifier);
    if (foundNewSuccessor == true){
        this->fingerTable.setSuccessor(successor);
    }

    auto notifyPromise = successor.notifyRequest();
    notifyPromise.setNode(this->thisCap());
    notifyPromise.send().wait(io.waitScope);
}

kj::Promise<void> ChordNodeImpl::notify(NotifyContext context){
    std::cout << "Notify called...";
    std::cout.flush();

    auto node, nodeIdentifier = context.getParams().getNode();

    auto io = kj::setupAsyncIo();

    std::string predecessorIdentifier = this->predecessor
        .getIdentifierRequest()
        .send()
        .wait(io.waitScope)
        .getIdentifier();

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
    auto selectedNode = this->fingerTable->nodes[randomIndex];
    auto selectedNodeRef = selectedNode.nodeRef;

    auto indexSuccessorRequest = selectedNodeRef.findSuccessorRequest();
    indexSuccessorRequest.setKey(this->fingerTable->getStart(randomIndex));
    auto indexSuccessor = indexSuccessorRequest.send().wait(io.waitScope).getSuccessor();

    this->fingerTable->nodes[randomIndex] = indexSuccessor;
}