#include "server.h"

ChordNodeImpl::ChordNodeImpl(std::string ipAddress) : fingerTable(), predecessor(this->thisCap()) {
    this->ipAddress = ipAddress;
    this->identifier = ipAddress; // (!)

    stabilize();    // run every 30s
}

kj::Promise<void> ChordNodeImpl::join(JoinContext context){
    auto node = context.getParams().getNode();

    std::cout << "Joining...";
    std::cout.flush();

    this->predecessor = nullptr;

    auto io = kj::setupAsyncIo();

    auto successorRequest = node.findSuccessorRequest();
    successorRequest.setKey(this->identifier);
    auto successor = successorRequest.send().wait(io.waitScope).getSuccessor();

    this->fingerTable.setSuccessor(successor);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::findSuccessor(FindSuccessorContext context) {
    std::cout << "Finding successor...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    auto io = kj::setupAsyncIo();

    auto nodeRef = this->thisCap();
    auto predecessorRequest = nodeRef.findPredecessorRequest();
    predecessorRequest.setKey(key);

    auto successor = predecessorRequest
        .send()
        .wait(io.waitScope)
        .getPredecessor()
        .getSuccessorRequest()
        .send()
        .wait(io.waitScope)
        .getSuccessor();

    context.getResults().setSuccessor(successor);

    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::findPredecessor(FindPredecessorContext context) {
    std::cout << "Finding predecessor...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    auto io = kj::setupAsyncIo();

    auto candidate = this->thisCap();
    auto candidateIdentifier = this->identifier;

    while(true){
        std::string candidateSuccessorIdentifier = candidate
            .getSuccessorRequest()
            .send()
            .wait(io.waitScope)
            .getSuccessor();

        Interval predecessorRange = Interval(
            candidateIdentifier,
            candidateSuccessorIdentifier,
            false,
            true
            );
        bool predecessorFound = predecessorRange.inRange(key);
        if (predecessorFound){
            break;
        }

        auto nextCandidatePromise = candidate.closestPrecedingFingerRequest();
        candidate.setKey(key);
        auto nextCandidateResults = nextCandidatePromise.send().wait(io.waitScope);

        candidate = nextCandidateResults.getClosestPrecedingNodeRef();
        candidateIdentifier = nextCandidateResults.getClosestPrecedingNodeIdentifier();
    }

    context.getResults().setPredecessor(candidate);
    return kj::READY_NOW;
}

kj::Promise<void> ChordNodeImpl::closestPrecedingFinger(ClosestPrecedingFingerContext context) {
    std::cout << "Finding closest preceding finger...";
    std::cout.flush();

    auto key = context.getParams().getKey();

    auto io = kj::setupAsyncIo();
    int fingerTableSize = this->fingerTable->fingerTableSize;

    for (int i = fingerTableSize - 1; i > 0; i--){
        auto nodeIdentifier = this->fingerTable->nodes[i].identifier;

        Interval closestPrecedingRange = Interval(
            this->identifier,
            key,
            false,
            false
            );

        bool isClosestPrecedingFinger = closestPrecedingRange.inRange(nodeIdentifier);
        if(isClosestPrecedingFinger){
            context.getResults().setClosestPrecedingNode(this->fingerTable->nodes[i]);
            return kj::READY_NOW;
        }
    }

    context.getResults().setClosestPrecedingNode(this->thisCap());
    return kj::READY_NOW;
}