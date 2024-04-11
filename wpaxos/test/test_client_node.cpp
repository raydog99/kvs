#include <kj/async-io.h>
#include <capnp/rpc-twoparty.h>
#include <kj/debug.h>
#include <capnp/message.h>
#include <iostream>
#include "server.h"

int main(int argc, const char* argv[]) {
	if (argc != 2) {
		std::cerr << "usage: " << argv[0] << " ADDRESS[:PORT]\n"
		"Runs the server bound to the given address/port.\n"
		"ADDRESS may be '*' to bind to all local addresses.\n"
		":PORT may be omitted to choose a port automatically." << std::endl;
	return 1;
	}

	auto clientIo = kj::setupAsyncIo();

	kj::Network& clientNetwork = clientIo.provider->getNetwork();
	kj::Own<kj::NetworkAddress> clientAddr = clientNetwork.parseAddress(argv[1]).wait(clientIo.waitScope);
	kj::Own<kj::AsyncIoStream> conn = clientAddr->connect().wait(clientIo.waitScope);

	capnp::TwoPartyClient client(*conn);

	ChordNode::Client clientNode = client.bootstrap().castAs<ChordNode>();

	{
	    std::cout << "Find successor of base node...";
	    std::cout.flush();

	    auto request = clientNode.getSuccessorRequest();
	    auto findSuccessorPromise = request.send();

	    auto response = findSuccessorPromise.wait(clientIo.waitScope);

	    std::cout << "PASS" << std::endl;

	    response.getSuccessor().getSuccessorRequest().send().wait(clientIo.waitScope);

	    std::cout << "PASS" << std::endl;
	}
}