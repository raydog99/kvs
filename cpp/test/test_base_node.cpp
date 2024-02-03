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
	
	auto io = kj::setupAsyncIo();

	kj::Network& network = io.provider->getNetwork();
	kj::Own<kj::NetworkAddress> addr = network.parseAddress(argv[1]).wait(io.waitScope);
	kj::Own<kj::ConnectionReceiver> listener = addr->listen();

	uint port = listener->getPort();
	if (port == 0){
		std::cout << "Listening on Unix socket..." << std::endl;
	} else {
		std::cout << "Listening on port " << port << "..." << std::endl;
	}

	capnp::TwoPartyServer server(kj::heap<ChordNodeImpl>(argv[1]));

	server.listen(*listener).wait(io.waitScope);
}