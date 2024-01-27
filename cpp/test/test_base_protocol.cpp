#include <iostream>
#include <cstdlib>
#include <ctime>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <capnp/ez-rpc.h>
#include <kj/debug.h>
#include "chordprotocol.capnp.h"
#include "server.h"

// Function to generate a random IP address
std::string generateRandomIPAddress() {
    std::string ip_address;
    for (int i = 0; i < 4; ++i) {
    	// Each octet is a random number from 0 to 255
        ip_address += std::to_string(rand() % 256);
        if (i < 3) ip_address += ".";
    }
    return ip_address;
}

int main(){
	std::string randomIpAddress = generateRandomIPAddress();

	capnp::EzRpcServer server(kj::heap<ChordNodeImpl>(), randomIpAddress);
	auto& waitScope = server.getWaitScope();
	uint port = server.getPort().wait(waitScope);
	if (port == 0) {
	    // The address format "unix:/path/to/socket" opens a unix domain socket,
	    // in which case the port will be zero.
	    std::cout << "Listening on Unix socket..." << std::endl;
	  } else {
	    std::cout << "Listening on port " << port << "..." << std::endl;
	  }

	  // Run forever, accepting connections and handling requests.
	  kj::NEVER_DONE.wait(waitScope);
}