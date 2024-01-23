#include <iostream>
#include <cstdlib>
#include <ctime>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

// Function to generate a random IP address
std::string generateRandomIPAddress() {
    std::string ip_address;
    for (int i = 0; i < 4; ++i) {
        ip_address += std::to_string(rand() % 256);  // Each octet is a random number from 0 to 255
        if (i < 3) ip_address += ".";
    }
    return ip_address;
}

int main(){
	int numNodes = 1;

	for (int node = 0; node < numNodes; node++){
		std::string nodeIpAddr = generateRandomIPAddress();
	}
}