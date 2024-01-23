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


        /*      CapnProto boilerplate       */

        // Create a socket for communication
        int serverSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (serverSocket == -1) {
            std::cerr << "Error creating socket." << std::endl;
            return 1;
        }

        // Define the server address
        struct sockaddr_in serverAddress;
        serverAddress.sin_family = AF_INET;
        serverAddress.sin_port = htons(PORT);
        serverAddress.sin_addr.s_addr = INADDR_ANY;

        // Bind the socket to the server address
        if (bind(serverSocket, (struct sockaddr*)&serverAddress, sizeof(serverAddress)) == -1) {
            std::cerr << "Error binding socket." << std::endl;
            close(serverSocket);
            return 1;
        }

        // Listen for incoming connections
        if (listen(serverSocket, 1) == -1) {
            std::cerr << "Error listening for connections." << std::endl;
            close(serverSocket);
            return 1;
        }

        // Forking
        pid_t pid = fork();
        if (pid == -1){
            std::cerr << "Error forking process" << std::endl;
            return 1;
        }
        else if (pid == 0){
            // Child process

             // Accept the incoming connection from the parent
            int clientSocket = accept(serverSocket, nullptr, nullptr);
            if (clientSocket == -1) {
                std::cerr << "Error accepting connection in child process." << std::endl;
                close(serverSocket);
                return 1;
            }

            // Read the RPC message from the parent
            char buffer[256];
            memset(buffer, 0, sizeof(buffer));
            if (read(clientSocket, buffer, sizeof(buffer)) == -1) {
                std::cerr << "Error reading from socket in child process." << std::endl;
                close(clientSocket);
                close(serverSocket);
                return 1;
            }

            std::cout << "Received RPC message in child process: " << buffer << std::endl;

            // Close the sockets
            close(clientSocket);
            close(serverSocket);
        }
        else {
            // Parent process

            // Create a socket for communication with the child
            int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
            if (clientSocket == -1) {
                std::cerr << "Error creating socket in parent process." << std::endl;
                close(serverSocket);
                return 1;
            }

            // Define the client address
            struct sockaddr_in clientAddress;
            clientAddress.sin_family = AF_INET;
            clientAddress.sin_port = htons(PORT);
            inet_pton(AF_INET, random_ip.c_str(), &clientAddress.sin_addr);

            // Connect to the child process
            if (connect(clientSocket, (struct sockaddr*)&clientAddress, sizeof(clientAddress)) == -1) {
                std::cerr << "Error connecting to child process." << std::endl;
                close(clientSocket);
                close(serverSocket);
                return 1;
            }

            // Send an RPC message to the child
            const char* rpcMessage = "BORK";
            if (write(clientSocket, rpcMessage, strlen(rpcMessage)) == -1) {
                std::cerr << "Error writing to socket in parent process." << std::endl;
                close(clientSocket);
                close(serverSocket);
                return 1;
            }

            // Close the sockets
            close(clientSocket);
            close(serverSocket);

            // Wait for the child process to finish
            int status;
            waitpid(pid, &status, 0);
            std::cout << "Child process finished with status: " << status << std::endl;
        }

	}
}