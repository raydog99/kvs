#include "helperFunctions.h"

std::string generateRandomIPAddress() {
    std::string ipAddress;
    for (int i = 0; i < 4; ++i) {
        // Each octet is a random number from 0 to 255
        ipAddress += std::to_string(rand() % 256);
        if (i < 3) ipAddress += ".";
    }
    return ipAddress;
}