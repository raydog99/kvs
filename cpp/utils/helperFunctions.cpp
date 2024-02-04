#include "helperFunctions.h"

bool isInInterval(const std::string& value, const std::string& leftBound, const std::string& rightBound, bool leftInclusive, bool rightInclusive) {
    if (leftInclusive) {
        if (rightInclusive) {
            return (value >= leftBound) && (value <= rightBound);
        } else {
            return (value >= leftBound) && (value < rightBound);
        }
    } else {
        if (rightInclusive) {
            return (value > leftBound) && (value <= rightBound);
        } else {
            return (value > leftBound) && (value < rightBound);
        }
    }
}

std::string generateRandomIPAddress() {
    std::string ipAddress;
    for (int i = 0; i < 4; ++i) {
        // Each octet is a random number from 0 to 255
        ipAddress += std::to_string(rand() % 256);
        if (i < 3) ipAddress += ".";
    }
    return ipAddress;
}