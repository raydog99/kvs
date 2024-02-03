#include "helper_functions.h"

bool is_in_interval(const std::string& value, const std::string& left_bound, const std::string& right_bound, bool left_inclusive, bool right_inclusive) {
    if (left_inclusive) {
        if (right_inclusive) {
            return (value >= left_bound) && (value <= right_bound);
        } else {
            return (value >= left_bound) && (value < right_bound);
        }
    } else {
        if (right_inclusive) {
            return (value > left_bound) && (value <= right_bound);
        } else {
            return (value > left_bound) && (value < right_bound);
        }
    }
}

std::string generateRandomIPAddress() {
    std::string ip_address;
    for (int i = 0; i < 4; ++i) {
        // Each octet is a random number from 0 to 255
        ip_address += std::to_string(rand() % 256);
        if (i < 3) ip_address += ".";
    }
    return ip_address;
}
