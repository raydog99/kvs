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
