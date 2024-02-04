#ifndef _HELPER_FUNCTIONS_H
#define _HELPER_FUNCTIONS_H

#include <string>
#include <stdio.h>
#include <map>

enum class IpVersions{
    Ipv4,
    Ipv6
};

// std::map<IpVersions, int> fingerTableSize = {
//     {IpVersions::Ipv4, 32},
//     {IpVersions::Ipv6, 144}
// };

struct Interval{
	std::string leftBound;
	std::string rightBound;
	bool leftInclusive;
	bool rightInclusive;

	Interval(
		const std::string& leftBound,
		const std::string& rightBound,
		bool leftInclusive,
		bool rightInclusive
		) : 
		leftBound(leftBound),
		rightBound(rightBound),
		leftInclusive(leftInclusive),
		rightInclusive(rightInclusive) {}

	bool inRange(const std::string& key) const {
		bool leftCheck = (leftInclusive) ? (key >= leftBound) : (key > leftBound);
        bool rightCheck = (rightInclusive) ? (key <= rightBound) : (key < rightBound);

        return leftCheck && rightCheck;
	}
};

std::string generateRandomIPAddress();

#endif