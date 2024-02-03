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

bool is_in_interval(
	const std::string& value, 
	const std::string& left_bound, 
	const std::string& right_bound, 
	bool left_inclusive, 
	bool right_inclusive
	);

std::string generateRandomIPAddress();

#endif