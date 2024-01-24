#ifndef _SERVER_H
#define _SERVER_H

#include "chord.h"
#include <string>
#include <stdio.h>

class Server : ChordNode{
public:
	Server(const std::string& ip);
    virtual ~Server();

    ChordNode* find_successor(std::string id) override;
    ChordNode* find_predecessor(std::string id) override;
    ChordNode* closest_preceding_finger(std::string id) override;
private:
	// 48 bits for IPv4 (32 bits) + port (16 bits)
	// 144 bits for IPv6 (128 bits) + port (16 bits)
	std::string ipAddr;
};

#endif