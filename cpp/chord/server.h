#ifndef _SERVER_H
#define _SERVER_H

#include "chord.h"
#include <string>
#include <stdio.h>

class Server : ChordNode{
public:
	Server(const std::string& ip);
    virtual ~Server() override;

    ChordNode* find_successor(std::string id) override;
    ChordNode* find_predecessor(std::string id) override;
    ChordNode* closest_preceding_finger(std::string id) override;
private:
	std::string ipAddr;
};








#endif