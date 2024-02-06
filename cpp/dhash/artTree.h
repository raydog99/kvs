#ifndef _ARTTREE_H
#define _ARTTREE_H

#include "artNode.h"
#include <string>

class Art{
	Art();

	ArtNode search(ArtNode node, std::string key, int depth);
	ArtNode findChild(ArtNode node, std::string byte);
	void insert(ArtNode node, std::string key, ArtNode leaf, int depth);
}

#endif