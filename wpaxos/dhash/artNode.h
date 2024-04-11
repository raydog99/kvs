#ifndef _ARTNODE_H
#define _ARTNODE_H

enum NodeType{
	Node4,
	Node16,
	Node48,
	Node256
}

struct Node4{
	char keys[4];
	void *pointers[4];
}

struct Node16{
	char keys[16];
	void *pointers[16];
}

struct Node48{
	char index[256];
	void *pointers[48];
}

struct Node256{
	void *pointers[256];
}

struct InnerNode{
	struct NodeHeader{
		NodeType nodetype;
		int numChildren;
		std::string compressedPath;
	}
	int prefixLen;
	std::string prefix;
}

struct LeafNode{
	std::string value;
}

#endif