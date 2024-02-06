#ifndef _ARTNODE_H
#define _ARTNODE_H

struct artNode{
	int prefixLen;
	NodeType node; 
}

enum NodeType{
	Node4,
	Node16,
	Node48,
	Node256
}

struct Node4{
	int numChildren;
	char keys[4];
	void *pointers[4];
}

struct Node16{
	int numChildren;
	char keys[16];
	void *pointers[16];
}

struct Node48{
	int numChildren;
	char index[256];
	void *pointers[48];
}

struct Node256{
	int numChildren;
	void *pointers[256];
}

#endif