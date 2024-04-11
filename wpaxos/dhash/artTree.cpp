#include "artTree.h"

ArtNode Art::search(ArtNode node, std::string key, int depth){
	if (node == nullptr){
		return nullptr;
	}

	if isLeaf(node){
		if (leafMatches(node, key, depth)){
			return node;
		}
		return nullptr;
	}

	if (checkPrefix(node, key, depth) != node.prefixLen){
		return nullptr;
	}

	depth += node.prefixLen;
	next = findChild(node, key[depth]);
	return search(next, key, depth + 1);
}

ArtNode Art::findChild(ArtNode node, std::string byte){
	// dispatch on type instead of comparisons
	if (node.type == Node4){
		for (int i = 0; i < node.count; i++){
			if (node.key[i] == byte){
				return node.child[i];
			}
		}
		return nullptr;
	}
	if (node.type == Node16){		// SSE comparison
		key=_mm_set1_epi8(byte);
		cmp = mm_cmpeq_epi8(key, node.key);
		mask = (1 << node.count) - 1;
		bitfield = _mm_movemask_epi8(cmp)&mask;
		if (bitfield){
			return node.child[ctz(bitfield)];
		}
		return nullptr;
	}
	if (node.type == Node48){
		if (node.childIndex[byte].length > 0){
			return node.child[node.childIndex[byte]]
		}
		else{
			return nullptr;
		}
	}
	if (node.type == Node256){
		return node.child[byte];
	}
}

void Art::insert(ArtNode node, std::string key, ArtNode leaf, int depth){
	if (node == nullptr){
		node = leaf;
		return;
	}

	// expand node
	if isLeaf(node){
		newNode = makeNode4();
		key2 = loadKey(node);
		int i = depth;
		for (i; key[i] == key2[i]; i++){
			newNode.prefix[i - depth] = key[i];
		}
		newNode.prefixLen = i - depth;
		depth += newNode.prefixLen;
		addChild(newNode, key[depth], leaf);
		addChild(newNode, key2[depth], node);
		replace(node, newNode);
		return;
	}

	p = checkPrefix(node, key, depth);
	if (p != node.prefixLen){
		newNode = makeNode4();
		addChild(newNode, key[depth + p], leaf);
		addChild(newNode, node.prefix[p], node);
		addNode.prefixLen = p;
		memcpy(newNode.prefix, node.prefix, p);
		memmove(node.prefix, node.prefix + p + 1, node.prefixLen);
		replace(node, newNode);
		return;
	}

	depth += node.prefixLen;
	next = findChild(node, key[depth]);
	// recurse
	if (next){		
		insert(next, key, leaf, depth + 1);
		return;
	}

	// continue to next node
	if (isFull(node)){
		grow(node);
	}
	addChild(node, key[depth], leaf);
}