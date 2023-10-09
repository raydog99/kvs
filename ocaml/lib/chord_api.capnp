@0xb41a1c89ce69570c;

interface ChordNode{
	struct Node{
		id @0 : UInt32;
		address @1 : Text;
		uri @2 : Text;
	}

	findSuccessor @0 (request: UInt32) -> Node;
	findPredecessor @1 (request: UInt32) -> Node;
	findClosestPrecedingFinger @2 (request: UInt32) -> Node;
	join @3 (request: Node);
}