@0xb41a1c89ce69570c;

interface ChordNode{
	struct Node{
		id @0 : UInt32;
		address @1 : Text;
		uri @2 : Text;
	}

	findSuccessor @0 (request: UInt32) -> (reply: Node);
	findPredecessor @1 (request: UInt32) -> (reply: Node);
	findClosestPrecedingFinger @2 (request: UInt32) -> (reply: Node);
	join @3 (request: Node);

	getSuccessor @4 () -> (reply: Node);
}