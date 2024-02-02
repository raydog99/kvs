@0xf3b660d9f4459d91;

struct NodeInfo {
	id @0: Text;
	ipAddress @1: Text;
}

interface ChordNode{
	findSuccessor @0 (key : Text) -> (result : NodeInfo);

	findPredecessor @1 (key : Text) -> (result : NodeInfo);

	closestPrecedingFinger @2 (key : Text) -> (result : NodeInfo);

	updateFingerTable @3 (node : NodeInfo, fingerIndex : Int64) -> (result: NodeInfo);

	getSuccessor @4 () -> (result : NodeInfo);
}