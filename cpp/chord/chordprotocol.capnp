@0xf3b660d9f4459d91;

interface ChordNode{
	findSuccessor @0 (key :Text) -> (result :ChordNode);
	findPredecessor @1 (key :Text) -> (result :ChordNode);
	closestPrecedingFinger @2 (key :Text) -> (result :ChordNode);

	join @3 (node :ChordNode) -> ();
	notify @4 (node :ChordNode) -> ();

	getSuccessor @5 () -> (result :ChordNode);
	getPredecessor @6 () -> (result :ChordNode);
	inRange @7 (key :Text, previousNodeIdentifier :Text, leftInclusive :Bool, rightInclusive :Bool) -> (result :Bool);
	getIdentifier @8 () -> (result :Text);
}