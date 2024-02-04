@0xf3b660d9f4459d91;

interface ChordNode{
	findSuccessor @0 (key :Text) -> (successor :ChordNode);
	findPredecessor @1 (key :Text) -> (predecessor :ChordNode);
	closestPrecedingFinger @2 (key :Text) -> (closestPrecedingNode :ChordNode);

	join @3 (node :ChordNode) -> ();
	notify @4 (node :ChordNode) -> ();

	getSuccessor @5 () -> (successor :ChordNode);
	getPredecessor @6 () -> (predecessor :ChordNode);

	inRange @7 (key :Text, previousNodeIdentifier :Text, leftInclusive :Bool, rightInclusive :Bool) -> (result :Bool);
	getIdentifier @8 () -> (identifier :Text);
}