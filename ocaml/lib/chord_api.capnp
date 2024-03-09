@0xb41a1c89ce69570c;

interface ChordNode{
	findSuccessor @0 (key :Text) -> (successorRef :ChordNode, successorIdentifier :Text);
	findPredecessor @1 (key :Text) -> (predecessorRef :ChordNode, predecessorIdentifier :Text);
	closestPrecedingFinger @2 (key :Text) -> (closestPrecedingNodeRef :ChordNode, closestPrecedingNodeIdentifier :Text);

	join @3 (nodeRef :ChordNode, nodeIdentifier :Text) -> ();
	notify @4 (nodeRef :ChordNode, nodeIdentifier :Text) -> ();
}