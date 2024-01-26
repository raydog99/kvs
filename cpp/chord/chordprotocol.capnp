struct NodeInfo {
	id @0: Text;
}

interface ChordNode{
	find_successor @0 (key : Text) -> (result : NodeInfo);

	find_predecessor @1 (key : Text) -> (result : NodeInfo);

	closest_preceding_finger @2 (key : Text) -> (result : NodeInfo);

	join @3 (nodeInfo : NodeInfo) -> ();

	init_finger_table @4 () -> ();
}