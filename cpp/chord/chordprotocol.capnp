struct NodeInfo {
	id @0: Text;
	ipAddress @0: Text;
}

interface ChordNode{
	find_successor @0 (key : Text) -> (result : NodeInfo);

	find_predecessor @1 (key : Text) -> (result : NodeInfo);

	closest_preceding_finger @2 (key : Text) -> (result : NodeInfo);

	get_successor @5 () -> (result : NodeInfo);
}