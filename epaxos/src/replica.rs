use std::collections::{HashMap, VecDeque};

const HT_INIT_SIZE: usize = 200000;
const BF_K: usize = 4;

static mut BF_PT: u32 = 0;

#[derive(Debug)]
struct Replica {
    node: Node,
    n: usize,
    instance_space: HashMap<ID, Vec<Instance>>,
    crt_instance: HashMap<ID, usize>,
    committed_up_to: HashMap<ID, usize>,
    executed_up_to: HashMap<ID, usize>,
    conflicts: HashMap<ID, HashMap<Key, usize>>,
    max_seq_per_key: HashMap<Key, usize>,
    max_seq: usize,
    latest_cp_replica: ID,
    latest_cp_instance: usize,
    shutdown: bool,
    instances_to_recover: VecDeque<InstanceId>,
}

#[derive(Debug)]
struct Instance {
    cmds: Vec<Command>,
    ballot: i32,
    status: i8,
    seq: usize,
    deps: HashMap<ID, usize>,
    lb: LeaderBookkeeping,
    index: usize,
    lowlink: usize,
    bfilter: Bloomfilter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ID {
    id: usize,
}

#[derive(Debug)]
struct Command {
}

#[derive(Debug)]
struct Bloomfilter {
}

#[derive(Debug)]
struct InstanceId {
    replica: ID,
    instance: usize,
}

#[derive(Debug)]
struct RecoveryInstance {
    cmds: Vec<Command>,
    status: i8,
    seq: usize,
    deps: HashMap<ID, usize>,
    pre_accept_count: usize,
    leader_responded: bool,
}

#[derive(Debug)]
struct LeaderBookkeeping {
    proposals: Vec<Request>,
    max_recv_ballot: i32,
    prepare_quorum: Quorum,
    all_equal: bool,
    pre_accept_quorum: Quorum,
    accept_quorum: Quorum,
    nacks: usize,
    original_deps: HashMap<ID, usize>,
    committed_deps: HashMap<ID, usize>,
    recovery_inst: RecoveryInstance,
    preparing: bool,
    trying_to_pre_accept: bool,
    possible_quorum: Vec<bool>,
    tpa_oks: usize,             
}

#[derive(Debug)]
struct Quorum {
}

#[derive(Debug)]
struct Node {
}