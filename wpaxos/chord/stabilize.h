#ifndef _STABILIZE_H
#define _STABILIZE_H

class ChordNodeImpl;

void stabilize(ChordNodeImpl& chordNode);
void fixFingers(ChordNodeImpl& chordNode);
void notify(ChordNodeImpl& chordNode, ChordNode::NotifyContext context);

#endif