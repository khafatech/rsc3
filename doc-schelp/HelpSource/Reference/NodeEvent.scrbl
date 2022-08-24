#lang scribble/manual
@(require (for-label racket))

@title{NodeEvent}
@section{categories}
  Streams-Patterns-Events
@section{related}
  Classes/Event
 synth- and group- like interface of Event
@section{description}

The methods link::Classes/Event#-synth:: and link::Classes/Event#-group:: set the parent event of the receiver to specialized events that duplicate the functionality of link::Classes/Synth:: and link::Classes/Group:: objects. These objects follow the naming conventions of patterns (i.e., groups and addActions are integer ID's) and have the same stop/play/pause/resume interface as the EventStreamPlayers created by Pattern-play. So, they can be used interchangeably with patterns in control schemes and GUI's.

The following example creates a group with nodeID = 2 and plays a synth within it.


@racketblock[
g = (id: 2).group;
g.play;
a = (group: 2).synth;
a.play;
g.release;
g.stop;
::

]
@section{subsection}
 Interface

@section{method}
  play
starts synth or group, returns this.delta
@section{method}
  stop
if ev[\hasGate] == true set gate to 0, otherwise frees the node
@section{method}
  pause
disables the node
@section{method}
  resume
reenables the node
@section{method}
  set ( key, value)
sets control identified by key to value
@section{method}
  split
returns an array of events, one for each synth or group specified by the receiver
@section{method}
  map (key, busID)
maps control to control bus
@section{method}
  before (nodeID)
moves to immediately before nodeID
@section{method}
  after (nodeID)
moves to immediately after nodeID
@section{method}
  headOf (nodeID)
moves to immediately to head of nodeID
@section{method}
  tailOf (nodeID)
moves to immediately to tail of nodeID

@section{subsection}
  Multi-channel expansion
With the exception of ~server, ~latency, and ~instrument any key in the event can have an array as a
value and the standard rules of multi-channel expansion will be followed.

@section{Examples}
 


@racketblock[
// Here is a simple example of its use:

// define a multiple Group event
g = (id: [2,3,4,5,6], group: 0, addAction: 1).group;
g.play; // play it

// make a Synth event
b = ( freq: [500,510], group: [2,3]).synth;
b.play;

b.set(\freq,[1000,1006]);

g.release;

b.play;
h = g.split;	// split into individual group events
c = b.split;	// and synth events
c[0].set(\freq,700);
c[1].set(\freq,400);

h[0].stop;
h[1].stop;

g.stop;
::
]


