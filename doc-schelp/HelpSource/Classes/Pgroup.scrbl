#lang scribble/manual
@(require (for-label racket))

@title{Pgroup}
 Starts a new Group and plays the pattern in this group@section{related}
  Classes/Group, Classes/Pbus
@section{categories}
  Streams-Patterns-Events>Patterns>Server Control

@section{description}


The group is released when the stream has ended. The group's strong::release:: is delayed (default 0.1 beats) until after the last note releases. But, Pgroup does not know how long the synths' envelopes last. You can extend the lag by putting the number of beats into the event prototype's 
@racketblock[\groupReleaseTime:: key:
]

@racketblock[
Pgroup(...).play(protoEvent: Event.default.put(\groupReleaseTime, releaseLag));
::

]
@section{Examples}
 


@racketblock[

p = Pbind(\degree, Prand((0..7), inf), \dur, 0.3, \legato, 0.2);

 // watch the node structure as it changes
s.waitForBoot({ s.plotTree });

// one group
Pgroup(p).play;

// two nested groups
Pgroup(Pgroup(p)).play;
Pgroup(Ppar([Pgroup(p), Pgroup(p)])).play;




::
]


