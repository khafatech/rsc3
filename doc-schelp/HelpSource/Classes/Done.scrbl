#lang scribble/manual
@(require (for-label racket))

@title{Done}
 Monitors another UGen to see when it is finished@section{related}
   Classes/UGen, Classes/FreeSelfWhenDone, Classes/PauseSelfWhenDone
@section{categories}
   UGens>Synth control, Server>Nodes

@section{description}


Some UGens set a 'done' flag when they are finished playing. This UGen echoes that flag when it is set to track a particular UGen.

The UGens trackable by Done are:
@section{list}
 
## link::Classes/PlayBuf::
## link::Classes/RecordBuf::
## link::Classes/Line::
## link::Classes/XLine::
## link::Classes/EnvGen::
## link::Classes/Linen::
## link::Classes/BufRd::
## link::Classes/BufWr::
## link::Classes/Dbufrd::
## link::Classes/Dbufwr::
## link::Classes/DiskIn::
## link::Classes/VDiskIn::
## link::Classes/Demand::
::

@section{section}
  Actions
A number of UGens implement doneActions. These allow one to optionally free or pause the enclosing synth and other related nodes when the UGen is finished. You can use the constants in this class to name doneActions, which can be clearer than using the number codes alone. The available doneActions are as follows:

@section{table}
 
## name     || value || description
## none || 0 || do nothing when the UGen is finished
## pauseSelf || 1 || pause the enclosing synth, but do not free it
## freeSelf || 2 || free the enclosing synth
## freeSelfAndPrev || 3 || free both this synth and the preceding node
## freeSelfAndNext || 4 || free both this synth and the following node
## freeSelfAndFreeAllInPrev || 5 || free this synth; if the preceding node is a group then do g_freeAll on it, else free it
## freeSelfAndFreeAllInNext || 6 || free this synth; if the following node is a group then do g_freeAll on it, else free it
## freeSelfToHead || 7 || free this synth and all preceding nodes in this group
## freeSelfToTail || 8 || free this synth and all following nodes in this group
## freeSelfPausePrev || 9 || free this synth and pause the preceding node
## freeSelfPauseNext || 10 || free this synth and pause the following node
## freeSelfAndDeepFreePrev || 11 || free this synth and if the preceding node is a group then do g_deepFree on it, else free it
## freeSelfAndDeepFreeNext || 12 || free this synth and if the following node is a group then do g_deepFree on it, else free it
## freeAllInGroup || 13 || free this synth and all other nodes in this group (before and after)
## freeGroup || 14 || free the enclosing group and all nodes within it (including this synth)
::

For information on 
@racketblock[freeAll:: and ]

@racketblock[deepFree::, see link::Classes/Group:: and link::Reference/Server-Command-Reference::.

]
@section{section}
  Alternatives
Another way to free a synth when some UGen is done playing is to use link::Classes/FreeSelfWhenDone::, or link::Classes/FreeSelf:: in combination with link::Classes/Done::. For example, this can be used to delay the freeing to let reverb tails fade out, etc.

@section{classmethods}
 
@section{private}
  categories

@section{method}
 kr

@section{argument}
 src

The UGen to monitor

@section{examples}
 
The 'done' flag can be used to trigger other things in the same synth:

@racketblock[
(
SynthDef("Done-help", { arg out, t_trig;
	var line, a, b;

	line= Line.kr(1,0,1);

	a= SinOsc.ar(440,0,0.1*line); //sound fading out
	b= WhiteNoise.ar(Done.kr(line)*0.1); //noise starts at end of line

	Out.ar(out, Pan2.ar(a+b));
}).add;
)

Synth("Done-help"); //note that this synth doesn't have it's own doneAction, so you'll need to manually deallocate it
::
The 'done' flag can be used to trigger a delayed freeing of the current synth, which is not possible by using doneActions alone:
]

@racketblock[
play {
    var env = Line.kr(1,0,2);
    var sig = PinkNoise.ar(env);
    FreeSelf.kr(TDelay.kr(Done.kr(env),3));
    GVerb.ar(sig,70,7);
}
::
]

