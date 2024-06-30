#lang scribble/manual
@(require (for-label racket))

@title{Event types}
 Different ways that an Event can "play"@section{categories}
  Streams-Patterns-Events>Events
@section{related}
  Classes/Event


An link::Classes/Event:: responds to a 
@racketblock[play:: message by evaluating ~play in the event, and the default behaviour of ~play is determined by the value of ~type.]
@section{footnote}
 To see how event types are normally invoked, here is a slightly simplified version of the default definition of  ~play as defined in the Event class:


@racketblock[{ ~eventTypes[~type].value(server); }::

The function uses the value of ~type to select a function from the Dictionary held in ~eventTypes. ::


]

@racketblock[
a = (play: { ~word.scramble.postln }, word: "hello word");
a.play;

a = (type: \note, freq: [1310, 1321]); // choosing a play function by specifying type
a.play;
::


The collection of eventTypes can be readily extended using link::Classes/Event#*addEventType#*addEventType:::

]

@racketblock[
Event.addEventType(\test, { "Your word is: ".post; ~word.scramble.postln });
(type: \test, word: "annahme").play;
::


]
@section{section}
 Currently existing event types:
@section{Note}
  this documentation is incomplete. ::

@section{definitionlist}
 

## note || Instantiate a synth on the server, with specified arguments, and later to free it. The choice of link::Classes/SynthDef:: is specified using the \instrument key. This event type is what 
@racketblock[Event.default:: returns.

]

@racketblock[(degree: [0, 5, 7, 11]).play;::

Actually plays this event type:

]

@racketblock[(type: \note, degree: [0, 5, 7, 11], instrument: \default).play;::

## set || used to set parameters of some already-running node(s).

]

@racketblock[
a = (degree: 3, sustain: 40).play;
fork { 10.do { (type: \set, id: a[\id], \degree: [0, 5, 8, 11].choose).play; 0.3.wait } };
::

(See also: note in link::Classes/Pmono:: helpfile)


## group || creates a new group
optional parameters:
]
@section{table}
 
## ~id || node ID, or node object
## ~group || outer group id or object
## ~addAction / ~lag / ~timingOffset || determine how and when the group is created
::

Example:

@racketblock[
(type: \group, id: 2).play					// create a group with nodeID 2
(type: \note, freq: 500, group: 2).play		// play a synth in that group
::


## midi || send note parameters to midi device
optional parameters:
]
@section{table}
 
## ~id || node ID, or node object
## ~group || outer group id or object
## ~addAction / ~lag / ~timingOffset || determine how and when the group is created
::

## on || play synth, ~id must be specified
## off || release synth (or free if no gate)
## kill || free synth

## rest || do nothing for a specified amount of time

## bus || write ~array to control buses starting at ~out
## audioBus || allocate ~channels consecutive audio buses
## controlBus || allocate ~channels consecutive control buses

## alloc || allocate ~bufnum with ~numframes and ~numchannels
## allocRead || load a file from ~path, starting at ~firstFileFrame, reading ~numFrames sample frames
## cue || cue a file for DiskIn, with ~bufferSize frames

## free || free ~bufnum
## gen || send ~gencmd to ~bufnum
## load || load ~filename starting at ~frame into ~bufnum
## read ||

## table || load ~amps directly into a buffer
## sine1 || generate a buffer from ~amps
## sine2 || generate a buffer from ~freqs, ~amps
## sine3 || generate a buffer from ~freqs, ~amps, ~phases
## cheby || generate a waveshape buffer from ~amps

## setProperties || sends setter messages to ~receiver for each key in ~args that has a nonNil value in the Event.

## tree || creates a tree of groups. ~tree can be an array of nodeIDs, and may contain associations to further nested arrays.

## phrase || instead of playing a single synth from a SynthDef with ~instrument, it looks up a Pdef and plays a cluster of sounds.


::

@section{subsection}
 Some event types are used internally, e.g.:
@section{definitionlist}
 
## monoNote || used by Pmono
## monoSet || used by Pmono
## monoOff || used by Pmono
::

