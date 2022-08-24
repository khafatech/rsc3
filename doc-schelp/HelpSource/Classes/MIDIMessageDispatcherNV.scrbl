#lang scribble/manual
@(require (for-label racket))

@title{MIDIMessageDispatcherNV}
 The default dispatcher for MIDIFunc's and MIDIdef's set to respond to touch, program, and bend messages.@section{categories}
  External Control>MIDI>Dispatchers
@section{related}
  Classes/MIDIFunc, Classes/MIDIdef, Classes/AbstractWrappingDispatcher, Classes/AbstractDispatcher, Classes/MIDIMessageDispatcher, Classes/AbstractMessageMatcher, Classes/MIDIFuncSrcMessageMatcher, Classes/MIDIFuncChanMessageMatcher, Classes/MIDIFuncChanArrayMessageMatcher, Classes/MIDIFuncSrcMessageMatcherNV, Classes/MIDIFuncBothMessageMatcher, Classes/MIDIFuncBothCAMessageMatcher, Guides/MIDI

@section{description}

MIDIMessageDispatcherNV is used to dispatch incoming MIDI touch, program, and bend messages to matching functions. Normally users should not have to create or message instances of this class directly.


@section{CLASSMETHODS}
 


@section{INSTANCEMETHODS}
 

@section{METHOD}
  getKeysForFuncProxy
Get the keys at which a responder func's functions are stored in this dispatcher's active dictionary. The keys will be MIDI channels.

@section{argument}
  funcProxy
The link::Classes/MIDIFunc:: or link::Classes/MIDIdef:: whose keys should be returned.

@section{returns}
  An link::Classes/Array:: containing the funcProxy's channel number as an link::Classes/Integer::.

@section{METHOD}
  value
Attempt to match an incoming MIDI message with this dispatcher's responder funcs, and evaluate their functions for all matches found.

@section{argument}
  src
The UID of the source of the MIDI message as an link::Classes/Integer::.

@section{argument}
  chan
The channel number of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-15.

@section{argument}
  val
The message value (e.g. velocity, etc.) of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-127.

@section{METHOD}
  wrapFunc
Called internally to wrap functions in message matcher objects, if needed.

@section{argument}
  funcProxy
An instance of link::Classes/MIDIFunc:: or link::Classes/MIDIdef:: whose function(s) are to be wrapped.



