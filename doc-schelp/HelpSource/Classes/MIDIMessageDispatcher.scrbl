#lang scribble/manual
@(require (for-label racket))

@title{MIDIMessageDispatcher}
 The default dispatcher for MIDIFunc's and MIDIdef's set to respond to noteOn, noteOff, control, and polytouch messages.@section{categories}
  External Control>MIDI>Dispatchers
@section{related}
  Classes/MIDIFunc, Classes/MIDIdef, Classes/AbstractWrappingDispatcher, Classes/AbstractDispatcher, Classes/MIDIMessageDispatcherNV, Classes/AbstractMessageMatcher, Classes/MIDIFuncSrcMessageMatcher, Classes/MIDIFuncChanMessageMatcher, Classes/MIDIFuncChanArrayMessageMatcher, Classes/MIDIFuncSrcMessageMatcherNV, Classes/MIDIFuncBothMessageMatcher, Classes/MIDIFuncBothCAMessageMatcher, Guides/MIDI

@section{description}

MIDIMessageDispatcher is used to dispatch incoming MIDI noteOn, noteOff, control, and polytouch messages to matching functions. Normally users should not have to create or message instances of this class directly.


@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a new instance.

@section{argument}
  messageType
A link::Classes/Symbol:: indicating the message type, one of 
@racketblock[\noteOn::, ]

@racketblock[\noteOff::, ]

@racketblock[\control::, or ]

@racketblock[\polytouch::.

]
@section{returns}
  A new MIDIMessageDispatcher.


@section{INSTANCEMETHODS}
 

@section{METHOD}
  messageType
Get this dispatcher's message type, one of 
@racketblock[\noteOn::, ]

@racketblock[\noteOff::, ]

@racketblock[\control::, or ]

@racketblock[\polytouch::.

]
@section{returns}
  A link::Classes/Symbol::.

@section{METHOD}
  getKeysForFuncProxy
Get the keys at which a responder func's functions are stored in this dispatcher's active dictionary. The keys will be MIDI message numbers.

@section{argument}
  funcProxy
The link::Classes/MIDIFunc:: or link::Classes/MIDIdef:: whose keys should be returned.

@section{returns}
  An link::Classes/Array:: containing the funcProxy's message number as an link::Classes/Integer::.

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
  num
The message number (e.g. note number, etc.) of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-127.

@section{argument}
  val
The message value (e.g. velocity, etc.) of the MIDI message as an link::Classes/Integer::. Note this should be in the range 0-127.

@section{METHOD}
  register
Adds this dispatcher to the appropriate receive hook in link::Classes/MIDIIn::.

@section{METHOD}
  unregister
Removes this dispatcher from the appropriate receive hook in link::Classes/MIDIIn::.

@section{METHOD}
  wrapFunc
Called internally to wrap functions in message matcher objects, if needed.

@section{argument}
  funcProxy
An instance of link::Classes/MIDIFunc:: or link::Classes/MIDIdef:: whose function(s) are to be wrapped.

@section{METHOD}
  typeKey
Gets a key indicating the type of message this dispatcher responds to, in the form: 
@racketblock[('MIDI ' ++ messageType).asSymbol::.

]
@section{returns}
  A link::Classes/Symbol::.



