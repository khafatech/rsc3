#lang scribble/manual
@(require (for-label racket))

@title{MIDIFunc}
 Fast Responder for incoming MIDI Messages@section{categories}
  External Control>MIDI
@section{related}
  Guides/MIDI, Classes/MIDIdef

@section{description}

MIDIFunc (and its subclass link::Classes/MIDIdef::) registers one or more functions to respond to an incoming MIDI message. Many of its methods are inherited from its superclass link::Classes/AbstractResponderFunc::.

@section{note}
  MIDIFunc and link::Classes/MIDIdef:: aim to improve upon the MIDIresponder classes by being faster, easier to use, and providing support for all MIDI message types. They were made with the intention of creating a more convenient, logical and consistent interface, which shares a common design with link::Classes/OSCFunc:: and link::Classes/OSCdef::. Note that unlike the older classes, MIDIFuncs are removed on Cmd-. by default. This can be overridden using either of the fix or permanent methods.::


@section{CLASSMETHODS}
 
@section{private}
  initClass, cmdPeriod

@section{METHOD}
  defaultDispatchers
Get or set an link::Classes/IdentityDictionary:: containing the default dispatcher objects for MIDIFuncs of different types (these are what you get if you pass nil as the dispatcher argument to link::#*new::). These objects will decide if any of their registered MIDIFuncs should respond to an incoming MIDI message. The dictionary should have the keys 
@racketblock[[\noteOn, \noteOff, \control, \polytouch, \touch, \program, \bend]:: and values of an appropriate subclass of link::Classes/AbstractDispatcher:: for each message type. By default these will be instances of link::Classes/MIDIMessageDispatcher:: for the the first four message types, and instances of link::Classes/MIDIMessageDispatcherNV:: for the last three.

]
@section{returns}
  The getter returns an link::Classes/IdentityDictionary::.

@section{METHOD}
  new
Create a new, enabled MIDIFunc. Normally one would use one of the message type specific convenience methods below, rather than use this method directly.

@section{argument}
  func
A link::Classes/Function:: or similar object which will respond to the incoming message. When evaluated for noteOn, noteOff, control, and polytouch messages it will be passed the arguments val, num, chan, and src, corresponding to the message value (e.g. velocity, control value, etc.), message number (e.g. note number), MIDI channel, and MIDI source uid. For touch, program change and bend messages it will be passed only val, chan, and src. For information on the args passed for the other sorts of 
@racketblock[msgType:: see the convenience methods below.

]
@section{argument}
  msgNum
An link::Classes/Integer:: indicating the MIDI message number (note number, control number, or program number) for this MIDIFunc. This can be an array. If nil, the MIDIFunc will respond to messages of all possible message numbers.

@section{argument}
  chan
An link::Classes/Integer:: indicating the MIDI channel number for this MIDIFunc. This can be an array. If nil, the MIDIFunc will respond to messages received on all channels.

@section{argument}
  msgType
A link::Classes/Symbol:: indicating which kind of MIDI message this MIDIFunc should respond to. One of 
@racketblock[\noteOn::, ]

@racketblock[\noteOff::, ]

@racketblock[\control::, ]

@racketblock[\touch::, ]

@racketblock[\polytouch::, ]

@racketblock[\bend::, ]

@racketblock[\program::, ]

@racketblock[\sysex::, ]

@racketblock[\mtcQF::, ]

@racketblock[\smpte::, ]

@racketblock[\songPosition::, ]

@racketblock[\songSelect::, ]

@racketblock[\tuneRequest::, ]

@racketblock[\midiClock::, ]

@racketblock[\sysrt::, ]

@racketblock[\tick::, ]

@racketblock[\start::, ]

@racketblock[\continue::, ]

@racketblock[\stop::, ]

@racketblock[\activeSense::, or ]

@racketblock[\reset::.

]
@section{argument}
  srcID
An link::Classes/Integer:: corresponding to the uid of the MIDI input. (See link::Classes/MIDIClient::.) If nil, the MIDIFunc will respond to messages received from all sources.

@section{argument}
  argTemplate
An optional link::Classes/Integer:: or link::Classes/Function:: (or object which responds to the method link::Overviews/Methods#matchItem::) used to match the value of an incoming MIDI message. (e.g. velocity, control value, program number, etc.). If a Function, it will be evaluated with the message value as an argument, and should return a link::Classes/Boolean:: indicating whether the message matches and this MIDIFunc should respond.

@section{argument}
  dispatcher
An optional instance of an appropriate subclass of link::Classes/AbstractDispatcher::. This can be used to allow for customised dispatching. Normally this should not be needed.

@section{returns}
  A new instance of MIDIFunc.

@section{METHOD}
  cc
A convenience method to create a new MIDIFunc which responds to MIDI control messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI control messages.

@section{METHOD}
  noteOn
A convenience method to create a new MIDIFunc which responds to MIDI note on messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI note on messages.

@section{METHOD}
  noteOff
A convenience method to create a new MIDIFunc which responds to MIDI note off messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI note off messages.

@section{METHOD}
  polytouch
A convenience method to create a new MIDIFunc which responds to MIDI polytouch messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI polytouch messages.

@section{METHOD}
  touch
A convenience method to create a new MIDIFunc which responds to MIDI touch messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI touch messages.

@section{METHOD}
  bend
A convenience method to create a new MIDIFunc which responds to MIDI bend messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI bend messages.

@section{METHOD}
  program
A convenience method to create a new MIDIFunc which responds to MIDI program change messages. See link::#*new:: for argument descriptions.

@section{returns}
  A new instance of MIDIFunc which responds to MIDI program change messages.

@section{METHOD}
  sysex
A convenience method to create a new MIDIFunc which responds to MIDI system exclusive messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments data (an link::Classes/Int8Array::) and srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI system exclusive messages.

@section{SUBSECTION}
  System Common

N.B. Because of SC's underlying low level MIDI implementation, there is no generic 
@racketblock[msgType:: and convenience method for System Common messages. Instead these are grouped with System Realtime under link::#*sysrt:: below.

]
@section{METHOD}
  smpte
A convenience method to create a new MIDIFunc which responds to MIDI smpte messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments seconds, framerate, dropframe, and srcID. (dropframe is a link::Classes/Boolean::).

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI smpte messages.

@section{METHOD}
  mtcQuarterFrame
A convenience method to create a new MIDIFunc which responds to MIDI Time Code Quarter Frame messages. Note that the link::#smpte:: method above automatically assembles quarter frames into time code. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments data, srcID, and pieceNumber. You will need to manually assemble each 8 messages into smpte.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI Time Code Quarter Frame messages.

@section{METHOD}
  songPosition
A convenience method to create a new MIDIFunc which responds to MIDI song position messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments position and srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI song position messages.

@section{METHOD}
  songSelect
A convenience method to create a new MIDIFunc which responds to MIDI song select messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments song and srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI song select messages.

@section{METHOD}
  tuneRequest
A convenience method to create a new MIDIFunc which responds to MIDI tune request messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI tune request messages.

@section{METHOD}
  midiClock
A convenience method to create a new MIDIFunc which responds to MIDI clock messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI clock messages.

@section{SUBSECTION}
  System Realtime

@section{METHOD}
  sysrt
A convenience method to create a new MIDIFunc which responds generically to MIDI System Realtime and System Common messages. Note that the message specific methods above and below are probably more convenient in most cases. Note that this does not include MIDI Time Code Quarter Frame messages (sysrt index 1). For those see link::#*mtcQuarterFrame:: and link::#*smpte::. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the arguments data (may be nil), srcID, and index. Index indicates the message type as follows:
]
@section{table}
 
## MIDI Time Code Quarter Frames || 1
## Song Position || 2
## Song Select || 3
## Tune Request || 6
## MIDI Clock || 8
## Tick || 9
## Start || 10
## Continue || 11
## Stop || 12
## Active Sense || 14
## Reset || 15
::

@section{returns}
  A new instance of MIDIFunc which responds to MIDI System Realtime messages.

@section{METHOD}
  tick
A convenience method to create a new MIDIFunc which responds to MIDI tick messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI tick messages.

@section{METHOD}
  start
A convenience method to create a new MIDIFunc which responds to MIDI start messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI start messages.

@section{METHOD}
  stop
A convenience method to create a new MIDIFunc which responds to MIDI stop messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI stop messages.

@section{METHOD}
  continue
A convenience method to create a new MIDIFunc which responds to MIDI continue messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI continue messages.

@section{METHOD}
  reset
A convenience method to create a new MIDIFunc which responds to MIDI reset messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI reset messages.

@section{METHOD}
  activeSense
A convenience method to create a new MIDIFunc which responds to MIDI active sense messages. See link::#*new:: for argument descriptions. The responding 
@racketblock[func:: will be passed the argument srcID.

]
@section{returns}
  A new instance of MIDIFunc which responds to MIDI active sense messages.

@section{SUBSECTION}
  Debugging

@section{METHOD}
  trace
A convenience method which dumps all incoming MIDI messages.

@section{argument}
  bool
A link::Classes/Boolean:: indicating whether dumping is on or off.

@section{INSTANCEMETHODS}
 
@section{private}
  init, printOn

@section{METHOD}
  chan
Get this MIDIFunc's MIDI channel number.

@section{returns}
  An link::Classes/Integer::.

@section{METHOD}
  msgNum
Get this MIDIFunc's MIDI message number.

@section{returns}
  An link::Classes/Integer::.

@section{METHOD}
  msgType
Get this MIDIFunc's message type.

@section{returns}
  A link::Classes/Symbol::; one of 
@racketblock[\noteOn::, ]

@racketblock[\noteOff::, ]

@racketblock[\control::, ]

@racketblock[\touch::, ]

@racketblock[\polytouch::, ]

@racketblock[\bend::, ]

@racketblock[\program::, ]

@racketblock[\sysex::, ]

@racketblock[\mtcQF::, ]

@racketblock[\smpte::, ]

@racketblock[\songPosition::, ]

@racketblock[\songSelect::, ]

@racketblock[\tuneRequest::, ]

@racketblock[\midiClock::, ]

@racketblock[\sysrt::, ]

@racketblock[\tick::, ]

@racketblock[\start::, ]

@racketblock[\continue::, ]

@racketblock[\stop::, ]

@racketblock[\activeSense::, or ]

@racketblock[\reset::.

]
@section{METHOD}
  argTemplate
Get this MIDIFunc's argTemplate.
@section{returns}
  A link::Classes/Integer:: or link::Classes/Function:: (or object which responds to the method link::Overviews/Methods#matchItem::) used to match the value of an incoming MIDI message. (e.g. velocity, control value, program number, etc.).

@section{METHOD}
  learn
Train this MIDIFunc to respond to the next message of its type. Arguments passed at creation (e.g. chan, srcID, msgNum, etc.) will filter the training to the next matching message received.

@section{argument}
  learnVal
A link::Classes/Boolean:: indicating whether the responder should learn the specific value. For example, if used with a control change MIDIFunc, the object would learn to match the next specific control change value. If used with a note on MIDIFunc, it would match only a specific velocity. The default is false.


@section{EXAMPLES}
 


@racketblock[
MIDIIn.connectAll

a = MIDIFunc.cc({arg ...args; args.postln}, 1); // match cc 1
a.free; // cleanup

b = MIDIFunc.cc({arg ...args; args.postln}, 1, 1); // match cc1, chan 1
b.free; // cleanup

c = MIDIFunc.cc({arg ...args; args.postln}, (1..10)); // match cc 1-10
c.free; // cleanup

n = MIDIFunc.noteOn({arg ...args; args.postln}); // match any noteOn
n.free; // cleanup

MIDIIn.doNoteOnAction(1, 1, 64, 64); // spoof a note on
MIDIIn.doControlAction(1, 1, 1, 64); // spoof a cc
MIDIIn.doControlAction(1, 1, 9, 64);
MIDIIn.doControlAction(1, 10, 1, 64);
::
]


