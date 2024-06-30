#lang scribble/manual
@(require (for-label racket))

@title{MIDI}
 about MIDI@section{related}
  Guides/UsingMIDI, Classes/MIDIFunc, Classes/MIDIdef
@section{categories}
  External Control>MIDI

@section{section}
  Practical usage overview

Begin with the link::Guides/UsingMIDI:: help file.

@section{subsection}
  Receiving MIDI input

link::Classes/MIDIFunc:: and link::Classes/MIDIdef:: are the standard, recommended way to receive MIDI note on/off, controller, pitch bend, aftertouch, poly-touch and program change messages.

@section{note}
 
strong::IMPORTANT: :: Before MIDI can be received, SuperCollider needs to be told to connect to the MIDI subsystem and connect to the available devices.


@racketblock[
MIDIClient.init;
MIDIIn.connectAll;
::

You need to do this once after launching SuperCollider, or recompiling the class library.
::

There are some examples in the wild using the MIDIIn class directly to receive MIDI. This is not recommended for normal use. The exceptions are sysex (system exclusive) and sysrt (MIDI clock) messages, which are currently supported only by MIDIIn. See the example below.

]
@section{subsection}
  Sending MIDI output

See the link::Classes/MIDIOut:: help file for details.

@section{section}
  Summary of MIDI classes

@section{definitionlist}
 
## link::Classes/MIDIClient:: || This class connects to the operating system's MIDI layer, and obtains the lists of available MIDI sources and destinations. The information about the hardware is stored in 
@racketblock[MIDIClient.sources:: and ]

@racketblock[MIDIClient.destinations:: as link::Classes/MIDIEndPoint:: objects. MIDIClient must be initialized before MIDI can be received. See the note above.
## link::Classes/MIDIFunc:: || The optimal way to receive the most typical MIDI messages: note on/off, controller, pitch bend, aftertouch, poly-touch and program change.
## link::Classes/MIDIdef:: || Related to link::Classes/MIDIFunc::, this class keeps several MIDIFunc objects in global storage, by name. Especially helpful for live or interactive use.
## link::Classes/MIDIOut:: || Supports MIDI output to hardware ports or inter-application MIDI buses.
## link::Classes/MIDIEndPoint:: || Represents a MIDI port published by the operating system. It contains a device name, port name and unique identifier (uid).
## link::Classes/MIDIIn:: || The lowest-level MIDI input class. MIDIFunc and MIDIdef use this class so that you don't have to. It is strongly recommended to avoid using this class directly.
::


]
@section{Examples}
 

MIDI input:


@racketblock[
(
MIDIClient.init;
MIDIIn.connectAll;
m = MIDIFunc.noteOn({ |vel, num|
	"note % @ velocity %\n".postf(num, vel);
});
)

// when finished
m.free;
::


MIDI output:

]

@racketblock[
(
MIDIClient.init;
m = MIDIOut(0, MIDIClient.destinations.at(0).uid);
m.noteOn(0, 60, 60);
)
::

Receiving system exclusive messages:

]

@racketblock[
~sysexFunc = { |uid, data|
	// 'data' holds the sysex packet as 8-bit integers
};
MIDIIn.addFuncTo(\sysex, ~sysexFunc);

// when finished
MIDIIn.removeFuncFrom(\sysex, ~sysexFunc);
::
]

