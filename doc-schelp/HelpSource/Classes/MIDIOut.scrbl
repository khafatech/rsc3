#lang scribble/manual
@(require (for-label racket))

@title{MIDIOut}
 send MIDI messages@section{related}
  Classes/MIDIClient, Classes/MIDIIn, Guides/MIDI, Guides/UsingMIDI
@section{categories}
  External Control>MIDI

@section{description}

a MIDIOut is bound to a specific link::Classes/MIDIEndPoint:: as defined by the operating system.

@section{ClassMethods}
 

@section{private}
 connectByUID, disconnectByUID

@section{method}
 new

@section{argument}
 port
the index of the MIDIEndPoint in the 
@racketblock[MIDIClient.destinations:: array.

]
@section{argument}
 uid
@section{definitionList}
 
## macOS / Windows || uid is optional; if specified, it should be the uid of that port ie. MIDIClient.destinations[port].uid. If you don't provide a uid, the correct uid will be filled in for you (easier).
## Linux || using the uid is optional as described below.
::

@section{method}
 newByName
Searches for the device by name. This is safer then depending on the index which will change if your studio setup changes.

@racketblock[
//list connected out ports with names:
MIDIClient.init;
MIDIClient.destinations;
::

]
@section{method}
 findPort
Searches for a connected MIDIEndPoint by name.

@racketblock[
//list connected out ports with names:
MIDIClient.init;
MIDIClient.destinations;
::

]
@section{method}
 connect, disconnect
Linux only. MacOS does not need to connect. On Linux it is an optional feature (see below).

@section{InstanceMethods}
 
@section{method}
 sysex
Sends a sysex command represented as an link::Classes/Int8Array:: to the device.

@section{note}
 
On Windows, the function call must contain a full sysex message. In other words,
it must start with 0xF0 (240 or -16) and end with 0xF7 (247 or -9).
::

@section{argument}
  packet
An Int8Array of data bytes to be sent.

@section{private}
 send, prSysex

@section{method}
 latency
This sets the latency with which a midi event is sent out. Per default this is set to 0.2, in order to be equal to the Server.latency.
@section{note}
 
On Linux there seems to be an ALSA or kernel bug if the latency is larger than 0, for some Linux kernels. If MIDIOut does not seem to work, set the latency to 0.
::

@section{Examples}
 


@racketblock[
MIDIClient.init;

m = MIDIOut(0);
m.noteOn(16, 60, 60);
m.noteOn(16, 61, 60);
m.noteOff(16, 61, 60);
m.allNotesOff(16);


MIDIIn.connect; // 1 port midi interface
MIDIIn.sysex = { arg uid, packet; [uid,packet].postln };
MIDIIn.sysrt = { arg src, chan, val;  [src, chan, val].postln; };
MIDIIn.smpte = { arg src, chan, val;  [src, chan, val].postln; };

m.sysex(Int8Array[0xF0, 0, 0, 27, 11, 0, 0xF7])

m.smpte(24, 16)
m.midiClock
m.start
m.continue
m.stop
::

]
@section{subsection}
 Using patterns for sending MIDI events


@racketblock[
MIDIClient.init;
m = MIDIOut(0);

a = Pbind(\degree, Prand([1, 2, 3, [0, 5]], inf), \bend, Pwhite(0, 76, inf));


// chain a midi event into the pattern and play it (see Pchain)

(a <> (type: \midi, midiout: m)).play;
::

See link::Tutorials/A-Practical-Guide/PG_08_Event_Types_and_Parameters#MIDI output:: for a list of midi commands supported by the 'midi' event type.

]
@section{subsection}
 Linux specific: Connecting and disconnecting ports

On Linux, a MIDIOut can be created without setting the destination:


@racketblock[
m = MIDIOut(0);
::

In this case each message will be sent to all ports connected to SuperCollider's first MIDI output.

A connection can be made through:

]

@racketblock[
m.connect( 2 );
::

Note that by connecting in this way, you can connect more than one destination to the MIDI output.
You can also use other tools to connect to a MIDIOut port of SC, e.g. through aconnect or QJackCtl (on the ALSA tab).

If you set the uid in MIDIOut, a direct connection is established and data will only be sent to that MIDI input port, and not to any other connections made to SC's MIDI output port (through the connect message or external tools).

]
@section{subsection}
 macOS specific: Sending MIDI to other applications

Open the Audio MIDI Setup application. Double-click on IAC Driver and check "device is online".

reinitialize:

MIDIClient.init(numIns,numOuts)

The IAC Bus will now appear in MIDIClient.destinations. It will appear first, which means that any code that you have written that addresses the first physical bus as 0 will now have to be changed.

For this reason it is always safer to find the port by name :

@racketblock[
MIDIOut.newByName("RemoteSL IN","Port 1");
::
The IAC Bus will now also appear to other applications.


MIDIMonitor (freeware) can be very useful for troubleshooting:

http://www.snoize.com/MIDIMonitor/

]
@section{subsection}
 Sysex example

a machinedrum manual say sysex commands should be formatted like this...

@racketblock[
$f0,$00,$20,$3c,$02,$00,command,...,$f7
::

and to set the tempo the machinedrum expects this command...
]

@racketblock[
$61 | Set tempo ID
%0aaaaaaa | Upper bits
%0bbbbbbb | Lower bits
$f7 | SYSEX end
Note: Tempo = %aaaaaaabbbbbbb / 24, max 300 BPM, min 30 BPM
::

so to create and send a valid set tempo sysex command from SuperCollider to this machinedrum do...
]

@racketblock[
MIDIClient.init;
m = MIDIOut(0);
m.sysex(Int8Array[0xf0, 0x00, 0x20, 0x3c, 0x02, 0x00, 0x61, 21, 54, 0xf7]);
::

This will set the tempo to 114.23 bpm. One can calculate the upper and lower 7bit values like this...
]

@racketblock[
(
var bpm, val, upper, lower;
bpm = 114.23;
val = (bpm*24).round.asInteger;
upper = val&2r11111110000000>>7;
lower = val&2r00000001111111;
[upper, lower].postln;
)
::
where the resulting 21 and 54 are the same as 2r0010101 and 2r0110110 in binary notation.
]


