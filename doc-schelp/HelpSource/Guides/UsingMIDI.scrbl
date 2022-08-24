#lang scribble/manual
@(require (for-label racket))

@title{Using MIDI}
 Notes on MIDI support in SuperCollider@section{related}
  Guides/MIDI, Classes/MIDIFunc, Classes/MIDIdef
@section{categories}
  External Control>MIDI

@section{keyword}
 MIDI
@section{section}
 Introduction

SuperCollider's out of the box MIDI support is fairly thorough (although not as complete as you'll find in commercial sequencers). All MIDI devices accessible to your operating system (CoreMIDI on macOS, ALSA on Linux, PortMIDI on Windows) are accessible to SuperCollider.

The main MIDI classes are:

@section{definitionList}
 
## link::Classes/MIDIClient:: || This class connects to the operating system's MIDI layer, and obtains the lists of available MIDI sources and destinations. The information about the hardware is stored in 
@racketblock[MIDIClient.sources:: and ]

@racketblock[MIDIClient.destinations:: as link::Classes/MIDIEndPoint:: objects. MIDIClient must be initialized before MIDI can be received. See the example link::#Playing notes on your MIDI keyboard::.
## link::Classes/MIDIFunc:: || The optimal way to receive the most typical MIDI messages: note on/off, controller, pitch bend, aftertouch, poly-touch and program change.
## link::Classes/MIDIdef:: || Related to link::Classes/MIDIFunc::, this class keeps several MIDIFunc objects in global storage, by name. Especially helpful for live or interactive use.
## link::Classes/MIDIOut:: || Supports MIDI output to hardware ports or inter-application MIDI buses.
## link::Classes/MIDIEndPoint:: || Represents a MIDI port published by the operating system. It contains a device name, port name and unique identifier (uid).
## link::Classes/MIDIIn:: || The lowest-level MIDI input class. MIDIFunc and MIDIdef use this class so that you don't have to. It is strongly recommended to avoid using this class directly.
::

In most cases, each physical MIDI connection (pair of in/out jacks on the MIDI interface) has one MIDIEndPoint object to represent it in the client.

]
@section{section}
 Receiving MIDI input
@section{subsection}
  MIDIFunc and MIDIdef

For most uses, the preferred way to receive MIDI input is using the link::Classes/MIDIFunc:: and link::Classes/MIDIdef:: classes. The advantage of this approach is that any number of responders can be registered, using extremely flexible matching.

link::Classes/MIDIFunc:: has a number of convenience methods allowing you to register for the different MIDI message types. It can  filter incoming MIDI messages to respond to a particular device, channel number, or specific message number, or ranges thereof.

See link::#Playing notes on your MIDI keyboard:: below for a simple example using the note-on and note-off MIDIFuncs.

@section{subsection}
  MIDIIn

MIDIIn has a number of class variables holding functions to be evaluated when a MIDI event comes in. Technical details on each function can be found in the link::Classes/MIDIIn:: help file.

Note, however, that MIDIIn provides no functionality for filtering incoming MIDI based on device, controller number or other factors. This means, for practical use, MIDIIn is significantly harder to use than link::Classes/MIDIFunc:: or link::Classes/MIDIdef::. strong::It is generally recommended to avoid using MIDIIn directly.:: The exceptions are sysex (system exclusive) and sysrt (MIDI clock) messages, which are currently supported only by MIDIIn.

@section{section}
 Playing notes on your MIDI keyboard

The technical problem is that every note on needs to save its synth object so that the note off message can end the right server-side node.


@racketblock[
s.boot;

(
var notes, on, off;

MIDIClient.init;
MIDIIn.connectAll;

notes = Array.newClear(128);	// array has one slot per possible MIDI note

on = MIDIFunc.noteOn({ |veloc, num, chan, src|
	notes[num] = Synth(\default, [\freq, num.midicps,
		\amp, veloc * 0.00315]);
});

off = MIDIFunc.noteOff({ |veloc, num, chan, src|
	notes[num].release;
});

q = { on.free; off.free; };
)

// when done:
q.value;
::

The link::Classes/MIDIIn:: help file contains a more elaborate example.

SuperCollider does not have a built-in class to handle this automatically. However, emphasis::dewdrop_lib::, a third party library mentioned link::#Third party libraries#below::, includes Voicer (to simplify note on-off bookkeeping) and VoicerMIDISocket (to trigger Voicer notes by MIDI). Users interested in this functionality may wish to examine that library.

]
@section{section}
 Sending MIDI out

See the link::Classes/MIDIOut:: helpfile. Unlike MIDIIn, with MIDIOut you create an instance of the MIDIOut class with a port and uid. You can have multiple MIDIOut objects to send MIDI to different physical devices.

Many users have reported timing issues with MIDIOut. When the CPU is busy, especially during graphics updates, outgoing MIDI messages may be delayed. Use with caution in a performance situation.

@section{section}
 MIDI synchronization

MIDI synchronization may be performed using MIDIIn's sysrt or smpte response functions. It's up to the user to implement the desired kind of synchronization.

For sysrt, external MIDI clocks output 24 pulses per quarter note. The responder should count the incoming pulses and multiply the rhythmic value into 24 to determine how many pulses to wait:

@section{table}
 
## 0.25 || wait 6 pulses (16th note)
## 0.5 || wait 12 pulses (8th note)
## 2 || wait 48 pulses (half note)
::

dewdrop_lib (third party library) includes a class, MIDISyncClock, that receives MIDI clock messages and allows events to be scheduled to keep time with an external MIDI device. See the MIDISyncClock helpfile for details.

There are significant limitations, discussed in the helpfile. This is not really a fully supported class, but it's there for users who need rudimentary MIDI sync functionality.

@section{section}
 Third party libraries

emphasis::dewdrop_lib:: is a third party library providing a number of useful performance features, available through the link::Classes/Quarks:: interface. The library provides a user-extensible framework of MIDI responder classes designed for multiport, multichannel applications.

Among its features:

- user-extensible: simple functions may be used, and frequently-needed responses can be written into classes that inherit from the framework (see BasicMIDISocket and BasicMIDIControl helpfiles)

- easy to use classes for playing MIDI notes and assigning MIDI controllers to synthesis parameters

- a user-configurable array of MIDI controller numbers, to simplify assignment of events to hardware controllers


