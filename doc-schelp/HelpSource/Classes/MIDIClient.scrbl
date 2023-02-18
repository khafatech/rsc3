#lang scribble/manual
@(require (for-label racket))

@title{MIDIClient}
 Basic access to MIDI on your computer@section{categories}
  External Control>MIDI
@section{related}
  Classes/MIDIIn, Classes/MIDIOut, Classes/MIDIFunc, Classes/MIDIdef, Guides/MIDI, Guides/UsingMIDI

@section{description}

MIDIClient is the core class that provides access to the MIDI subsystem on your computer.

See the link::Guides/UsingMIDI:: helpfile for practical considerations and techniques for using MIDI in SC.

@section{CLASSMETHODS}
 

@section{private}
  prInit, prDisposeClient, prInitClient, prList


@section{METHOD}
  init
Initializes the MIDIClient, checks which available MIDI sources and destinations there are, and opens as many connections as desired.

@section{ARGUMENT}
  inports
the number of MIDI input connections to open; if 
@racketblock[nil:: then opens as many inports as there are MIDI sources.

]
@section{ARGUMENT}
  outports
the number of MIDI output connections to open; if 
@racketblock[nil:: then opens as many outports as there are MIDI destinations.

]
@section{ARGUMENT}
  verbose
A flag whether or not to post the MIDI sources and destinations that were found. Default is true.


@section{METHOD}
  initialized
A flag that tells whether of not the MIDIClient has been initialized.



@section{METHOD}
  disposeClient
Cleans up the MIDIClient. After using this method, you will have to reinitialize the MIDIClient before you can use MIDI again.



@section{METHOD}
  list
Created the list of available sources and destinations.


@section{METHOD}
  sources
The list of available MIDI sources, including SuperCollider's own sources.

@section{returns}
  A 
@racketblock[]
@section{List}
  of 
@racketblock[MIDIEndPoints::

]
@section{METHOD}
  externalSources
The list of available MIDI sources, excluding SuperCollider's own sources. Only on Linux the list of 
@racketblock[sources:: and ]

@racketblock[externalSources:: differs.

]
@section{returns}
  A 
@racketblock[]
@section{List}
  of 
@racketblock[MIDIEndPoints::

]
@section{METHOD}
  destinations
The list of available MIDI destinations, including SuperCollider's own destinations.

@section{returns}
  A 
@racketblock[]
@section{List}
  of 
@racketblock[MIDIEndPoints::

]
@section{METHOD}
  externalDestinations
The list of available MIDI destinations, excluding SuperCollider's own destinations. Only on Linux the list of 
@racketblock[destinations:: and ]

@racketblock[externalDestinations:: differs.

]
@section{returns}
  A 
@racketblock[]
@section{List}
  of 
@racketblock[MIDIEndPoints::

]
@section{METHOD}
  restart
Restart the MIDIClient.


@section{METHOD}
  myinports
The number of input ports that SuperCollider created. This is mainly useful to know on the Linux platform.


@section{METHOD}
  myoutports
The number of output ports that SuperCollider created. This is mainly useful to know on the Linux platform.

@section{METHOD}
  getClientID
Linux only. This gets the client ID by which the MIDIClient is defined in the ALSA subsystem. It can be used to identify whether a port is belonging to this client or another one.
On non-linux systems, this posts a warning and reutrns nil.

@section{EXAMPLES}
 


@racketblock[
MIDIClient.init;

MIDIClient.disposeClient;

MIDIClient.init( verbose: false );
]


