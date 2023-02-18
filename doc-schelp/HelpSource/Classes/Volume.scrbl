#lang scribble/manual
@(require (for-label racket))

@title{Volume}
 Model for the global volume of the synthesis server@section{categories}
  Server

@section{description}

Internally used by Server. When volume value != 0 dB or muted, a server's volume object will create a synth for controlling the volume on the main outputs for the number of channels given.

@section{classmethods}
 
@section{method}
  new
Create and return a new instance of Volume for a given server,
ranging from 
@racketblock[startBus:: over ]

@racketblock[numChans:: (usually the server's number of output bus channels).
]
@section{argument}
  server
a server
@section{argument}
  startBus
start bus
@section{argument}
  numChannels
number of channels
@section{argument}
  min
minimum volume in decibel
@section{argument}
  max
minimum volume in decibel
@section{argument}
  persist
whether to persist a reset

@section{instancemethods}
 

@section{method}
  mute
mute output

@section{method}
  unmute
unmute output

@section{method}
  volume
set the volume (in db)

@section{method}
  lag
set the lag time that dampens volume changes

@section{method}
  setVolumeRange
set the volume range

@section{method}
  gui
create a volume gui.

@section{examples}
 

@racketblock[
v = s.volume;

v.min;
v.max;
v.volume = rrand(-50, 5);
v.setVolumeRange(-90, 8);
v.mute;
v.unmute;

// separate window
v.gui;
::

]


