#lang scribble/manual
@(require (for-label racket))

@title{ServerMeter}
 Graphical feedback window showing input/output levels@section{categories}
  GUI>Interfaces
@section{related}
  Classes/Stethoscope, Classes/FreqScope, Classes/ServerMeterView

@section{description}

A ServerMeter is a simple graphical display showing the volume levels of inputs and outputs on the server.

Also see:
link::Classes/Server#-meter::

@section{CLASSMETHODS}
 

@section{METHOD}
  new
Create a new ServerMeter.

@section{ARGUMENT}
  server
The link::Classes/Server:: whose inputs and outputs you wish to visualize.

@section{ARGUMENT}
  numIns
The number of inputs you want to display (starts counting at the first input bus, similar to link::Classes/SoundIn::)

@section{ARGUMENT}
  numOuts
The number of outputs you want to display (starts counting at bus 0)

@section{returns}
  A reference to the newly created ServerMeter


@section{INSTANCEMETHODS}
 

@section{METHOD}
  window
Get a reference to the link::Classes/Window:: of this ServerView

@section{returns}
  a link::Classes/Window:: reference

@section{METHOD}
  meterView
Get a reference to the link::Classes/ServerMeterView:: of this ServerView

@section{returns}
  a link::Classes/ServerMeterView:: reference

@section{note}
 
A ServerMeter encapsulates both a link::Classes/Window:: and a link::Classes/ServerMeterView:: within that Window. For more information about Windows and views see link::Guides/GUI-Introduction#Basic elements: Windows, views and containers::
::


@section{EXAMPLES}
 


@racketblock[

s = Server.internal; // use the internal server
s.boot;
// display 4 input channels & main stereo output
m = ServerMeter.new(s, 4, 2);
]


