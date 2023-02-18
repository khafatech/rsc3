#lang scribble/manual
@(require (for-label racket))

@title{FreqScope}
 Frequency spectrum visualizer@section{categories}
  GUI>Interfaces
@section{related}
  Classes/FreqScopeView

@section{description}

FreqScope shows the frequency spectrum of the specified audio bus. The scope will remain active after a command-period. To turn it off you must either click off the 'Power' button or close the window.

Panel commands:
@section{table}
 
## Power || Turns the scope on and off. This is useful for freezing the signal on the display or for saving CPU.
## BusIn || The audio bus to be analyzed.
## FrqScl || Determines the mapping of frequencies on the x-axis. Can be linear (lin) or logarithmic (log). Logarithmic is equal spacing per musical octave.
## dbCut || Determines the lowest decibel shown on the y-axis.
::

@section{ClassMethods}
 

@section{method}
  new
@section{argument}
  width
Default value is 512.
@section{argument}
  height
Default value is 300.
@section{argument}
  busNum
The number of the audio link::Classes/Bus:: to be monitored.
@section{argument}
  scopeColor
An instance of link::Classes/Color::. The drawing color of the scope.
@section{argument}
  bgColor
An instance of link::Classes/Color::. The background color of the scope.
@section{argument}
  server
the server whose buses to show on scope.
@section{discussion}
 
Example:

@racketblock[
s.boot;

// create a new analyzer
FreqScope.new(400, 200, 0, server: s);

// basic sine
{ SinOsc.ar(2000, 0, 0.25) }.play(s);

// random saw
{ RLPF.ar(Saw.ar(110, 0.2), LFNoise2.kr(1,1e4,1e4), LFNoise2.kr(1, 0.2, 0.22)) }.play(s);

// modulate phase
{ SinOsc.ar(800, SinOsc.ar(XLine.kr(20, 8000, 10), 0, 2pi), 0.25) }.play(s);

// all harmonics
{ Blip.ar(200, Line.kr(1, 100, 10), 0.2) }.play(s);
::

]
@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
  scopeOpen
Returns a link::Classes/Boolean::, whether the scope is open.

@section{InstanceMethods}
 

@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{method}
 window
Returns the window in which the link::Classes/FreqScopeView:: is placed.

@section{method}
  scope
Returns the link::Classes/FreqScopeView::.


