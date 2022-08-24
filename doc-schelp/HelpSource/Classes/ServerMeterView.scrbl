#lang scribble/manual
@(require (for-label racket))

@title{ServerMeterView}
 A GUI widget that displays input/output levels@section{categories}
  GUI>Views
@section{related}
  Classes/ScopeView, Classes/FreqScopeView, Classes/ServerMeter

@section{description}

A link::Classes/ServerMeterView:: is a modular widget for showing the volume levels of inputs/outputs on the server. link::Classes/ServerMeterView:: can be embedded inside of your custom graphical user interfaces just like a button or slider.

@section{note}
 
If you are looking for a quick input/output levels display without having to build your own GUI from scratch, see link::Classes/ServerMeter:: and link::Classes/Server#-meter::
::



@section{CLASSMETHODS}
 

@section{METHOD}
  height
Get the height in pixels of the standard link::Classes/ServerMeterView:: widget

@section{returns}
  an link::Classes/Integer::

@section{METHOD}
  getWidth
Get the width in pixels of a link::Classes/ServerMeterView:: widget with the given number of inputs and outputs

@section{ARGUMENT}
  numIns
number of inputs used to calculate the width

@section{ARGUMENT}
  numOuts
number of outputs used to calculate the width

@section{ARGUMENT}
  server
the server

@section{returns}
  an link::Classes/Integer::

@section{METHOD}
  new
Create a new link::Classes/ServerMeterView:: instance

@section{ARGUMENT}
  aserver
The link::Classes/Server:: whose inputs/outputs will be monitored

@section{ARGUMENT}
  parent
The parent link::Classes/View:: or link::Classes/Window:: where the new link::Classes/ServerMeterView:: will be embedded.

@section{ARGUMENT}
  leftUp
Where to position the new link::Classes/ServerMeterView:: inside the parent. strong::leftUp:: must be a link::Classes/Point::, describing where to place the upper left corner of the new link::Classes/ServerMeterView::.

@section{ARGUMENT}
  numIns
The number of inputs to monitor

@section{ARGUMENT}
  numOuts
The number of outputs to monitor

@section{returns}
  A new link::Classes/ServerMeterView::


@section{INSTANCEMETHODS}
 

@section{METHOD}
  view
get the link::Classes/CompositeView:: used to construct the various elements of the link::Classes/ServerMeterView:: widget

@section{returns}
  a link::Classes/CompositeView::

@section{METHOD}
  remove
Removes this link::Classes/ServerMeterView:: from its parent view (if any) and then destroys it. Once this method is called you can no longer use this link::Classes/ServerMeterView::.

@section{METHOD}
  start
Enable the monitoring of input/outputs

@section{returns}
  this link::Classes/ServerMeterView::

@section{METHOD}
  stop
Disable the monitoring of input/outputs

@section{returns}
  this link::Classes/ServerMeterView::


@section{PRIVATE}
  setSynthFunc, startResponders, init


@section{EXAMPLES}
 

@section{subsection}
 Simple Usage


@racketblock[
// make a window and embed a ServerMeterView inside of it.
w = Window.new("Server Levels");
ServerMeterView.new(s, w, 0@0, 2, 2);
w.front; // show the window
::

]
@section{subsection}
 A More Complex Example


@racketblock[
// make a GUI to monitor two servers running simultaneously
s = Server.local;
q = Server.internal;
s.boot; q.boot; // wait a moment for the servers to boot

// make a window big enough to hold 2 ServerMeterViews
r = Rect(0, 0, ServerMeterView.getWidth(2, 2) * 2, ServerMeterView.height)
w = Window.new("Local | Internal", r);

// make one ServerMeterView to monitor the input/output of each server
ServerMeterView.new(s, w, Point(0,0), 2, 2);
ServerMeterView.new(q, w, Point(ServerMeterView.getWidth(2,2), 0), 2, 2);
w.front; // show the window
]


