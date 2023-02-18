#lang scribble/manual
@(require (for-label racket))

@title{Stethoscope}
 An oscilloscope@section{categories}
  GUI>Interfaces
@section{related}
  Classes/ScopeView, Classes/FreqScope

@section{description}


Stethoscope provides a complete oscilloscope GUI. It displays a window containing a bus-plotting link::Classes/ScopeView:: and an interface to configure the plotting and choose among the buses.

@section{SUBSECTION}
  Creation by message .scope

Several classes provide a convenient 'scope' method that creates a Stethoscope to display their data. See for example: link::Classes/Server#-scope::, link::Classes/Bus#-scope::, link::Classes/Function#-scope::.

@section{SUBSECTION}
  Keyboard shortcuts

The following keyboard shortcuts may be used when focused on the Stethoscope display:

@section{table}
 
## strong::Shortcut:: || strong::Action::
## J || one channel back
## K || switch rate (audio vs. control)
## L || one channel forward
## O || jump to first hardware output channel and adjust numChannels to hardware
## I || jump to first hardware input channel and adjust numChannels to hardware
## space || run, if not running already
## . (period) || stop
## M || toggle screen size
## + / - || zoom horizontally
## * / _ || zoom vertically
## S || change style between parallel and overlay
## Shift+S || change style to lissajou
## Shift+A || allocate buffer size so it fills the screen (to next power of two) (this can be dangerous, might crash)
::

@section{CLASSMETHODS}
 

@section{PRIVATE}
  key

@section{METHOD}
  new

    Create a Stethoscope, either as a window, or placed on a given parent view.

    @section{argument}
  server
        A valid Server (either a local or the internal server), or 
@racketblock[nil::, in which case the ]

@racketblock[Server.default:: is used.
    ]
@section{argument}
  numChannels
        An integer. Default value is 2.
    @section{argument}
  index
        The offset index. An Integer. Default is nil.
    @section{argument}
  bufsize
        The size of the analysis buffer. Default is 4096. See also link::#-bufsize::.
    @section{argument}
  zoom
        Horizontal magnification of the displayed wave. Default is 1. See also link::#-xZoom::.
    @section{argument}
  rate
        \audio or \control. Default is \audio.
    @section{argument}
  view
        The optional parent view. Default is nil. If nil, then it will open in its own Window.
    @section{argument}
  bufnum
        The id number of the Buffer to analyze. Default value is nil. If nil, then a Buffer of size bufSize is allocated.

    discussion:
    Example:

@racketblock[
s.boot
{SinOsc.ar([330,440], 0, 0.4)}.play;
Stethoscope(s,2);
::


]
@section{METHOD}
  isValidServer

    Tests whether Stethoscope can operate on the given server (any local server. See link::Classes/Server#-isLocal::).

    @section{argument}
 
        A link::Classes/Server::.
    @section{returns}
 
        A Boolean.

@section{METHOD}
  ugenScopes

    Returns an array of the running ugen scopes.


@racketblock[
s.boot
{[SinOsc.ar.scope,WhiteNoise.ar(0.5).scope]*0.1}.scope(2);
Stethoscope.ugenScopes; // returns the ugen scopes
::

]
@section{METHOD}
  tileBounds

    A utility method used by link::Classes/UGen#-scope:: to tile scope windows.

    @section{returns}
 
        A Rect.



@section{INSTANCEMETHODS}
 


@section{SUBSECTION}
  Data

@section{METHOD}
  server
    The server on which the scope operates.

@section{METHOD}
  rate
    Whether to operate on audio or control busses.
    @section{argument}
 
        One of the two symbols: 
@racketblock[\audio:: or ]

@racketblock[\control::.

]
@section{METHOD}
  index
    The starting index of the busses to scope.
    @section{argument}
 
        An Integer.

@section{METHOD}
  numChannels
    The amount of adjacent busses to scope (from link::#-index:: on).
    @section{argument}
 
        An Integer.

@section{METHOD}
  bufsize
   Defines the maximum allowed link::#-cycle::.

@section{METHOD}
  cycle
    @section{note}
  Only available in Qt GUI ::

    The exact scoping period, in signal frames. Reciprocal to what is also known as emphasis::sweep speed:: in analog oscilloscopes. It is dynamically adjustable while the scope is running.

    Data from scoped signals will be accumulated into a buffer until it reaches 
@racketblock[cycle:: amount frames, at which point the buffering will immediately restart. The view will repeatedly display the entire buffer; it may skip a cycle if the drawing is too slow to keep up with the speed of incoming data, but the cycle boundaries will never shift with respect to signals.

    If you are scoping a periodic signal, setting ]

@racketblock[cycle:: to match the signal's period will keep the waveform locked in place.

]
@section{SUBSECTION}
  Display

@section{METHOD}
  window
    The (parent) Window of the scope.

@section{METHOD}
  size
    Sets the width and the height of the scope window.
    @section{argument}
 
        An Integer (the window is square).

@section{METHOD}
  toggleSize

    Toggle between small and large size.

@section{METHOD}
  zoom
    A synonym for link::#-xZoom::.

@section{METHOD}
  xZoom
    Magnifies the displayed wave horizontally to the given factor.

    This sets link::#-cycle:: to 
@racketblock[1024 * xZoom.reciprocal::.

    ]
@section{argument}
 
        A Float.

@section{METHOD}
  yZoom
    Magnifies the displayed wave vertically to the given factor.

    @section{argument}
 
        A Float.

@section{METHOD}
  style
    The plotting style:
    @section{list}
 
    ## 0 = the channels are vertically spaced
    ## 1 = the channels are overlaid
    ## 2 = lissajou; the first two channels are used for 2D plotting (as streams of x and y coordinates).
    ::

    @section{argument}
 
        One of the above Integers.


@section{SUBSECTION}
  Operation

@section{METHOD}
  run

    Starts the scope, if not already running.

@section{METHOD}
  quit

    Closes the window, and cleans up any used synths and buffers.

@section{SUBSECTION}
  Convenience

@section{METHOD}
  setProperties

    Sets several properties at once: link::#-numChannels::, link::#-index::, link::#-bufsize::, link::#-zoom::, and link::#-rate::.



@section{EXAMPLES}
 

@section{SUBSECTION}
  A step-by-step example

@racketblock[
s.boot;
(
{
    SinOsc.ar([225, 450, 900], 0, 0.2)
    + LPF.ar(
        LFPulse.ar(226 * [1, 2, 5],[0,0.1,0.1],0.2, 0.2),
        MouseX.kr(20, 10000, 1)
        )
}.scope;
)

// server.scope only changes the properies explicitly given:

s.scope(numChannels:5);
s.scope(index:12);
s.scope(zoom:4);
s.scope(index:0);

s.scopeWindow.size = 600;
s.scopeWindow.size = 222;

// scoping buses:

a = Bus.audio(s, 4);
{ WhiteNoise.ar(0.2.dup(4)) }.play(s, a);

a.scope;

c = Bus.control(s, 3);
{ WhiteNoise.kr(1.dup(4) * MouseX.kr) }.play(s, c);

c.scope;

// note that scoping control rate buses shows block size interpolation (this is due to the
// fact that ScopeOut.kr doesn't work yet.)
::

]
@section{SUBSECTION}
  Embedded use
You can pass your own view in to add a stethoscope to it:


@racketblock[
w = Window.new("my own scope", Rect(20, 20, 400, 500));
w.view.decorator = FlowLayout(w.view.bounds);
c = Stethoscope.new(s, view:w.view);
w.onClose = { c.free }; // don't forget this
w.front;
::
]


