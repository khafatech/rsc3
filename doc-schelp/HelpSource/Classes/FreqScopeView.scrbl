#lang scribble/manual
@(require (for-label racket))

@title{FreqScopeView}
 Frequency analysis view@section{categories}
  GUI>Views
@section{related}
  Classes/FreqScope

@section{description}

FreqScopeView shows the frequency spectrum of a specified audio bus.

@section{note}
 

The scope will remain active after a command-period. To turn it off you must use the 'active' method.
Very important: You must run 
@racketblock[kill():: when the parent window is closed to avoid problems. It also frees the buffers that the scope allocated and stops the FFT analysis synth. So:
]

@racketblock[
(
w = Window("My Analyzer", Rect(0, 0, 511, 300));
f = FreqScopeView(w, w.view.bounds);
w.onClose_({ f.kill }); // YOU MUST HAVE THIS
w.front;
)
::

::

]
@section{classmethods}
 

@section{private}
 initClass

@section{method}
  new
@section{argument}
  parent
The parent view.
@section{argument}
  bounds
An instance of link::Classes/Rect::, or a link::Classes/Point:: indicating 
@racketblock[width@height::.
]
@section{argument}
  server
The server to be shown in scope.

@section{discussion}
 
Example:

@racketblock[
// Start server
s.boot;

// Create analyzer in a window
(
w = Window("My Analyzer", Rect(0, 0, 511, 300)); // width should be 511
f = FreqScopeView(w, w.view.bounds);
f.active_(true); // turn it on the first time;

w.onClose_({ f.kill }); // you must have this
w.front;
{ SinOsc.ar([500, 1000], 0, 0.25).mean.dup }.play(s); // start two sine waves
)
::

]
@section{method}
  response
Create a scope in a special frequency-response mode. This uses FFT-based spectral division to estimate the frequency response of some effect, on the assumption that the signal to bus1 is transformed to the signal at bus2 by some linear time-invariant process.
@section{argument}
  parent
The parent view.
@section{argument}
  bounds
An instance of link::Classes/Rect::, or a link::Classes/Point:: indicating 
@racketblock[width@height::.
]
@section{argument}
  bus1
The bus on which the "pre" signal is found.
@section{argument}
  bus2
The bus on which the "post" signal is found.
@section{argument}
  freqMode
Linear (0) or log(1) frequency mode. Defaults to 1.
@section{discussion}
 
Example:

@racketblock[
s.boot

// basic usage. try these. Each one will open a new window
// move the mouse left and right to test response in different ranges
LPF.scopeResponse
HPF.scopeResponse
MoogFF.scopeResponse
BBandPass.scopeResponse
BLowShelf.scopeResponse // by default BLowShelf doesn't mangle much
Resonz.scopeResponse
BRF.scopeResponse
Integrator.scopeResponse
Median.scopeResponse // nonlinear, and therefore interesting

// customize the parameters for more informative scoping
{|in| MoogFF.ar(in, freq: MouseX.kr(10, 10000, 1),
gain:MouseY.kr(4, 0))}.scopeResponse
::

]
@section{subsection}
  Subclassing and Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these as needed.

@section{instancemethods}
 

@section{method}
  kill
Very important. This must be run when the parent window is closed to avoid problems. It also frees the buffers that the scope allocated and stops the FFT analysis synth.

@section{method}
  active
Turn the scope on or off.
@section{argument}
  bool
An instance of link::Classes/Boolean::.

@section{method}
  freqMode
@section{argument}
  mode
0 = linear, 1 = logarithmic.

@section{method}
  inBus
The bus to listen on.
@section{argument}
  num
An audio link::Classes/Bus:: number.

@section{method}
  dbRange
Get/set the amplitude range.
@section{argument}
  db
A link::Classes/Number::.

@section{method}
  special
Put the scope into a special mode using a user-specified link::Classes/SynthDef::. Note that only very particular SynthDefs should be used, namely ones that are derived from the 
@racketblock[\freqScope0:: or ]

@racketblock[\freqScope1:: SynthDefs. Most users will not need to use this method directly, but it can be used to provide a customised analysis shown in the scope.
]
@section{argument}
  defname
Name of the link::Classes/SynthDef:: you wish to use.
@section{argument}
  extraArgs
Extra arguments that you may wish to pass to the synth.

@section{subsection}
  instance variables
@section{method}
  server
the server that is freqscoped
@section{method}
  synth
the synth running the freqscope analysis
@section{method}
  scope
the scopeview that shows the running analysis
@section{method}
  scopebuf
the buffer used by the scope

@section{subsection}
  Internal Methods

The following methods are usually not used directly or are called by a primitive. Programmers can still call or override these in subclasses as needed.

@section{method}
  initFreqScope
initialize and show on parent view
@section{method}
  doesNotUnderstand
redirects methods to scope view variable

@section{method}
  start
@section{method}
  allocBuffers
@section{method}
  freeBuffers
@section{method}
  bufSize
@section{method}
  doOnServerQuit
@section{method}
  doOnServerTree
@section{method}
  shmScopeAvailable
@section{method}
  specialSynthArgs
@section{method}
  specialSynthDef

@section{examples}
 


@racketblock[
// Start server
s.boot;

// Create analyzer in a window
(
w = Window("My Analyzer", Rect(0, 0, 511, 300)); // width should be 511
f = FreqScopeView(w, w.view.bounds);
f.active_(true); // turn it on the first time;

w.onClose_({ f.kill }); // you must have this
w.front;
{ SinOsc.ar([500, 1000], 0, 0.25).mean.dup }.play(s); // start two sine waves
)

f.freqMode_(1); // change to log scale so we can see them
f.inBus_(1); // look at bus 1
f.dbRange_(200); // expand amplitude range
f.active_(false); // turn scope off (watch CPU)
f.active_(true); // turn it back on

// Now press command-period. The scope is still running.

{ Mix.ar(SinOsc.ar([500, 1200, 3000, 9000, 12000], 0, [0.2, 0.1, 0.05, 0.03, 0.01])) }.play(s); // restart some sines

// Close window and scope is killed.
::
]


