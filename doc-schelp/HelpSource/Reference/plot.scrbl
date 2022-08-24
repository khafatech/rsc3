#lang scribble/manual
@(require (for-label racket))

@title{plot}
 Plot data in a graph@section{categories}
  Common methods, GUI


@section{section}
  Description
The link::Overviews/Methods#plot#plot @section{method}
  provides the ability to plot data in a GUI window. The method is implemented in the link::Classes/ArrayedCollection:: class but is also available for other classes for convenience, including link::Classes/Function::, link::Classes/Bus::, link::Classes/Env::, link::Classes/Buffer::, link::Classes/SoundFile::, link::Classes/Wave@section{table}
 .

@section{method}
  plot
All arguments are optional.
@section{note}
 The arguments available vary from object to object. The below list is only for explanation of possible arguments.::


@section{argument}
  name
The name to be used as the GUI window title.

@section{argument}
  bounds
A link::Classes/Rect:: providing coordinates for the GUI location.

@section{argument}
  discrete
Plots are line-plots by default. Set this to 
@racketblock[true:: for bar charts.

]
@section{argument}
  numChannels
The number of interleaved channels that an array represents. For Buffers this argument is not available, since it's filled in automatically.

@section{argument}
  minval
Minimum value(s) for the display range. For a Buffer this defaults to 
@racketblock[-1:: but can be changed.

]
@section{argument}
  maxval
Maximum value(s) for the display range. For a Buffer this defaults to 
@racketblock[+1:: but can be changed.

]
@section{argument}
  separately
When finding the right display range in multi channel plots, do this together for all or keep them separate.

@section{argument}
  parent
By default the plot is placed in a new GUI window. This argument can be used to specify an existing GUI container to send the plot to.

@section{argument}
  labels
By default labels appear at the top left of the plot giving a data readout based on mouse position. Set this argument to 
@racketblock[false:: to prevent them appearing.


]
@section{discussion}
 
If 
@racketblock[minval:: and/or ]

@racketblock[maxval:: are set to ]

@racketblock[nil:: (this is default, except for link::Classes/Buffer::s), they will be automatically calculated from the dataset minimum and/or maximum. For multi-channel data, ]

@racketblock[minval:: and ]

@racketblock[maxval:: may be arrays, specifying the range independently for each channel (including use of ]

@racketblock[nil::, in which case the min/max will be calculated for the specific channel rather than for the overall dataset).

Hitting the strong::E-key:: on the keyboard when the window is focussed toggles the lock, and the window can be used to edit the data.

]
@section{section}
  Examples

@section{note}
  See some of the classes linked above for more examples ::


@racketblock[
// Arrays
[5, 6, 7, 6.5, 4.5, 3.5].plot("Some data")
[5, 6, 7, 6.5, 4.5, 3.5].plot("Some data, in stereo", numChannels:2)
[5, 6, 7, 6.5, 4.5, 3.5].plot("Some data, in stereo", numChannels:2, discrete: true)

{ |i| { |j| j + 1 * (i + 1) % 6 }.dup(100) }.dup(5).plot("Some 2-d data");

// 3-channel interlaced data
b = [{1.0.rand}.dup(50), { 20.0.rand - 30 }.dup(50),{ 10.0.rand }.dup(50)].lace(150);
b.plot(numChannels:3); // Common rescaling
b.plot(numChannels:3, separately: false);
b.plot(numChannels:3, minval: [nil, -100, nil], maxval: [nil, nil, 10]); // multichannel range parameters

// Envelopes
Env.adsr(0.4, 0.4, 0.8, 0.9).plot

// Buffers
s.boot;
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
b.plot; // +-1 range
b.plot(minval: nil, maxval: nil); // auto range
b.plot(minval: 0, maxval: nil); // semi-auto range

// UGen functions
{ LFDNoise3.ar(XLine.ar(1000, 100, 0.1) ! 3) }.plot(0.1);
::
]


