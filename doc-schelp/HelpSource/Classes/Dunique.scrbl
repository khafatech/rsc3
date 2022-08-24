#lang scribble/manual
@(require (for-label racket))

@title{Dunique}
 Return the same unique series of values for several demand streams@section{categories}
  UGens>Demand
@section{related}
  Classes/Demand, Classes/Drand, Classes/Dser, Classes/Duty, Classes/Dxrand, Classes/TDuty

@section{description}

A demand UGen represents a single series of states. When used by more than one demand stream, it will output only every nth value for each. Wrapping a demand UGen in a Dunique will guarantee that that all streams receive the same value.


@racketblock[
// without any measures, a demand ugen's values will be distributed between several series:
{ var x = Dseq([0, 1, 2, 3, 4, 5], inf); { Demand.ar(Impulse.ar(1/rrand(0.001, 0.0015)), 0, x) } ! 3 }.plot;

// using a Dunique, each series iterates through the same values
{ var x = Dunique(Dseq([0, 1, 2, 3, 4, 5], inf)); { Demand.ar(Impulse.ar(1/rrand(0.001, 0.0015)), 0, x) } ! 3 }.plot;

// random values will also be identical
{ var x = Dunique(Drand([0, 1, 2, 3, 4, 5], inf)); { Demand.ar(Impulse.ar(1/rrand(0.001, 0.0015)), 0, x) } ! 3 }.plot;
::

]
@section{CLASSMETHODS}
 

@section{METHOD}
  new
Return a new instance.

@section{ARGUMENT}
  source
The demand ugen that is to be reused in several others.

@section{ARGUMENT}
  protected
There are limitations to this ugen: If one copy is called much faster than the slowest, the buffer can overrun. Trying to protect from such a buffer overrun, one has to rely on counting up to infinity. Using 32bit float, only 16777216 events can be correctly played back. When protected is true, these two limitations are caught by ending the series (default: true). Set this parameter to false (or zero) in order to ignore this (e.g. by adjusting buffer size appropriately).

@racketblock[
// to demonstrate, make the buffer deliberately small:
{ var x = Dunique(Drand([0, 1, 2, 3, 4, 5], inf), 20); { Duty.ar(0.5e-4 + 0.001.rand, 0, x, doneAction: Done.freeSelf) } ! 3}.plot;
::
]


