#lang scribble/manual
@(require (for-label racket))

@title{Thread}
The context of code evaluation@section{categories}
 Core>Kernel
@section{related}
  Classes/Routine

@section{description}


@section{note}
  A SuperCollider Thread is strong::not an operating system thread::. Although they
have some conceptual similarities, they do not correspond.
::

A Thread represents the strong::context:: within which code runs. It is also said that
code runs "emphasis::on:: a Thread". A Thread records the strong::state:: of code
execution, and thus provides support for code to be suspended at any time, and then
resumed where it left off. It is then said that the Thread itself is strong::suspended and
resumed::.

There is always one link::Classes/Process#-mainThread#main Thread:: belonging to the
link::Classes/Process:: - it is the Thread on which the top-level code runs. Another
Thread may be started using an instance of the Thread's subclass link::Classes/Routine::
 which will run a link::Classes/Function:: in the context of its own (there is no use in
instantiating the Thread class itself).

When code on a Thread starts or resumes another Thread (Routine), the former Thread
becomes the latter's strong::parent::, and the latter its strong::child::. The parent
Thread's execution is strong::blocked:: until the child Thread finishes or is suspended,
at which point the parent Thread continues execution. The strong::current:: Thread may be
accessed using link::#.thisThread#thisThread::, while a Thread's parent may be accessed
using link::#-parent::.

A Thread has
@section{list}
 
## associated link::#-beats#logical time::
## an associated link::#-clock#Clock::
## own link::#-randSeed#random number seed::
## own link::#-exceptionHandler#exception handler::
::

@section{method}
  thisThread

The global pseudo-variable 
@racketblock[thisThread:: always represents the current Thread, i.e.
the context in which the current code is running. It can be either the
link::Classes/Process#-mainThread#main Thread:: or the link::Classes/Routine:: running the
current code.

See also: link::Classes/Clock#Scheduling and Threads::.

]

@racketblock[
// example
thisThread.beats;
thisThread.seconds;
thisThread.clock;
::

]
@section{classMethods}
 

@section{method}
 new

Creates an instance of Thread, passing it the Function with code to run.

@section{note}
  There is no good use in instantiating a Thread, because this class offers no method
of starting the given Function. Instead, use the Thread's subclass
link::Classes/Routine::. The only purpose of this constructor is for Routine to call it
within its own constructor.
::


@section{argument}
 func
A Function with code for the Thread to run.

@section{argument}
 stackSize
Call stack size (an Integer).

@section{instanceMethods}
 

@section{method}
  parent
The parent Thread that started or resumed this Thread.

@section{discussion}
 

The parent Thread's execution is blocked until the child Thread finishes or is suspended.

When a child Thread is started or resumed, it inherits certain aspects from its parent:
@section{list}
 
## associated link::#-clock#Clock::
## associated link::#-beats#logical time::
::


@section{method}
 beats
Get or set the current logical time of the Thread in beats. This will be the same
as the time in seconds, unless this Thread's link::#-clock#clock:: is
link::Classes/TempoClock::, and the clock's link::Classes/TempoClock#-tempo#tempo::
 is other than 
@racketblock[1::.

Setting ]

@racketblock[beats:: also sets link::#-seconds:: to
]

@racketblock[ thisThread.clock.beats2secs(beats) ::.

]
@section{discussion}
 

There are several sources of logical time:
@section{list}
 
## When code is run from the code editor, the command line, or in response to OSC
and MIDI messages, the link::Classes/Process#-mainThread#main Thread::'s logical time is
set to the current strong::physical time:: (see link::Classes/Process#*elapsedTime::).

## When code strong::scheduled:: on a link::Classes/Clock:: is run, the
link::Classes/Process#-mainThread#main Thread::'s logical time is set to the time the code
was scheduled for.

## Child Threads strong::inherit:: logical time from their link::#-parent#parents::
 - whenever a Thread (Routine) is started or resumed, its logical time is set to
that of the parent Thread.
::

However, a Thread's logical time may also be set strong::manually:: (using this
method or link::#-seconds::). It may be useful to change the strong::current:: Thread's
time in order to manipulate behavior of streams that use the current logical time for
their operation (e.g. streams created by link::Classes/Pstep:: and
link::Classes/Pseg:: patterns). This will affect all code running within the current
Thread, as well as any child Threads, due to logical time inheritance. Note however
that changing strong::another:: Thread's time will have no effect, because the time
will be overridden by inheritance as soon as the Thread is run; likewise, any changes
to the current Thread's time only have effect until the Thread is suspended
(it link::Classes/Object#-yield#yields::) and resumed again.

See also: link::Classes/Clock#Scheduling and Threads::.

@section{method}
 seconds
Get or set the current logical time of the Thread in seconds.

Setting 
@racketblock[seconds:: also sets link::#-beats:: to
]

@racketblock[thisThread.clock.secs2beats(seconds)::.

See link::#-beats:: for general discussion on Threads and logical time.

]
@section{method}
 clock
Get or set the Thread's associated link::Classes/Clock::.

@section{discussion}
 

There are several ways a Clock becomes associated with a Thread:
@section{list}
 
## When code is run from the code editor, the command line, or in response to OSC and
MIDI messages, the link::Classes/Process#-mainThread#main Thread::'s clock is set to
link::Classes/SystemClock::.

## When code strong::scheduled:: on a Clock is run, that clock becomes the
link::Classes/Process#-mainThread#main Thread::'s clock.

## Child Threads strong::inherit:: the associated clock from their
link::#-parent#parents::.
::

A Thread's associated clock may also be set strong::manually:: using this method. Setting
the strong::current:: Thread's clock is useful to manipulate further behavior of the
Thread or its child Threads, but the clock will be reset the next time the Thread
is resumed, due to clock inheritance. For the same reason, setting
strong::another:: Thread's clock will have no effect on code running on it.

See also: link::Classes/Clock#Scheduling and Threads::.

@section{method}
 isPlaying
@section{Returns}
  true if it is playing.

@section{method}
 state

The internal state values for a Thread instance can be polled:
@section{table}
 
## 0 || not started
## 7 || running
## 8 || stopped
::

@section{subsection}
 Seeding the random number generator

see also: link::Reference/randomSeed::

@section{method}
 randSeed
Set the random number generator seed using a single integer.
@section{discussion}
 
Example:

@racketblock[
g = thisThread.randSeed = 4;
10.do{1.0.rand2.postln};
::

]
@section{method}
 randData

Get or set the three integer array which defines the internal basis for the random number generator.  You can use this to get back the exact same random number sequence, and it provides a mechanism for automatic replay for generative music.
@section{discussion}
 
Example:

@racketblock[
g = thisThread.randData;
10.do{1.0.rand2.postln};
::
]

@racketblock[
// each time the seed is reset, the random number generation should give the same sequence
thisThread.randData_(Int32Array[ -662787342, 1546785953, 1661466823 ]);
10.do{1.0.rand2.postln};
::

]


