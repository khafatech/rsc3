#lang scribble/manual
@(require (for-label racket))

@title{SystemClock}
@section{categories}
 Scheduling>Clocks
 Clock running on separate accurately timed thread@section{related}
 Classes/AppClock, Classes/TempoClock

@section{description}


SystemClock is more accurate than AppClock, but cannot call GUI primitives.

See link::Classes/Clock:: for general explanation of how clocks operate.

@section{CLASSMETHODS}
 

@section{private}
 prClear

@section{method}
 sched
The float you return specifies the delta to resched the function for. Returning nil will stop the task from being rescheduled.

@racketblock[
(
SystemClock.sched(0.0,{ arg time;
	time.postln;
	rrand(0.1,0.9);
});
)
::
]

@racketblock[
(
SystemClock.sched(2.0,{
	"2.0 seconds later".postln;
	nil;
});
)
::

]
@section{method}
 clear
Clear the SystemClock's scheduler to stop it.

@racketblock[
SystemClock.clear;
::

]
@section{method}
 schedAbs

@racketblock[
(
SystemClock.schedAbs( (thisThread.seconds + 4.0).round(1.0),{ arg time;
	("the time is exactly " ++ time.asString
		++ " seconds since starting SuperCollider").postln;
});
)
::

]
@section{method}
 play
Calls to the GUI may not be made directly from actions triggered by SystemClock or incoming socket messages (OSCFunc).

To get around this, use 
@racketblock[{ }.defer ::. This will execute the function using the AppClock and is equivalent to ]

@racketblock[AppClock.sched(0, function)::

]

@racketblock[
(
var w, r;
w = Window.new("trem", Rect(512, 256, 360, 130));
w.front;
r = Routine({ arg time;
	60.do({ arg i;
		0.05.yield;
		{
			w.bounds = w.bounds.moveBy(10.rand2, 10.rand2);
			w.alpha = cos(i*0.1pi)*0.5+0.5;
		}.defer;
	});
	1.yield;
	w.close;
});
SystemClock.play(r);
)
::
This example is only to show how to make calls to Cocoa/GUI when scheduling with the SystemClock. If you only wish to control the GUI, use AppClock.
]


