#lang scribble/manual
@(require (for-label racket))

@title{AppClock}
@section{categories}
 Scheduling>Clocks
Clock running on main application thread@section{related}
 Classes/SystemClock, Classes/TempoClock

@section{description}

SystemClock is more accurate, but cannot call GUI primitives.

You will need to use the link::Classes/SystemClock:: to get accurate/musical scheduling.

See link::Classes/Clock:: for general explanation of how clocks operate.

@section{CLASSMETHODS}
 

@section{private}
 initClass, prSchedNotify

@section{method}
 sched
The float you return specifies the delta to resched the function for. Returning nil will stop the task from being rescheduled.

@racketblock[
(
AppClock.sched(0.0,{ arg time;
	["AppClock has been playing for ",time].postln;
	rrand(0.1,0.9);
});
)
::
]

@racketblock[
(
AppClock.sched(2.0,{
	"2.0 seconds later".postln;
	nil;
});
)
::

]
@section{method}
 clear
Clear the AppClock's scheduler to stop it.

@racketblock[
AppClock.clear;
::

]
@section{method}
 play
The link::Classes/Routine:: (or link::Classes/Task::) yields a float value indicating the delta (secs) for the AppClock to wait until resuming the Routine.

@racketblock[
(
var w, r;
w = Window.new("trem", Rect(512, 256, 360, 130));
w.front;
r = Routine({ arg appClockTime;
	["AppClock has been playing for secs:",appClockTime].postln;
	60.do({ arg i;
		0.05.yield;
		w.bounds = w.bounds.moveBy(10.rand2, 10.rand2);
		w.alpha = cos(i*0.1pi)*0.5+0.5;
	});
	1.yield;
	w.close;
});
AppClock.play(r);
)
::

]
@section{method}
 tick
AppClock.tick is called periodically by the SuperCollider language interpreter. This updates the link::Classes/Scheduler:: and causes any scheduled tasks to be executed. You should never call this method yourself.

