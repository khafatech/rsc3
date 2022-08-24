#lang scribble/manual
@(require (for-label racket))

@title{Scheduler}
@section{categories}
 Scheduling
schedules functions to be evaluated in the future
@section{description}

A Scheduler can be used to schedule and reschedule functions to be evaluated at a specific time-point. The Scheduler's time needs to be advanced manually. In most cases you will probably want to use a Clock (e.g. link::Classes/TempoClock::, link::Classes/SystemClock::, link::Classes/AppClock::) instead, in which the march of time is handled for you.

@section{CLASSMETHODS}
 

@section{method}
 new
@section{argument}
 clock
A clock, like SystemClock.
@section{argument}
 drift
If 
@racketblock[true::, link::#-sched:: will schedule tasks relative to the current absolute time ( link::Classes/Process#*elapsedTime#Main.elapsedTime:: ), otherwise to the current logical time of the scheduler ( link::#-seconds:: ).
]
@section{argument}
 recursive
Sets link::#-recursive::.

@section{INSTANCEMETHODS}
 

@section{method}
 play
Schedules the task immediately. Equivalent to 
@racketblock[sched(0, task)::.

]
@section{method}
 sched
Schedule the task at 
@racketblock[delta:: seconds relative to the current time, as defined by the ]

@racketblock[drift:: argument of the link::#*new#constructor::.

Regardless of what time a task is scheduled, it will only be awaken the next time link::#-seconds:: is set.

]
@section{method}
 schedAbs
Schedule the task at absolute 
@racketblock[time:: in seconds.

]
@section{method}
 advance
Advance the current logical time by 
@racketblock[delta:: seconds. Has same effect as setting link::#-seconds::.

]
@section{method}
 seconds
The current logical time of the scheduler.

Setting a new time will wake up (evaluate) any tasks scheduled within that time; a task that returns a new time will be rescheduled accordingly.

@section{method}
 isEmpty
Returns whether the scheduling queue is empty.

@section{method}
 clear
Clears the scheduling queue

@section{method}
 queue
@section{returns}
  The instance of link::Classes/PriorityQueue:: used internally as scheduling queue.

@section{method}
 recursive
If waking up items results in new items being scheduled, but some of them are already expired (scheduled at current time or earlier), this variable determines whether those items will be awaken as well in the same call to -seconds.

@section{EXAMPLES}
 


@racketblock[
a = Scheduler(SystemClock);

a.sched(3, { "now it is 3 seconds.".postln; nil });
a.sched(5, { "now it is 5 seconds.".postln; nil });
a.sched(1, { "now it is 1 second.".postln; nil });

a.advance(0.5);
a.advance(0.5);
a.advance(2);
a.advance(2);

// the beats, seconds and clock are passed into the task function:
a.sched(1, { arg beats, secs, clock; [beats, secs, clock].postln });
a.advance(1);

// the scheduling is relative to "now":
a.sched(3, { "now it was 3 seconds.".postln });
a.sched(5, { "now it was 5 seconds.".postln });
a.sched(1, { "now it was 1 second.".postln });

a.advance(0.5);
a.advance(0.5);
a.advance(2);
a.advance(2);

// play a Routine or a task:
a.play(Routine { 5.do { arg i; i.postln; 1.yield } });
a.advance(0.9);
::

]

@racketblock[
// scheduling tasks
(
x = Scheduler(TempoClock.default);

Task {
	inf.do { |i|
		("next " ++ i ++ " in task." + Main.elapsedTime).postln;
		0.5.wait;
	}
}.play(x);
)

x.advance(0.1);
x.seconds;
x.advance(5);
x.seconds;

(
Routine {
	loop { x.advance(0.1); 0.1.wait };
}.play;
)

(
Task { 5.do {
	x.advance(1);
	2.0.rand.wait;
	}
}.play;
)

x.advance(8.1);

Pbind(\degree, Pseries(0, 2, 8), \dur, 0.25).play(x);

(
Task { 5.do {
	x.advance(0.20);
	1.0.wait;
	}
}.play;
)
::
]


