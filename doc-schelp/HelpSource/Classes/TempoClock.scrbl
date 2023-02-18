#lang scribble/manual
@(require (for-label racket))

@title{TempoClock}
@section{categories}
 Scheduling>Clocks
tempo based scheduler@section{related}
 Classes/AppClock, Classes/SystemClock

@section{description}


TempoClock is a scheduler like link::Classes/SystemClock::, but it schedules relative to a
strong::tempo:: in beats per second.

See link::Classes/Clock:: for general explanation of how clocks operate.

@section{CLASSMETHODS}
 

@section{private}
 initClass

@section{method}
 new
Creates a new instance of TempoClock.

@section{argument}
  tempo
The initial link::#-tempo#tempo::. Defaults to 
@racketblock[1::.

]
@section{argument}
  beats
The time in beats, corresponding to the reference time given with the 
@racketblock[seconds:: argument.
Defaults to ]

@racketblock[0::.

]
@section{argument}
  seconds
The reference time in seconds, to which the 
@racketblock[beats:: argument corresponds.
Defaults to the current Thread's logical time
(see link::Classes/Thread#-seconds::).

]
@section{argument}
  queueSize
The storage size of the scheduling queue. Each scheduled item takes 2 counts of space, so this size
divided by 2 gives the amount of items that can be scheduled at a time. See also link::#-queue::.

@section{discussion}
 
The TempoClock will be created strong::as if:: it started counting beats at the time given in the

@racketblock[seconds:: argument with the starting amount given in the ]

@racketblock[beats:: argument. The current
count of beats will thus be equal to that starting amount plus the amount of beats that would be
counted since the given reference time in seconds, according to the given tempo.

The default arguments create a TempoClock that starts counting beats with ]

@racketblock[0:: at the current
logical time.

]

@racketblock[
// Create a TempoClock that starts counting beats with 5 now.
(
t = TempoClock.new(2, 5);
"current beats:" + t.beats;
)

// Create a TempoClock, as if it started counting beats 5 seconds ago with 0.
(
t = TempoClock.new(2, 0, thisThread.seconds - 5);
"current beats:" + t.beats;
)
::

]
@section{method}
 default
Sets or gets the permanent default TempoClock instantiated at startup.

@racketblock[
TempoClock.default.beats // beats since default TempoClock was started
::

]
@section{subsection}
  Forwarding to the default instance

The following methods only forward to the link::#*default#default instance::, allowing you to use
the TempoClock class itself in place of 
@racketblock[TempoClock.default::.

]
@section{method}
 stop
@section{method}
 play
@section{method}
 sched
@section{method}
 schedAbs
@section{method}
 clear
@section{method}
 tempo
@section{method}
 etempo
@section{method}
  beats
@section{method}
  beats2secs
@section{method}
  secs2beats
@section{method}
  nextTimeOnGrid
@section{method}
  timeToNextBeat
@section{method}
  setTempoAtBeat
@section{method}
  setTempoAtSec
@section{method}
  setMeterAtBeat
@section{method}
  beatsPerBar
@section{method}
  baseBarBeat
@section{method}
  baseBar
@section{method}
  playNextBar
@section{method}
  beatDur
@section{method}
  elapsedBeats
@section{method}
  beats2bars
@section{method}
  bars2beats
@section{method}
  bar
@section{method}
  nextBar
@section{method}
  beatInBar

@section{INSTANCEMETHODS}
 

@section{private}
 prDump, prStart, prStop, prClear

@section{method}
 stop
Destroys the scheduler and releases the OS thread running the scheduler.

@section{method}
 clear
Removes all tasks from the scheduling queue.

@section{method}
 tempo
Sets or gets the current tempo in beats per second.

@racketblock[
t= TempoClock.new;
t.tempo_(2.0); // equivalent to t.tempo = 2.0;
t.tempo;
t.tempo_(72/60) // 72 beats per minute
t.tempo;
::

]
@section{method}
 permanent
Sets or gets a link::Classes/Boolean:: value indicating whether the clock will survive cmd-period. If false the clock is stopped (and thus removed) on cmd-period. If true the clock survives cmd-period. It is false by default.

@section{method}
 beats

Gets or sets the current logical time in beats according to this clock.

When strong::getting:: 
@racketblock[beats::, if this clock is the current Thread's
link::Classes/Thread#-clock#associated clock::, the Thread's own
link::Classes/Thread#-beats#time in beats:: is returned, otherwise the Thread's
link::Classes/Thread#-seconds#time in seconds:: converted to beats according to this
clock is returned.

After strong::changing:: ]

@racketblock[beats:: towards the strong::future::, the clock will
immediately perform all tasks scheduled until the new time. Likewise, when changing
]

@racketblock[beats:: towards the strong::past::, already scheduled tasks will be postponed, so
they will still be performed at the scheduled time in beats.

]
@section{note}
 
When changing 
@racketblock[beats::, you are only changing the clocks's notion of time, and not
the current Thread's link::Classes/Thread#-beats#logical time::, which will stay the
same until the Thread is called again. Hence, if this clock is the current Thread's
link::Classes/Thread#-clock#associated clock::, and you ask the clock for time in beats
just after changing it, you will see no effect. Nevertheless, the effect will be visible
immediately on a different Thread.
::

]

@racketblock[
(
t = TempoClock.new;

t.sched(3, {
    t.beats = 100;
    t.beats.postln; // still 3
    nil
});
)

(
c = TempoClock.new;
fork {
    loop {
		c.beats.postln; // updates, because ".wait" calls the thread
        1.wait;
    }
};
)

c.beats = 100;

::


]
@section{method}
 schedAbs

Schedules a task to be performed at a particular time in strong::beats::.

When the scheduling time is up, the task's 
@racketblock[awake:: method is called. If the method
returns a number, the task will be rescheduled for the time equal to the last scheduling
time plus the returned value.

See also: link::Classes/Clock#Scheduling::, link::Classes/Object#-awake::.


]
@section{method}
 sched

Schedules a task to be performed 
@racketblock[delta:: amount of strong::beats:: after the
current Thread's logical time. If this clock is the current Thread's
link::Classes/Thread#-clock#associated clock::, the Thread's
link::Classes/Thread#-beats#time in beats:: is used, otherwise the Thread's
link::Classes/Thread#-seconds#time in seconds:: is converted to beats according to this
clock's tempo and time of origin.

When the scheduling time is up, the task's ]

@racketblock[awake:: method is called. If the method
returns a number, the task will be rescheduled for the time equal to the last scheduling
time plus the returned value.

See also: link::Classes/Clock#Scheduling::, link::Classes/Object#-awake::.


]
@section{method}
 play

Plays task (a function) at the next beat, where strong::quant:: is 1 by default. Shortcut for link::#-schedAbs::; see link::#-seconds:: and link::#-nextTimeOnGrid:: for further details on time and quant.

@racketblock[
t= TempoClock.default;
t.play({arg beats, time, clock; [beats, time, clock].postln});
::

]
@section{method}
 playNextBar
Plays task (a function) at the next bar using link::#-schedAbs::.

@section{method}
 queue
Returns the scheduling queue Array in the form [beat, function]. The maximum number of items is determined by the clock's queueSize argument upon instantiation. The default queueSize of 256 allows 128 functions to be in the queue at any time.

@section{method}
 beatDur
Returns the duration in seconds of a current whole beat.

@section{method}
 beatsPerBar
Gets or sets the number of beats per bar. The default is 4. Setting must be done from within the scheduling thread, e.g.

@racketblock[
t= TempoClock.new;
t.schedAbs(t.nextBar, {t.beatsPerBar_(3)});
t.beatsPerBar;
::

]
@section{method}
 bar
Returns the current bar. See link::#-bars2beats:: for returning beat of current bar.

@section{method}
 nextBar
Returns the number of beats at the next bar line relative to the beat argument. If strong::beat:: is not supplied, returns the beat at which the next bar begins.

@section{method}
 beatInBar
Returns the current bar beat (as a link::Classes/Float::) in relation to link::#-beatsPerBar::. Values range from 0 to < beatsPerBar.

@section{method}
 baseBar
Returns bar at which link::#-beatsPerBar:: was last changed. If beatsPerBar has not been changed since the clock was created, returns 0.

@section{method}
 baseBarBeat
Returns beat at which the link::#-beatsPerBar:: was last changed. If beatsPerBar has not been changed since the clock was created, returns 0.

@section{method}
 beats2bars
Returns a bar as a float relative to link::#-baseBarBeat::.

@section{method}
 bars2beats
Returns a beat relative to link::#-baseBar::.

@racketblock[
t= TempoClock.default;
t.bars2beats(t.bar) // downbeat of the current bar
::

]
@section{method}
 timeToNextBeat
Returns the logical time to next beat. strong::quant:: is 1 by default, relative to baseBarBeat, see link::#-nextTimeOnGrid::.

@section{method}
 nextTimeOnGrid
With default values, returns the next whole beat. strong::quant:: is 1 by default, strong::phase:: is 0. quant is relative to link::#-baseBarBeat::, such that

@racketblock[
t= TempoClock.default;
t.nextTimeOnGrid(t.beatsPerBar) == t.nextBar // => true
::
Together strong::quant:: and strong::phase:: are useful for finding the next n beat in a bar, e.g. ]

@racketblock[nextTimeOnGrid(4, 2):: will return the next 3rd beat of a bar (of 4 beats), whereas ]

@racketblock[nextBar-2:: may return an elapsed beat.

]
@section{method}
 elapsedBeats
Returns the current elapsed time in beats. This is equivalent to 
@racketblock[tempoClock.secs2beats(Main.elapsedTime)::. It is often preferable to use link::#-beats:: instead of elapsedBeats because beats uses a thread's logical time.

]
@section{method}
 seconds
Returns the current elapsed time. (This method is inherited from link::Classes/Clock::.)

@section{method}
 beats2secs
Converts absolute strong::beats:: to absolute strong::seconds::, returning the elapsed time of the clock at the given strong::beats::. Only works for times in the current tempo. If the tempo changes any computed time in future will be wrong.

@racketblock[
t= TempoClock.default;
t.beats2secs(t.beats) // equivalent to t.seconds
t.beats2secs(0) // how many seconds after startup did beat 0 occur?
::

]
@section{method}
 secs2beats
Converts absolute strong::seconds:: to absolute beats. Only works for times in the current tempo. If the tempo changes any computed time in future will be wrong.

@section{EXAMPLES}
 


@racketblock[
t = TempoClock(1); // create a TempoClock

// schedule an event at next whole beat
t.schedAbs(t.beats.ceil, { arg beat, sec; [beat, sec].postln; 1 });

t.tempo = 2;
t.tempo = 4;
t.tempo = 0.5;
t.tempo = 1;

t.clear;

t.schedAbs(t.beats.ceil, { arg beat, sec; [beat, sec].postln; 1 });

t.stop;
::
]

@racketblock[
(
// get elapsed time, round up to next second
v = Main.elapsedTime.ceil;

// create two clocks in a 5:2 relation, starting at time v.
t = TempoClock(1, 0, v);
u = TempoClock(0.4, 0, v);

// start two functions at beat zero in each clock.
t.schedAbs(0, { arg beat, sec; [\t, beat, sec].postln; 1 });
u.schedAbs(0, { arg beat, sec; [\u, beat, sec].postln; 1 });
)

(
u.tempo = u.tempo * 3;
t.tempo = t.tempo * 3;
)

(
u.tempo = u.tempo * 1/4;
t.tempo = t.tempo * 1/4;
)

(
t.stop;
u.stop;
)
::
]

@racketblock[
(
// get elapsed time, round up to next second
v = Main.elapsedTime.ceil;

// create two clocks, starting at time v.
t = TempoClock(1, 0, v);
u = TempoClock(1, 0, v);

// start two functions at beat zero in each clock.
// t controls u's tempo. They should stay in sync.
t.schedAbs(0, { arg beat, sec; u.tempo = t.tempo * [1,2,3,4,5].choose; [\t, beat, sec].postln; 1 });
u.schedAbs(0, { arg beat, sec; [\u, beat, sec].postln; 1 });
)

(
u.tempo = u.tempo * 3;
t.tempo = t.tempo * 3;
)

(
u.tempo = u.tempo * 1/4;
t.tempo = t.tempo * 1/4;
)

(
t.stop;
u.stop;
)
::
]


