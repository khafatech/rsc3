#lang scribble/manual
@(require (for-label racket))

@title{Task}
@section{categories}
 Scheduling
a pauseable process@section{related}
 Classes/Routine

@section{description}

Task is a pauseable process. It is implemented by wrapping a link::Classes/PauseStream:: around a link::Classes/Routine::. Most of its methods (start, stop, reset) are inherited from PauseStream.

Tasks are not 100% interchangeable with Routines.

@section{list}
 
## Condition does not work properly inside of a Task.
## Stopping a task and restarting it quickly may yield surprising results (see example below), but this is necessary to prevent tasks from becoming unstable if they are started and/or stopped in rapid succession.
::

@section{CLASSMETHODS}
 

@section{method}
 new
@section{argument}
 func
A Function to be evaluated.
@section{argument}
 clock
A Clock in which to play the link::Classes/Routine::. If you do not provide a Clock the default is an instance of link::Classes/TempoClock::. Remember that methods which call Cocoa primitives (i.e. GUI functions) must be played in link::Classes/AppClock::.

@section{INSTANCEMETHODS}
 

@section{method}
 play
@section{argument}
 argClock
(optional) Override the clock assigned in Task.new.
@section{argument}
 doReset
If true, the task will start over from the beginning. Default is false (task will resume where it was when it was last stopped).
@section{argument}
 quant
See the link::Classes/Quant:: helpfile.


@section{subsection}
 Other control methods

@section{method}
 start
Restart the task from the beginning.

@section{method}
 resume
Resume the task where it left off.

@section{method}
 pause
Stop playing now.

@section{method}
 stop
Stop playing now. (Pause and stop have the same implementation.)

@section{method}
 reset
Set the stream to restart from the beginning the next time it's played.

@section{subsection}
 Notifications

Other objects might need to be aware of changes in the state of a task. The following notifications are broadcast to dependents registered with the Task object.

@section{list}
 
## strong::\userPlayed:: - Sent at the time the user calls play, start or resume.
## strong::\playing:: - Sent at the time the task begins playing on the clock (corresponding to quant).
## strong::\userStopped:: - Sent at the time the user calls pause or stop.
## strong::\stopped:: - Sent at the time the task is finally removed from the clock (this is the time when the next event would have occurred if the task had not been stopped). If the task function completes on its own, this notification is sent without 'userStopped' being sent previously.
::

@section{EXAMPLES}
 

@section{subsection}
 What happens if you stop and start the task too quickly?

@racketblock[
(
t = Task({
	50.do({ arg i;
		i.squared.postln;
		0.5.wait;
	});
});
)

t.start;
t.pause;
t.resume;
t.reset;
t.stop;

// unexpected behavior here
(
t = Task({
	["go", thisThread.clock.beats].postln;
	inf.do({ arg i;
		2.wait;
		[ "wake up", i ].postln;
	});
});

fork {
	t.start;
	0.1.wait;
	t.stop;
	0.1.wait;
	t.start;
	6.wait;
	t.stop;
};
)

[ go, 1702.114411906 ]
[ go, 1704.114411906 ]
::

Based on the forked thread, you would expect the second "go" line of output to occur 0.2 seconds after the first, but in fact it happens two seconds later (the same amount of time the task waits between iterations). This is because the task must not schedule itself on the clock more than once. When the task is stopped, it remains scheduled until it wakes up again (based on its wait time). If, during this interval, the task were restarted, there would be two references to the task in the scheduler queue -- a situation that is irrecoverable short of stopping everything with command-period.

As a result, Task should be used for processes that need to start and stop relatively infrequently, but for which maximum stability is required. If you need fine-grained control over when and how the process stops and resumes (as is the case, for instance, with condition), link::Classes/Routine:: is preferred.
]


