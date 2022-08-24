#lang scribble/manual
@(require (for-label racket))

@title{Clock}
@section{categories}
 Scheduling>Clocks
abstract superclass for clocks@section{related}
 Classes/AppClock, Classes/SystemClock, Classes/TempoClock

@section{description}

Clock is an abstract class: it only defines an abstract set of methods that
all clocks should implement. See its subclasses: link::Classes/SystemClock::,
link::Classes/TempoClock::, link::Classes/AppClock:: for specific
implementations.

@section{subsection}
  Scheduling

A Clock keeps track of time and allows strong::tasks:: to be
strong::scheduled:: for some time in the future (e.g. using
link::Classes/TempoClock#-sched#sched::,
link::Classes/TempoClock#-schedAbs#schedAbs:: or
link::Classes/TempoClock#-play#play:: methods).
A task can be any link::Classes/Object::. When the time at which a task was scheduled is up, the task is emphasis::awoken::, i.e. its
link::Classes/Object#-awake#awake:: method is evaluated. If the value returned by
this method is a number, the task is automatically strong::rescheduled:: for the
time equal to its last scheduled time plus the return value (in
link::Classes/TempoClock#-beats#beats::).

@section{subsection}
  Useful Tasks

Objects of different classes may do different things in response to being
scheduled on a clock by having own implementation of the 
@racketblock[awake:: method.
The link::Classes/Object#-awake:: method that all classes inherit simply
calls the same object's link::Classes/Object#-next#next:: method, forwarding the
]

@racketblock[beats:: argument as well as the return value, so subclasses may implement
either one to equivalent effect, as far as clock scheduling is concerned.
]
@section{footnote}
  However, note that the 
@racketblock[next:: method is also involved in the
concept of link::Tutorials/Streams-Patterns-Events1.html#Streams#streams::.::

Examples of useful objects to be scheduled on clocks:
]
@section{list}
 
## link::Classes/Function#-awake:: method is implemented so as to call
the function's own link::Classes/Function#-value:: method, effectively running
the code within the function.
## link::Classes/Routine#-awake:: calls own link::Classes/Routine#-next::, in
turn starting or resuming the Routine's Function.
## Some subclasses of link::Classes/Stream:: will have its 
@racketblock[next:: method
do something useful aside from returning a new value in a stream.
::

]
@section{subsection}
  Scheduling and Threads

Whenever a task is awaken, its 
@racketblock[awake:: method is called in the context of
the link::Classes/Process#-mainThread#main thread::. Just before that, the main
thread's link::Classes/Thread#-beats#logical time:: is set to the scheduling
time of the awaken task, and its link::Classes/Thread#-clock#clock:: is set to
the scheduling clock. Note however that if the task is a link::Classes/Routine::
 it will then immediately start or resume its Function, setting itself as the
link::Classes/Thread#.thisThread#current thread::.

]


