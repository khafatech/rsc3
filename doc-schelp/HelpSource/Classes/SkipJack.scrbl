#lang scribble/manual
@(require (for-label racket))

@title{SkipJack}
 A utility for background tasks that survive cmd-period@section{categories}
  Scheduling, Libraries>JITLib>GUI
@section{related}
  Classes/CmdPeriod

@section{description}

SkipJack is a utility to run a function in the background repeatedly, that survive cmd-period.

A typical use is with a window displaying the state of some objects every now and then. (This is better in some cases than
updating the GUI at every change. If the changes happen fast, you don't choke your CPU on gui updating.)

But SkipJack is useful whenever you need a periodic function to run in the background and not go away if the user hits cmd-period.

@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new

@section{argument}
 updateFunc
A link::Classes/Function:: to repeat in the background.

@section{argument}
 dt
The time interval at which to repeat. It can also be a stream or a function that returns a number.

@section{argument}
 stopTest
A test whether to stop the task now. Usually a Function.

@section{argument}
 name
A name for this skipjack. Used for posting information and in the link::#*stop:: classmethod.

@section{argument}
 clock
The clock that plays the task. Default is link::Classes/AppClock::, so SkipJack can call GUI primitives. If you need more precise timing, you can supply your own clock, and use defer only where necessary.

@section{argument}
 autostart
When true (default) SkipJack starts automatically as it is created.

@section{method}
  stop
Stop a skipjack by name.

@section{method}
  stopAll
Stop all skipjacks.

@section{method}
  defaultClock
The default clock (AppClock)

@section{method}
  verbose
When true, SkipJack posts messages when it starts, stops or restarts.

@section{method}
  all
The global set of all skipjacks.

@section{Instancemethods}
 
@section{private}
  cmdPeriod

@section{method}
  dt
Get or set the time interval.

@section{method}
  task
The internal Routine that wraps updateFunc.

@section{method}
  name
The name of this skipjack.

@section{method}
  stopTest
The current stopTest. (see argument in link::#*new:: )

@section{method}
  start
Start this skipjack.

@section{method}
  play
Same as 
@racketblock[start::

]
@section{method}
  stop
Stop this skipjack.

@section{method}
  clock
Get or set the clock used. This will only be updated when the skipjack restarts.

@section{method}
  updateFunc
The updateFunc set by the argument to link::#*new::

@section{Examples}
 
Simple example:

@racketblock[
w = SkipJack({ "watch...".postln; }, 0.5, name: "test");
SkipJack.verbose = true;    // post stop/wakeup logs

w.stop;
w.start;

// 	now try to stop with cmd-. : SkipJack always restarts itself.
thisProcess.stop;

w.stop;
::

Using stopTest:
]

@racketblock[
a = 5;
w = SkipJack({ "watch...".postln; }, 0.5, { a == 10 }, "test");
a = 10;	// fulfill stopTest
::

Typical use: SkipJack updates a window displaying the state of some objects every now and then.
]

@racketblock[
(
d = (a: 12, b: 24);
d.win = Window("dict", Rect(0,0,200,60)).front;
d.views = [\a, \b].collect { |name, i|
    StaticText(d.win, Rect(i * 100,0,96,20))
        .background_(Color.yellow).align_(0).string_(name);
};
w = SkipJack({
        "...".postln;
        [\a, \b].do { |name, i|
            d.views[i].string_(name ++ ":" + d[name])
        }
    },
    0.5,
    { d.win.isClosed },
    "showdict"
);
)

d.a = 123;      // updates should be displayed
d.b = \otto;
d.win.close;    // when window closes, SkipJack stops.
::

If you need to get rid of an unreachable skipjack:
]

@racketblock[
SkipJack({ "unreachable, unkillable...".postln }, name: "jack");

SkipJack.stopAll        // do this to stop all;

SkipJack.stop("jack");  // reach it by name and stop
::

]


