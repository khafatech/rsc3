#lang scribble/manual
@(require (for-label racket))

@title{PriorityQueue}
 Priority queue data structure@section{categories}
  Collections>Ordered

@section{description}

PriorityQueue implements a priority queue data structure, which is used to build schedulers. It allows you to put in items at some arbitrary time and pop them in
time order.

@section{INSTANCEMETHODS}
 
@section{private}
 prInternalArray


@section{method}
 put
Puts the item in the queue at the given time.

@section{method}
 topPriority
Returns the time of the earliest item in the queue.

@section{method}
 pop
Returns the earliest item in the queue.

@section{method}
 clear
Empty the queue.

@section{method}
 isEmpty
Return a link::Classes/Boolean:: whether the queue is empty.

@section{method}
 notEmpty
Return a link::Classes/Boolean:: whether the queue is not empty.

@section{method}
 removeValue
Remove all instances of value from the queue.


@section{EXAMPLES}
 


@racketblock[
(
var p;
p = PriorityQueue.new;

p.put(0.1, \a);
p.put(2.0, \b);
p.put(0.5, \c);
p.put(0.2, \d);
p.put(1.0, \e);

while ({ p.notEmpty },{
	[p.topPriority, p.pop].postln;
});


p.pop.postln;
p.pop.postln;
p.pop.postln;

)

[ 0.1, a ]
[ 0.2, d ]
[ 0.5, c ]
[ 1, e ]
[ 2, b ]
nil
nil
nil
::
]


