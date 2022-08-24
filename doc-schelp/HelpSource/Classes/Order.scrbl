#lang scribble/manual
@(require (for-label racket))

@title{Order}
an order of elements with a numerical index@section{related}
 Classes/SparseArray
@section{categories}
  Collections>Ordered

@section{description}

Keeps elements in an order and allows to put them at arbitrary slots
without having to allocate a large array.

@section{note}
 
link::#-put:: and link::#-at:: are slower than in link::Classes/IdentityDictionary:: / link::Classes/PriorityQueue::, link::#-do:: is faster.
::

@section{CLASSMETHODS}
 

@section{method}
 new
Create a new order.

@racketblock[
g = Order.new;
g.put(7, 100); // put a value (100) at index 7
g.clear; // empty
::

]
@section{method}
 newFromIndices
Create a new order from given items and indices.

@section{INSTANCEMETHODS}
 

@section{private}
 resetIndices, nextSlotFor, slotFor, prPutSlot

@section{method}
 doRange
Iterate over a range of the order's items.

@section{method}
 pos
Return the current write position.

@section{EXAMPLES}
 


@racketblock[
a = Order.new;

a[0] = \z;
a[0] = \y;
a[5] = \five;
a[4] = \four;

a[0] = \z;
a[5] = \five;
a[4] = \four;

a.indices;

a[9] = 100;
a.indices;
::
]


