#lang scribble/manual
@(require (for-label racket))

@title{LinkedList}
doubly linked list@section{categories}
  Collections>Ordered

@section{description}

LinkedList implements a doubly linked list.

Most methods are inherited from the superclasses.

@section{INSTANCEMETHODS}
 

@section{method}
 addFirst
Add an item to the head of the list.

@section{method}
 add
Add an item to the tail of the list.

@section{method}
 remove
Remove an item from the list.

@section{method}
 pop
Remove and return the last item in the list.

@section{method}
 popFirst
Remove and return the first item in the list.

@section{method}
 first
Return the first item in the list.

@section{method}
 last
Return the last item in the list.

@section{method}
 at
Return the item at the given index in the list.
This requires a scan of the list and so is O(n).

@section{method}
 put
Put the item at the given index in the list.
This requires a scan of the list and so is O(n).

@section{method}
 removeAt
Remove and return the item at the given index in the list.
This requires a scan of the list and so is O(n).


