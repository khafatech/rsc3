#lang scribble/manual
@(require (for-label racket))

@title{SortedList}
a Collection whose items are kept in a sorted order.@section{categories}
 Collections>Ordered

@section{CLASSMETHODS}
 

@section{method}
 new
Creates a SortedList with the initial capacity given by strong::size:: and a comparison strong::function::.

@section{INSTANCEMETHODS}
 

@section{private}
 indexForInserting

@section{method}
 add
Adds an item in the SortedList at the correct position.

@racketblock[
SortedList[1, 2, 5, 6].add(4).postln;
::

]
@section{method}
 addAll
Adds all the items in the collection into the SortedList.

@racketblock[
SortedList[1, 2, 5, 6].addAll([0, 3, 4, 7]).postln;
::
]


