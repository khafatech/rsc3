#lang scribble/manual
@(require (for-label racket))

@title{List}
list of items of variable size@section{related}
 Classes/Array
@section{categories}
 Collections>Ordered

@section{description}

List is a subclass of SequenceableCollection with unlimited growth in size. Although not a subclass of link::Classes/Array:: or its superclass link::Classes/ArrayedCollection:: it uses an Array in its implementation and is in many cases interchangeable with one. (List implements many of the same methods as Array.)

Arrays have a fixed maximum size. If you add beyond that size a new Array is created and returned, but you must use an assignment statement or the new array will be lost. (See the link::Classes/Array:: helpfile.) List has no size limitation and is thus more flexible, but has slightly more overhead.

@racketblock[
(
x = Array.new(3);
y = List.new(3);
5.do({ arg i; z = x.add(i); y.add(i); });
x.postln; z.postln; y.postln;
)
::

Many of List's methods are inherited from link::Classes/SequenceableCollection:: or link::Classes/Collection:: and are documented in those helpfiles.

]
@section{CLASSMETHODS}
 

@section{method}
 new
Creates a List with the initial capacity given by strong::size::.

@section{method}
 newClear
Creates a List with the initial capacity given by strong::size:: and slots filled with nil.

@section{method}
 copyInstance
Creates a List by copying strong::a@section{List}
 's array variable.

@section{method}
 newUsing
Creates a List using strong::anArray::.

@section{INSTANCEMETHODS}
 

@section{method}
 asArray
Returns a new link::Classes/Array:: based upon this List.

@section{method}
 array
Returns the List's Array, allowing it to be manipulated directly. This should only be necessary for exotic manipulations not implemented in List or its superclasses.

@racketblock[
(
x = List[1, 2, 3];
x.array.add("foo");
x.postln;
)
::

]
@section{method}
 array
Sets the List's Array.

@section{method}
 at
Return the item at strong::index::.

@racketblock[
List[ 1, 2, 3 ].at(0).postln;
::

]
@section{method}
 clipAt
Same as link::#-at::, but values for strong::index:: greater than the size of the List will be clipped to the last index.

@racketblock[
y = List[ 1, 2, 3 ];
y.clipAt(13).postln;
::

]
@section{method}
 wrapAt
Same as link::#-at::, but values for strong::index:: greater than the size of the List will be wrapped around to 0.

@racketblock[
y = List[ 1, 2, 3 ];
y.wrapAt(3).postln; // this returns the value at index 0
y.wrapAt(4).postln; // this returns the value at index 1
::

]
@section{method}
 foldAt
Same as link::#-at::, but values for strong::index:: greater than the size of the List will be folded back.

@racketblock[
y = List[ 1, 2, 3 ];
y.foldAt(3).postln; // this returns the value at index 1
y.foldAt(4).postln; // this returns the value at index 0
y.foldAt(5).postln; // this returns the value at index 1
::

]
@section{method}
 put
Put strong::item:: at strong::index::, replacing what is there.

@section{method}
 clipPut
Same as link::#-put::, but values for strong::index:: greater than the size of the List will be clipped to the last index.

@section{method}
 wrapPut
Same as link::#-put::, but values for strong::index:: greater than the size of the List will be wrapped around to 0.

@section{method}
 foldPut
Same as link::#-put::, but values for strong::index:: greater than the size of the List will be folded back.

@section{method}
 add
Adds an strong::item:: to the end of the List.

@section{method}
 addFirst
Inserts the strong::item:: at the beginning of the List.

@section{method}
 insert
Inserts the strong::item:: into the contents of the List at the indicated strong::index::.

@section{method}
 pop
Remove and return the last element of the List.

@section{method}
 grow
Increase the size of the List by strong::sizeIncrease:: number of slots.

@section{method}
 removeAt
Remove and return the element at strong::index::, shrinking the size of the List.

@racketblock[
y = List[ 1, 2, 3 ];
y.removeAt(1);
y.postln;
::

]
@section{method}
 fill
Inserts the item into the contents of the receiver, possibly returning a new collection. @section{note}
 the difference between this and link::Classes/Collection#fill#Collection's *fill::.::

@racketblock[
(
var z;
z = List[1, 2, 3, 4];
z.fill(4).postln;
z.fill([1,2,3,4]).postln;
)
::

]
@section{method}
 do
Iterate over the elements in order, calling the function for each element. The function is passed two arguments, the element and an index.

@racketblock[
List['a', 'b', 'c'].do({ arg item, i; [i, item].postln; });
::

]
@section{method}
 reverseDo
Iterate over the elements in reverse order, calling the function for each element. The function is passed two arguments, the element and an index.

@racketblock[
List['a', 'b', 'c'].reverseDo({ arg item, i; [i, item].postln; });
::

]
@section{method}
 pairsDo
Calls function for each subsequent pair of elements in the List. The function is passed the two elements and an index.

@racketblock[
List[1, 2, 3, 4, 5, 6].pairsDo({ arg a, b; [a, b].postln; });
::

]
@section{method}
 copyRange
Return a new List which is a copy of the indexed slots of the receiver from start to end.

@racketblock[
(
var y, z;
z = List[1, 2, 3, 4, 5];
y = z.copyRange(1,3);
z.postln;
y.postln;
)
::

]
@section{method}
 copySeries
Return a new List consisting of the values starting at strong::first::, then every step of the distance between strong::first:: and strong::second::, up until strong::last::.

@racketblock[
(
var y, z;
z = List[1, 2, 3, 4, 5, 6];
y = z.copySeries(0, 2, 5);
y.postln;
)
::

]
@section{method}
 putSeries
Put strong::value:: at every index starting at strong::first::, then every step of the distance between strong::first:: and strong::second::, up until strong::last::.

@racketblock[
(
var y, z;
z = List[1, 2, 3, 4, 5, 6];
y = z.putSeries(0, 2, 5, "foo");
y.postln;
)
::

]
@section{method}
 reverse
Return a new List whose elements are reversed.

@racketblock[
(
var y, z;
z = List[1, 2, 3, 4];
y = z.reverse;
z.postln;
y.postln;
)
::

]
@section{method}
 scramble
Returns a new List whose elements have been scrambled. The receiver is unchanged.

@racketblock[
List[1, 2, 3, 4, 5, 6].scramble.postln;
::

]
@section{method}
 mirror
Return a new List which is the receiver made into a palindrome. The receiver is unchanged.

@racketblock[
List[1, 2, 3, 4].mirror.postln;
::

]
@section{method}
 mirror1
Return a new List which is the receiver made into a palindrome with the last element removed. This is useful if the list will be repeated cyclically, the first element will not get played twice. The receiver is unchanged.

@racketblock[
List[1, 2, 3, 4].mirror1.postln;
::

]
@section{method}
 mirror2
Return a new List which is the receiver concatenated with a reversal of itself. The center element is duplicated. The receiver is unchanged.

@racketblock[
List[1, 2, 3, 4].mirror2.postln;
::

]
@section{method}
 stutter
Return a new List whose elements are repeated strong::n:: times. The receiver is unchanged.

@racketblock[
List[1, 2, 3].stutter(2).postln;
::

rotate
Return a new List whose elements are in rotated order. Negative strong::n:: values rotate left, positive strong::n:: values rotate right. The receiver is unchanged.
]

@racketblock[
List[1, 2, 3, 4, 5].rotate(1).postln;
List[1, 2, 3, 4, 5].rotate(-1).postln;
List[1, 2, 3, 4, 5].rotate(3).postln;
::

]
@section{method}
 pyramid
Return a new List whose elements have been reordered via one of 10 "counting" algorithms. The algorithms are numbered 1 through 10. Run the examples to see the algorithms.

@racketblock[
List[1, 2, 3, 4].pyramid(1).postln;

(
10.do({ arg i;
	List[1, 2, 3, 4].pyramid(i + 1).postcs;
});
)
::

]
@section{method}
 lace
Returns a new List whose elements are interlaced sequences of the elements of the receiver's subcollections, up to size strong::length::. The receiver is unchanged.

@racketblock[
(
x = List[ [1, 2, 3], 6, List["foo", 'bar']];
y = x.lace(12);
x.postln;
y.postln;
)
::

]
@section{method}
 permute
Returns a new List whose elements are the strong::nthPermutation:: of the elements of the receiver. The receiver is unchanged.

@racketblock[
(
x = List[ 1, 2, 3];
6.do({|i| x.permute(i).postln;});
)
::

]
@section{method}
 wrapExtend
Returns a new List whose elements are repeated sequences of the receiver, up to size strong::length::. The receiver is unchanged.

@racketblock[
(
x = List[ 1, 2, 3, "foo", 'bar' ];
y = x.wrapExtend(9);
x.postln;
y.postln;
)
::

]
@section{method}
 foldExtend
Same as link::#-wrapExtend:: but the sequences fold back on the list elements.

@racketblock[
(
x = List[ 1, 2, "foo"];
y = x.foldExtend(9);
x.postln;
y.postln;
)
::

]
@section{method}
 slide
Return a new List whose elements are repeated subsequences from the receiver. Easier to demonstrate than explain.

@racketblock[
List[1, 2, 3, 4, 5, 6].slide(3, 1).postcs;
List[1, 2, 3, 4, 5, 6].slide(3, 2).postcs;
List[1, 2, 3, 4, 5, 6].slide(4, 1).postcs;
::

]
@section{method}
 dump
Dump the List's Array.

@section{method}
 clear
Replace the List's Array with a new empty one.


