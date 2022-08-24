#lang scribble/manual
@(require (for-label racket))

@title{Array}
 fixed size collection@section{related}
  Reference/Literals, Classes/List
@section{categories}
  Collections>Ordered

@section{description}

Arrays are ArrayedCollections whose slots may contain any object. Arrays have a fixed maximum size beyond which they cannot grow. For expandable arrays, use the link::Classes/@section{List}
  class.

strong::Literal Arrays:: can be created at compile time, and are very efficient. See link::Reference/Literals:: for information.

For handling strong::multidimensional arrays::, there are specific methods which are covered in the helpfile link::Guides/J-concepts-in-SC::.

@section{note}
 
For Arrays, the 
@racketblock[add:: method may or may not return the same Array object. It will add the argument to the receiver if there is space, otherwise it returns a new Array object with the argument added. Thus the proper usage of ]

@racketblock[add:: with an Array is to always assign the result as follows:
]

@racketblock[
    z = z.add(obj);
::
This allows an efficient use of resources, only growing the array when it needs to. The link::Classes/]
@section{List}
  class manages the Array internally, and in many cases is more suitable.
::

Elements can be put into an existing slot with 
@racketblock[a.put(2,obj):: and accessed with
]

@racketblock[a.at(2):: or ]

@racketblock[a[2]::

See link::Classes/ArrayedCollection:: for the principal methods: at, put, clipAt, wrapAt, etc...


]
@section{ClassMethods}
 

@section{method}
 new
Create a new array with size 0 that can grow up to the fixed size.
@section{argument}
 maxSize
The maximum size of the array.

@section{method}
 newClear
Create a new array with all slots filled with nils.
@section{argument}
 indexedSize
The size of the array.

@section{method}
 with
Create a new Array whose slots are filled with the given arguments.
This is the same as the method in ArrayedCollection, but is reimplemented here to be more efficient.

@racketblock[
Array.with(7, 'eight',  9).postln;
::

]
@section{copymethod}
  Collection *fill

@section{copymethod}
  Collection *fill2D

@section{copymethod}
  Collection *fillND

@section{copymethod}
  Collection *newFrom

@section{copymethod}
  ArrayedCollection *geom

@section{copymethod}
  ArrayedCollection *series

@section{copymethod}
  ArrayedCollection *iota

@section{copymethod}
  SequenceableCollection *interpolation

@section{copymethod}
  SequenceableCollection *rand

@section{copymethod}
  SequenceableCollection *rand2

@section{copymethod}
  SequenceableCollection *linrand

@section{copymethod}
  SequenceableCollection *exprand

@section{copymethod}
  SequenceableCollection *fib





@section{InstanceMethods}
 

@section{copymethod}
  ArrayedCollection -at

@section{copymethod}
  ArrayedCollection -put

@section{copymethod}
  ArrayedCollection -insert

@section{copymethod}
  ArrayedCollection -clipAt

@section{copymethod}
  ArrayedCollection -wrapAt

@section{copymethod}
  ArrayedCollection -foldAt

@section{copymethod}
  ArrayedCollection -clipPut

@section{copymethod}
  ArrayedCollection -wrapPut

@section{copymethod}
  ArrayedCollection -foldPut

@section{copymethod}
  ArrayedCollection -swap

@section{copymethod}
  ArrayedCollection -replace

@section{copymethod}
  ArrayedCollection -++

@section{copymethod}
  ArrayedCollection -add

@section{copymethod}
  ArrayedCollection -addAll

@section{copymethod}
  ArrayedCollection -addFirst

@section{copymethod}
  ArrayedCollection -removeAt

@section{copymethod}
  ArrayedCollection -collect

@section{copymethod}
  ArrayedCollection -do

@section{copymethod}
  ArrayedCollection -reverseDo

@section{copymethod}
  ArrayedCollection -deepCollect

@section{copymethod}
  ArrayedCollection -reshape

@section{copymethod}
  ArrayedCollection -windex

@section{copymethod}
  ArrayedCollection -size

@section{copymethod}
  ArrayedCollection -normalize

@section{copymethod}
  ArrayedCollection -normalizeSum

@section{copymethod}
  ArrayedCollection -plot

@section{method}
 reverse
Returns a new Array whose elements are reversed. The receiver is unchanged.

@racketblock[
x = [1, 2, 3];
z = x.reverse;
x.postln;
z.postln;
::

]
@section{method}
 scramble
Returns a new Array whose elements have been scrambled. The receiver is unchanged.

@racketblock[
[1, 2, 3, 4, 5, 6].scramble.postln;
::

]
@section{method}
 mirror
Return a new Array which is the receiver made into a palindrome.
The receiver is unchanged.

@racketblock[
[1, 2, 3, 4].mirror.postln;
::

]
@section{method}
 mirror1
Return a new Array which is the receiver made into a palindrome with the last element removed.
This is useful if the list will be repeated cyclically, the first element will not get played twice.
The receiver is unchanged.

@racketblock[
[1, 2, 3, 4].mirror1.postln;
::

]
@section{method}
 mirror2
Return a new Array which is the receiver concatenated with a reversal of itself.
The center element is duplicated. The receiver is unchanged.

@racketblock[
[1, 2, 3, 4].mirror2.postln;
::

]
@section{method}
 stutter
Return a new Array whose elements are repeated n times. The receiver is unchanged.

@racketblock[
[1, 2, 3].stutter(2).postln;
::
]
@section{argument}
 n
Number of repeats.

@section{method}
 rotate
Return a new Array whose elements are in rotated order. The receiver is unchanged.

@racketblock[
[1, 2, 3, 4, 5].rotate(1).postln;
[1, 2, 3, 4, 5].rotate(-1).postln;
[1, 2, 3, 4, 5].rotate(3).postln;
::
]
@section{argument}
 n
Number of elements to rotate. Negative n values rotate left, positive n values
rotate right.

@section{method}
 pyramid
Return a new Array whose elements have been reordered via one of 10 "counting" algorithms.
Run the examples to see the algorithms.

@racketblock[
10.do({ arg i;
	[1, 2, 3, 4].pyramid(i + 1).postcs;
});
::
]
@section{argument}
 patternType
Choose counting algorithm. The algorithms are numbered 1 through 10.

@section{method}
 pyramidg
Like pyramid, but keep the resulting values grouped in subarrays.

@racketblock[
// compare:
[1, 2, 3, 4].pyramid(1).postln;
[1, 2, 3, 4].pyramidg(1).postln;
::

]
@section{method}
 sputter
Return a new Array of length maxlen with the items partly repeated (random choice of given probability).

@racketblock[
// compare:
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10].sputter(0.5, 16).postln;
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10].sputter(0.8, 8).postln;
::
]
@section{argument}
 probability
Probability of repeat.
@section{argument}
 maxlen
The length of the new Array.

@section{method}
 lace
Returns a new Array whose elements are interlaced sequences of the elements of the receiver's subcollections, up to size length. The receiver is unchanged.

@racketblock[
x = [ [1, 2, 3], 6, List["foo", 'bar']];
y = x.lace(12);
x.postln;
y.postln;
::

]
@section{method}
 permute
Returns a new Array whose elements are the nthPermutation of the elements of the receiver. The receiver is unchanged.

@racketblock[
x = [ 1, 2, 3];
6.do({|i| x.permute(i).postln;});
::

]
@section{method}
 allTuples
Returns a new Array whose elements contain all possible combinations of the receiver's subcollections.

@racketblock[
[[1, 2, 3, 4, 5], [10, 20, 30]].allTuples;
[[1, 2, 3, 4, 5], [10, 20, 30], [5, 6]].allTuples;
::

]
@section{method}
 wrapExtend
Returns a new Array whose elements are repeated sequences of the receiver, up to size length. The receiver is unchanged.

@racketblock[
x = [ 1, 2, 3, "foo", 'bar' ];
y = x.wrapExtend(9);
x.postln;
y.postln;
::

]
@section{method}
 foldExtend
Same as wrapExtend but the sequences fold back on the list elements.

@racketblock[
x = [ 1, 2, "foo"];
y = x.foldExtend(9);
x.postln;
y.postln;
::

]
@section{method}
 clipExtend
Same as wrapExtend but the sequences "clip" (return their last element) rather than wrapping.

@racketblock[
x = [ 1, 2, "foo"];
y = x.clipExtend(9);
x.postln;
y.postln;
::

]
@section{method}
 slide
Return a new Array whose elements are repeated subsequences from the receiver.
Easier to demonstrate than explain.

@racketblock[
[1, 2, 3, 4, 5, 6].slide(3, 1).postcs;
[1, 2, 3, 4, 5, 6].slide(3, 2).postcs;
[1, 2, 3, 4, 5, 6].slide(4, 1).postcs;
::

]
@section{method}
 shift
Shift the values of the array n steps to the right (n positive) or to the left(n negative),
dropping the excess and filling empty space with zero.

@racketblock[
[1, 2, 3, 4, 5, 6].shift(3).postln;
[1, 2, 3, 4, 5, 6].shift(-3).postln;
::

]
@section{method}
 containsSeqColl
Returns true if the receiver Array contains any instance of SequenceableCollection

@racketblock[
[1, 2, 3, 4].containsSeqColl.postln
[1, 2, [3], 4].containsSeqColl.postln
::

]
@section{method}
 powerset
Returns all possible combinations of the array's elements.

@racketblock[
[1, 2, 3].powerset.postln
[1, 2, 3].powerset.sort({ |a, b| a.size > b.size }); // sort by size, big first
[1, 2, 3].powerset.sort({ |a, b| a.size > b.size }).reverse; // by size, small first
::
powerset is also supported in Collection:
]

@racketblock[
Set[1, 2, 3].powerset;
List[1, 2, 3].powerset
(a: 1, b: 2, c: 3).powerset;
::

]
@section{method}
 envirPairs
Given an array of symbols, this returns an array of pairs of (symbol, value) from the current environment.
This can then be used as arguments for a Synth, or in an OSC message.

@racketblock[
e = (freq: 340, amp: 0.001, strangeness: 0.85);
e.use {
	[\amp, \taste, \strangeness].envirPairs;
}
::

]
@section{method}
 flop
Invert rows and columns in a two dimensional Array (turn inside out).
See also: Function, SequenceableCollection.

@racketblock[
[[1, 2, 3], [4, 5, 6]].flop;
[[1, 2, 3], [4, 5, 6], [7, 8]].flop; // shorter array wraps
[].flop; // result is always 2-d.
::

]
@section{method}
 multiChannelExpand
Used by UGens to perform multi channel expansion. Same as flop.

@section{method}
 source
Some UGens return Arrays of OutputProxy when instantiated. This method allows you to
get at the source UGen.

@racketblock[
z = Pan2.ar;
z.postln;
z.source.postln;
::

]
@section{method}
 fork
Used within Routines and assumes an array of functions, from which subroutines are created. The subroutines are played while the outer Routine carries on. The join parameter expresses after how many subroutines complete the outer Routine is allowed to go on. By default this happens after all subroutines have completed.

@racketblock[
// an array of routine functions:
(
a = [
	{ 1.wait; \done_one.postln },
	{ 0.5.wait; \done_two.postln },
	{ 0.2.wait; \done_three.postln }
];
)
// join after 0
(
Routine {
	"join = 0.".postcln;
	a.fork(0); \doneAll.postln;
}.play;
)
// join after 1
(
Routine {
	"join = 1.".postcln;
	a.fork(1); \doneAll.postln;
}.play;
)
// join after all
(
Routine {
	"join = a.size (default).".postcln;
	a.fork; \doneAll.postln;
}.play;
)

poll(trig, label, trigid)
apply an array of Poll units to an array of UGens (see those helpfiles for more details).

s.boot;
(
x = {
	SinOsc.ar([0.1, 0.2], 0).poll * 0.1
}.play;
)
x.trace; // By tracing the Synth you can see the two Poll units we created
x.free
::

]
@section{method}
 dpoll
apply an array of Dpoll units to an array of UGens (see those helpfiles for more details).

@section{method}
 atIdentityHash
This method is used by IdentitySet to search for a key among its members.

@section{method}
 atIdentityHashInPairs
This method is used by IdentityDictionary to search for a key among its members.

@section{method}
 asString
Returns a string representing the Array. May not be compilable due to elision (...) of excessive arguments.

@section{method}
 asCompileString
Returns a string that will compile to return an Array equal to the receiver.

@section{method}
 isValidUGenInput
Returns true. Arrays are valid UGen inputs.

@section{method}
 asRawOSC
Returns the OSC message as an Int8Array. Receiver must be a bundle.

@racketblock[
[0.1, [\s_new, \default, -1, 1, 1, \freq, 1961]].asRawOSC;
::

]


