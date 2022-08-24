#lang scribble/manual
@(require (for-label racket))

@title{Adverbs for Binary Operators}
 modify behaviour of binary operators@section{categories}
  Language, Collections
@section{related}
  Classes/SequenceableCollection, Classes/Stream

Adverbs are a third argument passed to binary operators that modifies how they iterate over link::Classes/SequenceableCollection::s or link::Classes/Stream::s. The idea for adverbs is taken from the J programming language which is a successor of APL.

@section{section}
  Adverbs and SequenceableCollections

Normally when you add two arrays like this:

@racketblock[
[10, 20, 30, 40, 50] + [1, 2, 3]
::
You get this result:
]

@racketblock[
[ 11, 22, 33, 41, 52 ]
::
A new array is created which is the length of the longer array and items from each array are added together by wrapped indexing.

Using adverbs can change this behavior. Adverbs are symbols and they follow a '.' (dot) after the binary operator. Adverbs can be applied to all binary operators.

]
@section{subsection}
  adverb 's'

The first adverb is 's' which means 'short'. The add operation now returns the shorter of the two arrays.

@racketblock[
[10, 20, 30, 40, 50] +.s [1, 2, 3]
::
returns:
]

@racketblock[
[ 11, 22, 33 ]
::

]
@section{subsection}
  adverb 'f'

Another adverb is 'f' which uses folded indexing instead of wrapped:

@racketblock[
[10, 20, 30, 40, 50] +.f [1, 2, 3]
::
returns:
]

@racketblock[
[ 11, 22, 33, 42, 51 ]
::

]
@section{subsection}
  adverb 't'

The table adverb 't' makes an array of arrays where each item in the first array is added to the whole second array and the resulting arrays are collected.

@racketblock[
[10, 20, 30, 40, 50] +.t [1, 2, 3]
::
returns:
]

@racketblock[
[ [ 11, 12, 13 ], [ 21, 22, 23 ], [ 31, 32, 33 ], [ 41, 42, 43 ], [ 51, 52, 53 ] ]
::

]
@section{subsection}
  adverb 'x'

The cross adverb 'x' is like table, except that the result is a flat array:

@racketblock[
[10, 20, 30, 40, 50] +.x [1, 2, 3]
::
returns:
]

@racketblock[
[ 11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53 ]
::

]
@section{section}
  Adverbs and Streams

There is currently one adverb defined for streams. This is the cross adverb, 'x'.

Normally when you add two streams like this:

@racketblock[
p = (Pseq([10, 20]) + Pseq([1, 2, 3])).asStream;
Array.fill(3, { p.next });
::
you get this:
]

@racketblock[
[ 11, 22, nil ]
::
The items are paired sequentially and the stream ends when the earliest stream ends.

The cross adverb allows you to pair each item in the first stream with every item in the second stream.
]

@racketblock[
p = (Pseq([10, 20]) +.x Pseq([1, 2, 3])).asStream;
Array.fill(7, { p.next });
::
the result is:
]

@racketblock[
[ 11, 12, 13, 21, 22, 23, nil ]
::

You can string these together. Every item in the left stream operand is "ornamented" by the right stream operand.
]

@racketblock[
p = (Pseq([100, 200]) +.x Pseq([10, 20, 30]) +.x Pseq([1, 2, 3, 4])).asStream;
Array.fill(25, { p.next });

[ 111, 112, 113, 114, 121, 122, 123, 124, 131, 132, 133, 134,
  211, 212, 213, 214, 221, 222, 223, 224, 231, 232, 233, 234, nil ]
::

Sound example:
]

@racketblock[
Pbind(\dur, 0.17, \degree, Pwhite(0, 6) +.x Pseq([[0, 2, 4], 1, [0, 2], 3])).trace.play
::

]


