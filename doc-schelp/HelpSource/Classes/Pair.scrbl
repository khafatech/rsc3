#lang scribble/manual
@(require (for-label racket))

@title{Pair}
LISP-like two element cells@section{categories}
  Collections>Ordered

@section{description}


@section{note}
 
Implementation incomplete. See link::Guides/J-concepts-in-SC:: for similar functionality.
::

Most methods are inherited from the superclasses.

@section{CLASSMETHODS}
 

@section{method}
 new
Return new instance.

@section{method}
 newFrom
Convert collection (e.g. arrays of arrays) to pairs.

@section{INSTANCEMETHODS}
 

@section{private}
 storeOn, printOn, storeArgs

@section{method}
 size
Return the size when linking across.

@section{method}
 depth
Return the size when linking down.

@section{method}
 do
Iterate over the two elements.

Traverse
Same like: link::#-depthFirstPreOrderTraversal::

@section{method}
 depthFirstPreOrderTraversal
Traverse the data structure first link down, then across (see link::#@section{Examples}
 ).

@section{method}
 depthFirstPostOrderTraversal
Traverse the data structure from bottom up (see link::#@section{Examples}
 ).

@section{EXAMPLES}
 


@racketblock[
a = Pair(Pair(Pair(1, 2), 4), Pair(5, 6));

a.size;
a.depth;
a.do { |x| x.postln };
a.traverse { |x| x.postln };
a.depthFirstPreOrderTraversal { |x| x.postln };
a.depthFirstPostOrderTraversal { |x| x.postln };


// alternative instantiations:

Pair.newFrom([1, [2, [[4, 5], 6]]]);

[1, [2, [[4, 5], 6]]].as(Pair); // equivalent.
::
]


