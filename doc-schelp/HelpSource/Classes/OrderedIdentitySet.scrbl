#lang scribble/manual
@(require (for-label racket))

@title{OrderedIdentitySet}
a set according to identity@section{related}
 Classes/IdentitySet, Classes/List, Classes/Dictionary
@section{categories}
 Collections>Ordered

@section{description}

An OrderedIdentitySet is a collection of objects, no two of which are the same object (aka. "identical").
Most of its methods are inherited. (see link::Classes/Collection:: and link::Classes/Set:: classes).
Unlike link::Classes/IdentitySet::, contents of an OrderedIdentitySet are ordered.

@section{INSTANCEMETHODS}
 

@section{private}
 putCheck

@section{method}
 do
Evaluates strong::function:: for each item in the OrderedIdentitySet. You may depend on the order of items. The function is passed two arguments, the item and an integer index.

@racketblock[
OrderedIdentitySet[1, 2, 3, 300].do { |item, i| item.postln };
::
]


