#lang scribble/manual
@(require (for-label racket))

@title{Set}
a set according to equality@section{related}
 Classes/IdentitySet, Classes/List, Classes/Dictionary
@section{categories}
 Collections>Unordered

@section{description}

A Set is s collection of objects, no two of which are equal. Most of its methods are inherited from Collection. The contents of a Set are unordered. You must not depend on the order of items in a set. For an ordered set, see link::Classes/OrderedIdentitySet::.

@section{INSTANCEMETHODS}
 

@section{private}
 initSet, putCheck, fullCheck, grow, noCheckAdd

@section{subsection}
 Adding and Removing

@section{method}
 add
Add anObject to the Set. An object which is equal to an object already in the Set will not be added.

@racketblock[
Set[1, 2, 3].add(4).postln;
Set[1, 2, 3].add(3).postln;
Set["abc", "def", "ghi"].add("jkl").postln;
Set["abc", "def", "ghi"].add("def").postln;
::

]
@section{method}
 remove
Remove anObject from the Set.

@racketblock[
Set[1, 2, 3].remove(3).postln;
::

]
@section{subsection}
 Iteration

@section{method}
 do
Evaluates function for each item in the Set. The function is passed two arguments, the item and an integer index.

@racketblock[
Set[1, 2, 3, 300].do({ arg item, i; item.postln });
::

]
@section{method}
 keyAt
Returns the object at the internal strong::index::. This index is not deterministic.

@section{subsection}
 Set specific operations

@section{method}
 sect, &
Return the set theoretical intersection of this and strong::that::.

@racketblock[
a = Set[1, 2, 3]; b = Set[2, 3, 4, 5];
sect(a, b);
a & b // shorter syntax
::

]
@section{method}
 union, |
Return the set theoretical union of this and strong::that::.

@racketblock[
a = Set[1, 2, 3]; b = Set[2, 3, 4, 5];
union(a, b);
a | b // shorter syntax
::

]
@section{method}
 difference, -
Return the set of all items which are elements of this, but not of strong::that::.

@racketblock[
a = Set[1, 2, 3]; b = Set[2, 3, 4, 5];
difference(a, b);
a - b // shorter syntax
::

]
@section{method}
 symmetricDifference, --
Return the set of all items which are not elements of both this and strong::that::.

@racketblock[
a = Set[1, 2, 3]; b = Set[2, 3, 4, 5];
symmetricDifference(a, b);
a -- b // shorter syntax
::

]
@section{method}
 isSubsetOf
Returns true if all elements of this are also elements of strong::that::.

@racketblock[
a = Set[1, 2, 3, 4];
Set[1, 2].isSubsetOf(a); // true
Set[1, 5].isSubsetOf(a); // false
::

]
@section{EXAMPLES}
 


@racketblock[
a = Set[1, 2, 3, 4];
b = a.powerset; // set of all parts
a.isSubsetOf(b); // false: no set is ever part of itself.
b.asArray.reduce(\union) == a; // true parts may not contain other elements that original
b.asArray.reduce(\difference).isEmpty; // true.
::

]

@racketblock[
// you can use Set to efficiently remove duplicates from an array:

a = [1, 2, 3, 4, 3, 5, 5, 2, 2, 1];
a.as(Set);		// convert to set
a.as(Set).as(Array);	// and convert back
::
]


