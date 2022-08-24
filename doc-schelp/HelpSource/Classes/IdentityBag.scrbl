#lang scribble/manual
@(require (for-label racket))

@title{IdentityBag}
A Bag according to identity@section{categories}
 Collections>Unordered

@section{description}

An IdentityBag is an unordered collection of objects. In some languages it is referred to as a counted set. A Bag keeps track of the number of times objects are inserted and requires that objects be removed the same number of times. There is only one instance of an object in a Bag even if the object has been added to the Bag multiple times (test is for strong::identity::).

The contents of a IdentityBag are unordered. You must not depend on the order of items in a set.

@section{INSTANCEMETHODS}
 

@section{private}
 setDictionary

@section{subsection}
 Adding and Removing

@section{method}
 add
Add anObject to the Bag. A Bag may contain multiple entries of the same object.

@racketblock[
IdentityBag[1, 2, 3].add(4);
IdentityBag[1, 2, 3].add(3);
IdentityBag["abc", "def", "ghi"].add("jkl");
IdentityBag["abc", "def", "ghi"].add("def");
::

]
@section{method}
 remove
Remove anObject from the IdentityBag.

@racketblock[
IdentityBag[1, 2, 3].remove(3);
::

]
@section{method}
 contents
Returns the dictionary that stores the objects in pairs (obj -> numberOfObjects)

@racketblock[
IdentityBag[\a, \b, \c, \c].contents;
::

]
@section{subsection}
 Iteration

@section{method}
 do
Evaluates function for each item in the IdentityBag.
The function is passed two arguments, the item and an integer index.

@racketblock[
IdentityBag[1, 2, 3, 300].do({ arg item, i; item.postln });
::

]
@section{section}
 Difference between Bag and IdentityBag

@racketblock[
// the two strings are equal, but not identical
"something" == "something"; // true
"something" === "something" // false

a = Bag.new;
a.add("something");
a.add("something");
a.contents; // only one object in the bag really

a = IdentityBag.new;
a.add("something");
a.add("something");
a.contents; // two objects in the bag
::
]


