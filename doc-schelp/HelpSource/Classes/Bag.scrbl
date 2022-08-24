#lang scribble/manual
@(require (for-label racket))

@title{Bag}
Unordered collection of objects@section{related}
 Classes/IdentityBag, Classes/Set
@section{categories}
 Collections>Unordered

@section{description}

A Bag is an unordered collection of objects. In some languages it is referred to as a counted set. A Bag keeps track of the number of times objects are inserted and requires that objects be removed the same number of times. There is only one instance of an object in a Bag even if the object has been added to the Bag multiple times (test is for strong::equality::)

Most of Bag's methods are inherited from Collection.
The contents of a Bag are unordered. You must not depend on the order of items in a set.

@section{CLASSMETHODS}
 

@section{method}
 new
Creates a Bag with an initial capacity for strong::n:: objects.

@section{INSTANCEMETHODS}
 

@section{private}
 setDictionary

@section{method}
 contents
Returns the dictionary that stores the objects in pairs (obj -> numberOfObjects)

@racketblock[
Bag["a", "b", "c", "c"].contents;
::
]
@section{returns}
  link::Classes/Dictionary::

@section{method}
 itemCount
Count the number of strong::item::s.

@racketblock[
Bag[1, 2, 2, 3, 300, 2].itemCount(2);
::

]
@section{subsection}
 Adding and Removing

@section{method}
 add
Add an object to the Bag. A Bag may contain multiple entries of the same object.

@racketblock[
Bag[1, 2, 3].add(4).postln;

Bag[1, 2, 3].add(3).postln;

Bag["abc", "def", "ghi"].add("jkl").postln;

Bag["abc", "def", "ghi"].add("def").postln;
::

]
@section{method}
 remove
Remove an object from the Bag.

@racketblock[
Bag[1, 2, 3].remove(3).postln;
::

]
@section{subsection}
 Iteration

@section{method}
 do
Evaluates strong::function:: for each item in the Bag.
The function is passed two arguments, the item and an integer index.

@racketblock[
Bag[1, 2, 3, 300].do({ arg item, i; item.postln });

Bag[1, 2, 2, 3, 300].do({ arg item, i; item.postln });
::
]
@section{argument}
 function
args to function: item, i

@section{method}
 countsDo
Evaluates strong::function:: for each unique item in the Bag along with that item's count.
The function is passed two arguments, the item, the quantity of that item in the Bag and an integer index.

@racketblock[
Bag[1, 2, 3, 300].countsDo({ arg item, count, i; [item,count].postln });

Bag[1, 2, 2, 3, 300].countsDo({ arg item, count, i; [item,count].postln });
::

]
@section{subsection}
 Testing

@section{method}
 includes
Answer whether an object is contained in the Bag.

@racketblock[
Bag[1, 2, 3, 4].includes(3);
::
]
@section{returns}
  link::Classes/Boolean::

@section{EXAMPLES}
 

@section{subsection}
 Difference between Bag and IdentityBag:

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


