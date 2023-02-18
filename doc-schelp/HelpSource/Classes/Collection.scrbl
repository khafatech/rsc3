#lang scribble/manual
@(require (for-label racket))

@title{Collection}
Abstract superclass of all collections@section{related}
 Classes/List, Classes/Array, Classes/Dictionary, Classes/Bag, Classes/Set, Classes/SortedList
@section{categories}
 Collections

@section{description}

Collection is an abstract class. You do not create direct instances of Collection.
There are many types of Collections including link::Classes/@section{List}
 , link::Classes/Array::, link::Classes/Dictionary::, link::Classes/Bag::, link::Classes/Set::, link::Classes/Sorted@section{List}
 , etc. See link::Overviews/Collections:: for a complete class tree.

@section{CLASSMETHODS}
 

@section{method}
 newFrom
Creates a new Collection from another collection. This supports the interface for the method "as".

@racketblock[
Array.newFrom(Set[4, 2, 1]);
Set.newFrom(Array[4, 2, 1]);
[1, 2, 3, 4, 3, 2].as(Set); // as(someClass) calls someClass.newFrom(this)
::

]
@section{method}
 with
Creates a new Collection from the args.

@racketblock[
Array.with(4, 2, 1);
::

]
@section{method}
 fill
Creates a Collection of the given size, the elements of which are determined by evaluation the given function. The function is passed the index as an argument.

@racketblock[
Array.fill(4, { arg i; i * 2 });
Bag.fill(14, { arg i; i.rand });
::

]
@section{argument}
 size
The size of the collection which is returned. If nil, it returns an empty collection. If an array of sizes is given, the resulting collection has the appropriate dimensions (see: link::#*fillND).
::


@racketblock[
Array.fill([2, 2, 3], { arg i, j, k;  i * 100 + (j * 10) + k });
::

]
@section{argument}
 function
The function which is called for each new element - the index is passed in as a first argument. The function be anything that responds to the message "value".


@racketblock[
Array.fill(10, { arg i; 2 ** i });
Array.fill(10, Pxrand([0, 1, 2], inf).iter);
Array.fill(10, 7); // an object that doesn't respond with a new value is just repeatedly added.
::

]
@section{method}
 fill2D
Creates a 2 dimensional Collection of the given sizes. The items are determined by evaluation of the supplied function. The function is passed row and column indexes as arguments. See link::Guides/J-concepts-in-SC::

@racketblock[
Array.fill2D(2, 4, 0);
Array.fill2D(3, 4, { arg r, c; r*c+c; });
::

]
@section{method}
 fill3D
Creates a 3 dimensional Collection of the given sizes. The items are determined by evaluation of the supplied function. The function is passed plane, row and column indexes as arguments. See link::Guides/J-concepts-in-SC::

@racketblock[
Array.fill3D(2, 3, 4, { arg p, r, c; p; });
::

]
@section{method}
 fillND
Creates a N dimensional Collection where N is the size of the array strong::dimensions::. The items are determined by evaluation of the supplied function. The function is passed N number of indexes as arguments. See link::Guides/J-concepts-in-SC::

@racketblock[
Array.fillND([4, 4], { arg a, b; a+b; });				// 2D
Array.fillND([4, 4, 4], { arg a, b, c; a+b*c; });		// 3D
Array.fillND([1, 2, 3, 4], { arg a, b, c, d; b+d; });	// 4D
::

]
@section{INSTANCEMETHODS}
 

@section{subsection}
 Accessing

@section{method}
 size
Answers the number of objects contained in the Collection.

@racketblock[
List[1, 2, 3, 4].size;
::

]
@section{method}
 isEmpty
Answer whether the receiver contains no objects.

@racketblock[
List[].isEmpty;
::


]
@section{subsection}
 Adding and Removing

@section{method}
 add
Add anObject to the receiver.

@racketblock[
List[1, 2].add(3);
::

]
@section{method}
 addAll
Add all items in aCollection to the receiver.

@racketblock[
List[1, 2].addAll(List[3, 4]);
::

]
@section{method}
 remove
Remove anObject from the receiver. Answers the removed object.

@racketblock[
(
var a;
a = List[1, 2, 3, 4];
a.remove(3);
a;
)
::

]
@section{method}
 removeAll
Remove all items in aCollection from the receiver.

@racketblock[
List[1, 2, 3, 4].removeAll(List[2, 3]);
::
]
@section{note}
 that multiple items in the receiver will not necessarily be removed

@racketblock[
~closet = [\hat, \hat, \hat, \coat, \coat, \shoe, \shoe];
~closet.removeAll([\hat, \coat, \shoe, \shoe]); // Doesn't empty the closet, just removes what we wanted to
::
See link::#-removeEvery:: for a related method that removes all occurrences.
::

]
@section{method}
 removeEvery
Remove all occurrences of the items in aCollection from the receiver.

@racketblock[
List[1, 2, 3, 2, 3, 2, 3, 4].removeEvery(List[2, 3]);
::

]
@section{method}
 removeAllSuchThat
Remove all items in the receiver for which function answers link::Classes/True::. The function is passed two arguments, the item and an integer index. Answers the objects which have been removed.

@racketblock[
(
var a;
a = List[1, 2, 3, 4];
a.removeAllSuchThat({ arg item, i; item < 3 });
a;
)
::

]
@section{method}
 putEach
Put the values in the corresponding indices given by keys. If one of the two argument arrays is longer then it will wrap.

@racketblock[
y = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
y.putEach([4, 7], [\smelly, \head]);
y.putEach([2, 3, 5, 6], \wotsits);
::

]
@section{method}
 atAll
Return a collection of all the items for the keys.

@racketblock[
y = [\a, \b, \c];
y.atAll([0, 2]);
::

]
@section{subsection}
 Testing

@section{method}
 includes
Answer whether anObject is contained in the receiver.

@racketblock[
List[1, 2, 3, 4].includes(3);
::

]
@section{method}
 includesAny
Answer whether any item in aCollection is contained in the receiver.

@racketblock[
List[1, 2, 3, 4].includesAny(List[4, 5]);
::

]
@section{method}
 includesAll
Answer whether all items in aCollection are contained in the receiver.

@racketblock[
List[1, 2, 3, 4].includesAll(List[4, 5]);
::

]
@section{method}
 matchItem
Returns link::Classes/True:: if this includes the strong::item::.

See also link::Reference/matchItem::.


@section{subsection}
 Iteration

@section{method}
 do
Evaluates strong::function:: for each item in the collection. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].do({ arg item, i; item.postln });
::

]
@section{method}
 collect
Answer a new collection which consists of the results of strong::function:: evaluated for each item in the collection. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].collect({ arg item, i; item + 10 });
::
If you want to control what type of collection is returned, use link::#-collectAs::(function, class).

]
@section{method}
 select
Answer a new collection which consists of all items in the receiver for which strong::function:: answers link::Classes/True::. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].select({ arg item, i; item.even });
::
If you want to control what type of collection is returned, use link::#-selectAs::(function, class).

]
@section{method}
 reject
Answer a new collection which consists of all items in the receiver for which strong::function:: answers link::Classes/False::. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].reject({ arg item, i; item.even });
::
If you want to control what type of collection is returned, use link::#-rejectAs::(function, class).

]
@section{method}
 detect
Answer the first item in the receiver for which strong::function:: answers link::Classes/True::. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].detect({ arg item, i; item.even });
::

]
@section{method}
 detectIndex
Similar to link::#-detect:: but returns the index instead of the item itself.

@racketblock[
List[1, 2, 3, 4].detectIndex({ arg item, i; item.even });
::

]
@section{method}
 inject
In functional programming, the operation known as a left fold.
inject takes an initial value and a function and combines the elements of the collection by applying the function to the accumulated value and an element from the collection starting from the first element in the collection. The strong::function:: takes two arguments and returns the new value. The accumulated value is initialized to strong::initialValue::.

@racketblock[
[1,2,3,4,5].inject(0, _+_); // 15

[1,2,3,4,5].inject(1, _*_); // 120

// same as .collect(_.squared)
[1,2,3,4,5].inject([], {|a,b| a ++ b.squared }); // [ 1, 4, 9, 16, 25 ]
[1,2,3,4,5].inject([], {|a,b| [b] ++ a ++ [b]}); // [ 5, 4, 3, 2, 1, 1, 2, 3, 4, 5 ]
[1,2,3,4,5].inject([], {|a,b| a ++ b ++ a});
[1,2,3,4,5].inject([], {|a,b| a ++ a ++ b});
::

]
@section{method}
 injectr
In functional programming, the operation known as a right fold.
inject takes an initial value and a function and combines the elements of the collection by applying the function to the accumulated value and an element from the collection starting from the last element in the collection. The strong::function:: takes two arguments and returns the new value. The accumulated value is initialized to strong::initialValue::.

@racketblock[
[1,2,3,4,5].injectr([], _++_); // [ 5, 4, 3, 2, 1 ]

[1,2,3,4,5].inject([], _++_); // [ 1, 2, 3, 4, 5 ]
::

]
@section{method}
 collectInPlace
Iterate over the collection and replace each item with a new one, returned by the function. This can be useful when one wants to aviod creating a new array in memory. In most cases, it is better to use link::#-collect::.


@racketblock[
a = [1, 5, 3, 4];
a.collectInPlace { |x| 2 ** x };
a; // changed

// compare:
a = [1, 5, 3, 4];
a.collect { |x| 2 ** x };
a; // remains unchanged
::

]
@section{method}
 collectCopy
Like link::#-collect::, but the collection is copied before iteration. This is recommended wherever the function may change the collection itself.


@racketblock[
a = [1, 5, 2, 3, 4];
b = a.collectCopy { |x| if(x.even) { a.remove(x); "removed" } { x }  };
a;
b;
::

]
@section{method}
 any
Answer whether strong::function:: answers link::Classes/True:: for any item in the receiver. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].any({ arg item, i; item.even });
::

]
@section{method}
 every
Answer whether strong::function:: answers link::Classes/True:: for every item in the receiver. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].every({ arg item, i; item.even });
::

]
@section{method}
 count
Answer the number of items for which strong::function:: answers link::Classes/True::. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].count({ arg item, i; item.even });
::

]
@section{method}
 occurrencesOf
Answer the number of items in the receiver which are equal to anObject.

@racketblock[
List[1, 2, 3, 3, 4, 3, 4, 3].occurrencesOf(3);
::

]
@section{method}
 sum
Answer the sum of the results of strong::function:: evaluated for each item in the receiver. The function is passed two arguments, the item and an integer index.

@racketblock[
List[1, 2, 3, 4].sum;
(0..8).sum { |i| 1 / (2 ** i) };
::

]
@section{method}
 maxItem
Answer the maximum of the results of strong::function:: evaluated for each item in the receiver. The function is passed two arguments, the item and an integer index.
If function is nil, then answer the maximum of all items in the receiver.

@racketblock[
List[1, 2, 3, 4].maxItem({ arg item, i; item + 10 });
::

]
@section{method}
 minItem
Answer the minimum of the results of strong::function:: evaluated for each item in the receiver. The function is passed two arguments, the item and an integer index.
If function is nil, then answer the minimum of all items in the receiver.

@racketblock[
List[1, 2, 3, 4].minItem({ arg item, i; item + 10 });
::

]
@section{method}
 maxIndex
Answer the index of the maximum of the results of strong::function:: evaluated for each item in the receiver. The function is passed two arguments, the item and an integer index.
If function is nil, then answer the maximum of all items in the receiver.

@racketblock[
List[1, 2, 3, 4].maxIndex({ arg item, i; item + 10 });
[3.2, 12.2, 13, 0.4].maxIndex;
::

]
@section{method}
 minIndex
Answer the index of the minimum of the results of strong::function:: evaluated for each item in the receiver. The function is passed two arguments, the item and an integer index.
If function is nil, then answer the minimum of all items in the receiver.

@racketblock[
List[1, 2, 3, 4].minIndex({ arg item, i; item + 10 });
List[3.2, 12.2, 13, 0.4].minIndex;
::

]
@section{method}
 maxSizeAtDepth
Returns the maximum size of all subcollections at a certain depth (dimension)

@section{argument}
 rank
The depth at which the size of the collection is measured


@racketblock[
Set[Set[1, 2, 3], [Set[41, 52], 5, 6], 1, 2, 3].maxSizeAtDepth(2);
Set[Set[1, 2, 3], [Set[41, 52], 5, 6], 1, 2, 3].maxSizeAtDepth(1);
Set[Set[1, 2, 3], [Set[41, 52], 5, 6], 1, 2, 3].maxSizeAtDepth(0);
Set[].maxSizeAtDepth(0);
Set[[]].maxSizeAtDepth(0);
Set[[]].maxSizeAtDepth(1);
::

]
@section{method}
 maxDepth
Returns the maximum depth of all subcollections.

@section{argument}
 max
Internally used only.


@racketblock[
Set[Set[1, 2, 3], Set[Set[41, 52], 5, 6], 1, 2, 3].maxDepth
::

]
@section{method}
 iter
Returns a link::Classes/Routine:: that returns the elements one by one.

@racketblock[
r = Set[10, 2, -3, -4].iter;
r.next;
r.next;
r.next;
r.next; // nil.
::

]
@section{subsection}
 Conversion

@section{method}
 asBag
Answer a link::Classes/Bag:: to which all items in the receiver have been added.

@racketblock[
List[1, 2, 3, 4].asBag;
::

]
@section{method}
 asList
Answer a link::Classes/@section{List}
  to which all items in the receiver have been added.

@racketblock[
Set[1, 2, 3, 4].asList;
::

]
@section{method}
 asSet
Answer a link::Classes/Set:: to which all items in the receiver have been added.

@racketblock[
List[1, 2, 3, 4].asSet;
::

]
@section{method}
 asSortedList
Answer a link::Classes/Sorted@section{List}
  to which all items in the receiver have been added.

@racketblock[
List[2, 1, 4, 3].asSortedList;
::

]
@section{method}
 powerset
Returns all possible combinations of the collection's elements.

@racketblock[
Set[1, 2, 3].powerset;

// generate the von neumann ordinals. (warning: only count to four at maximum!)
a = Set[];
a = a.powerset;
a = a.powerset;
a = a.powerset;

u = { |set| set.unify }; // union (count down)
n = { |set| set.powerset }; // powerset (count up)
a = Set[]; // empty set (zero)
n.(n.(a)); // two
u.(n.(n.(a))) == n.(a); // two - one == one
u.(u.(n.(n.(a)))) == u.(n.(a)); // two - two == one - one
::

]
@section{method}
 flopDict
Takes a collection of dictionaries and returns a single dictionary with arrays of all dictionaries' elements.
If unbubble is link::Classes/True:: (default), and if one element is singular, the array is replaced by this element.

@racketblock[
[(degree: 7, x: 4), (degree: 8, x: 5), (degree: -2, dur: 2.5)].flopDict;
[(degree: 7, x: 4), (degree: 8, x: 5), (degree: -2, dur: 2.5)].flopDict(false);
::

]
@section{method}
 histo
Returns a histogram of the collection by counting the number of values that fall into each slot of size (default: 100) subdivisions between min and max. If there are any values outside this range, it posts a note. If min or max is not given, the smallest (or largest value respectively) is used.

@racketblock[
{ 1.0.linrand }.dup(10000).histo(1000).plot;
{ 8.rand }.dup(10000).histo(8).plot(discrete: true);
::

]
@section{method}
 invert
Subtractively invert a collection about a value (default: sum of minimal and maximum value).
It can be used to invert a pitch list about a given axis.

@racketblock[
[0, 1, 4, 7].invert(0);
[0, 1, 2, 3].invert(1);
[3, 2, 9, 7].invert(11); // becomes [ 19, 20, 13, 15 ]
// if axis is nil, invert uses the registral center
[3, 2, 9, 7].invert; // becomes [ 8, 9, 2, 4 ]
// invert chords
[[0, 5, 7], [5, 7, 11], [6, 7, 9]].invert(5);
::

]
@section{subsection}
 Writing to streams

@section{method}
 printOn
Print a representation of the collection to a stream.

@section{method}
 storeOn
Write a compilable representation of the collection to a stream.

@section{method}
 printItemsOn
Print a comma separated compilable representation of the items in the collection to a stream.

@section{method}
 storeItemsOn
Write a comma separated compilable representation of the items in the collection to a stream.

@section{subsection}
 Set specific operations

@section{method}
 sect
Return the set theoretical intersection of this and strong::that::.

@racketblock[
a = [1, 2, 3]; b = [2, 3, 4, 5];
sect(a, b);
::

]
@section{method}
 union
Return the set theoretical union of this and strong::that::.

@racketblock[
a = [1, 2, 3]; b = [2, 3, 4, 5];
union(a, b);
::

]
@section{method}
 difference
Return the set of all items which are elements of this, but not of strong::that::.

@racketblock[
a = [1, 2, 3]; b = [2, 3, 4, 5];
difference(a, b);
::

]
@section{method}
 symmetricDifference
Return the set of all items which are not elements of both  this and strong::that::.
this -- that

@racketblock[
a = [1, 2, 3]; b = [2, 3, 4, 5];
symmetricDifference(a, b);
::

]
@section{method}
 isSubsetOf
Returns link::Classes/True:: if all elements of this are also elements of strong::that::

@racketblock[
a = Set[1, 2, 3, 4];
Set[1, 2].isSubsetOf(a); // true
Set[1, 5].isSubsetOf(a); // false
::
]


