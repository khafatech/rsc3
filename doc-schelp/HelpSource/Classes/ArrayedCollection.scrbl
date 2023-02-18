#lang scribble/manual
@(require (for-label racket))

@title{ArrayedCollection}
@section{categories}
 Collections>Ordered
 Abstract superclass of Collections of fixed maximum size
@section{description}

ArrayedCollection is an abstract class, a subclass of SequenceableCollections whose elements are held in a vector of slots. Instances of ArrayedCollection have a fixed maximum size beyond which they may not grow.

Its principal subclasses are link::Classes/Array:: (for holding objects), and link::Classes/RawArray::, from which link::Classes/Int8Array::, link::Classes/FloatArray::, link::Classes/Signal:: etc. inherit.

@section{CLASSMETHODS}
 

@section{method}
 newClear
Creates a new instance with strong::indexedSize:: indexable slots. The slots are filled with link::Classes/Nil::, zero or something else appropriate to the type of indexable slots in the object.

@racketblock[
Array.newClear(4).postln;
::

]
@section{method}
 with
Create a new ArrayedCollection whose slots are filled with the given arguments.

@racketblock[
Array.with(7, 'eight',  9).postln;
::

]
@section{method}
 series
Fill an ArrayedCollection with an arithmetic series.

@racketblock[
Array.series(5, 10, 2).postln;
::

]
@section{method}
 geom
Fill an ArrayedCollection with a geometric series.

@racketblock[
Array.geom(5, 1, 3).postln;
::

]
@section{method}
 iota
Fills an ArrayedCollection with a counter. See link::Guides/J-concepts-in-SC:: for more examples.

@racketblock[
Array.iota(2, 3);
Array.iota(2, 3, 4);
::


]
@section{INSTANCEMETHODS}
 

@section{method}
 size
Return the number of elements the ArrayedCollection.

@section{method}
 maxSize
Return the maximum number of elements the ArrayedCollection can hold. For example, link::Classes/Array::s may initialise themselves with a larger capacity than the number of elements added.

@racketblock[
[4, 5, 6].maxSize; // gosh
::

]
@section{method}
 at
Return the item at strong::index::.

The index can also be an Array of indices to extract specified elements. Example:

@racketblock[
x = [10,20,30];
y = [0,0,2,2,1];
x[y]; // returns [ 10, 10, 30, 30, 20 ]
::

]
@section{method}
 clipAt
Same as link::#-at::, but values for strong::index:: greater than the size of the ArrayedCollection will be clipped to the last index.

@racketblock[
y = [ 1, 2, 3 ];
y.clipAt(13).postln;
::

]
@section{method}
 wrapAt
Same as link::#-at::, but values for strong::index:: greater than the size of the ArrayedCollection will be wrapped around to 0.

@racketblock[
y = [ 1, 2, 3 ];
y.wrapAt(3).postln; // this returns the value at index 0
y.wrapAt(4).postln; // this returns the value at index 1
y.wrapAt([-2, 1])   // index can also be a collection or negative numbers
::

]
@section{method}
 foldAt
Same as link::#-at::, but values for strong::index:: greater than the size of the ArrayedCollection will be folded back.

@racketblock[
y = [ 1, 2, 3 ];
y.foldAt(3).postln; // this returns the value at index 1
y.foldAt(4).postln; // this returns the value at index 0
y.foldAt(5).postln; // this returns the value at index 1
::

]
@section{method}
 plot
Plot data in a GUI window. See link::Reference/plot:: for more details.

@section{method}
 swap
Swap the values at indices i and j.

@racketblock[
[ 1, 2, 3 ].swap(0, 2).postln;
::

]
@section{method}
 put
Put strong::item:: at strong::index::, replacing what is there.

@section{method}
 clipPut
Same as link::#-put::, but values for strong::index:: greater than the size of the ArrayedCollection will be clipped to the last index.

@section{method}
 wrapPut
Same as link::#-put::, but values for strong::index:: greater than the size of the ArrayedCollection will be wrapped around to 0.

@section{method}
 foldPut
Same as link::#-put::, but values for strong::index:: greater than the size of the ArrayedCollection will be folded back.

@section{method}
 putEach
Put the strong::values:: in the corresponding indices given by strong::keys::. If one of the two argument arrays is longer then it will wrap.

@racketblock[
y = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
y.putEach([4, 7], [\smelly, \head]);
y.putEach([2, 3, 5, 6], \wotsits);
::

]
@section{method}
 indexOf
Return the first index containing an item which matches strong::item::.

@racketblock[
y = [ \the, \symbol, \collection, \contains, \my, \symbol ];
y.indexOf(\symbol);
::

]
@section{method}
 includes
Return a boolean indicating whether the collection contains anything matching strong::item::.

@racketblock[
y = [ \the, \symbol, \collection, \contains, \my, \symbol ];
y.includes(\symbol);
y.includes(\solipsism);
::

]
@section{method}
 indexOfGreaterThan
Return the first index containing an item which is greater than strong::item::.

@racketblock[
y = [ 10, 5, 77, 55, 12, 123];
y.indexOfGreaterThan(70);
::

]
@section{method}
 removeAt
Remove and return the element at strong::index::, shrinking the size of the ArrayedCollection.

@racketblock[
y = [ 1, 2, 3 ];
y.removeAt(1);
y.postln;
::

]
@section{method}
 takeAt
Similar to link::#-removeAt::, but does not maintain the order of the items following the one that was removed. Instead, the last item is placed into the position of the removed item and the array's size decreases by one.

@racketblock[
y = [ 1, 2, 3, 4, 5 ];
y.takeAt(1);
y.postln;
::

]
@section{method}
 takeThese
Removes all items in the receiver for which the strong::func:: answers true. The function is passed two arguments, the item and an integer index. Note that order is not preserved. See link::#-takeAt::.

@racketblock[
y = [ 1, 2, 3, 4 ];
y.takeThese({ arg item, index; item.odd; });	//remove odd items
y.postln;
::

]
@section{method}
 add
Adds an item to an ArrayedCollection if there is space. This method may return a new ArrayedCollection. For this reason, you should always assign the result of add to a variable - never depend on 
@racketblock[add:: changing the receiver.
]

@racketblock[
(
// z and y are the same object
var y, z;
z = [1, 2, 3];
y = z.add(4);
z.postln;
y.postln;
)

(
// in this case a new object is returned
var y, z;
z = [1, 2, 3, 4];
y = z.add(5);
z.postln;
y.postln;
)
::

]
@section{method}
 addAll
Adds all the elements of aCollection to the contents of the receiver. This method may return a new ArrayedCollection. For this reason, you should always assign the result of 
@racketblock[addAll:: to a variable - never depend on add changing the receiver.
]

@racketblock[
(
// in this case a new object is returned
var y, z;
z = [1, 2, 3, 4];
y = z.addAll([7, 8, 9]);
z.postln;
y.postln;
)
::

]
@section{method}
 extend
Extends the object to match strong::size:: by adding a number of strong::item::s. If strong::size:: is less than receiver size then truncate. This method may return a new ArrayedCollection. For this reason, you should always assign the result of 
@racketblock[extend:: to a variable - never depend on add changing the receiver.
]

@racketblock[
(
var y, z;
z = [1, 2, 3, 4];
y = z.extend(10, 9);		//fill up with 9 until the size equals 10
z.postln;
y.postln;
)
::

]
@section{method}
 fill
Inserts the item into the contents of the receiver. @section{note}
 the difference between this and link::Classes/Collection#fill#Collection's *fill::.::

@racketblock[
(
var z;
z = [1, 2, 3, 4];
z.fill(4).postln;
z.fill([1,2,3,4]).postln;
)
::

]
@section{method}
 insert
Inserts the item into the contents of the receiver. This method may return a new ArrayedCollection. For this reason, you should always assign the result of 
@racketblock[insert:: to a variable - never depend on add changing the receiver.
]

@racketblock[
(
// in this case a new object is returned
var y, z;
z = [1, 2, 3, 4];
y = z.insert(1, 999);
z.postln;
y.postln;
)
::

]
@section{method}
 move
Moves an item from one position to another.


@racketblock[
[10, 20, 1000, 40, 50].move(2, 0) // move 1000 to index 0
::

]
@section{argument}
 fromIndex
The position in the array from which the element is removed.
@section{argument}
 toIndex
The position in the array before which the element is inserted again.




@section{method}
 addFirst
Inserts the item before the contents of the receiver, possibly returning a new collection.

@racketblock[
(
// in this case a new object is returned
var y, z;
z = [1, 2, 3, 4];
y = z.addFirst(999);
z.postln;
y.postln;
)
::

]
@section{method}
 pop
Remove and return the last element of the ArrayedCollection.

@racketblock[
(
var z;
z = [1, 2, 3, 4];
z.pop.postln;
z.postln;
)
::

]
@section{method}
 grow
Increase the size of the ArrayedCollection by strong::sizeIncrease:: number of slots, possibly returning a new collection.

@section{method}
 growClear
Increase the size of the ArrayedCollection by strong::sizeIncrease:: number of slots, returning a new collection with link::Classes/Nil::s in the added slots.

@racketblock[
// Compare:
[4,5,6].grow(5);
[4,5,6].growClear(5);
::

]
@section{method}
 copyRange
Return a new ArrayedCollection which is a copy of the indexed slots of the receiver from strong::start:: to strong::end::. If strong::end:: < strong::start::, an empty ArrayedCollection is returned.

@racketblock[
(
var y, z;
z = [1, 2, 3, 4, 5];
y = z.copyRange(1,3);
z.postln;
y.postln;
)
::
]
@section{warning}
  
@racketblock[x.copyRange(a, b):: is strong::not:: equivalent to ]

@racketblock[x[a..b]::. The latter compiles to link::#-copySeries::, which has different behavior when strong::end:: < strong::start::. ::

]
@section{method}
 copySeries
Return a new ArrayedCollection consisting of the values starting at strong::first::, then every step of the distance between strong::first:: and strong::second::, up until strong::last::. If strong::second:: is 
@racketblock[nil::, a step of 1 or -1 is used as appropriate.

]

@racketblock[x.copySeries(a, nil, c):: is equivalent to ]

@racketblock[x[a..c]::, and ]

@racketblock[x.copySeries(a, b, c):: is equivalent to ]

@racketblock[x[a,b..c]::

]

@racketblock[
(
var y, z;
z = [1, 2, 3, 4, 5, 6];
y = z.copySeries(0, 2, 5);
y.postln;
)
::

]
@section{note}
 If the intent is to copy emphasis::forward:: in an array, and you are calculating start and end indices such that 
@racketblock[end:: may be less than ]

@racketblock[start::, it is not safe to use ]

@racketblock[copySeries:: or the shortcut syntax ]

@racketblock[x[a..b]:: because it will adapt to use a positive or negative step as needed. In this case, ]

@racketblock[copyRange:: is recommended.

]

@racketblock[
a = Array.series(10, 0, 1);
a[2..0];  // [ 2, 1, 0 ]
a.copyRange(2, 0);  // [  ]
::
::

]
@section{method}
 seriesFill
Fill the receiver with an arithmetic progression. The first element will be strong::start::, the second strong::start + step::, the third strong::start + step + step:: ...

@racketblock[
(
var y;
y = Array.newClear(15);
y.seriesFill(5, 3);
y.postln;
)
::

]
@section{method}
 putSeries
Put strong::value:: at every index starting at strong::first::, then every step of the distance between strong::first:: and strong::second::, up until strong::last::.

@racketblock[x.putSeries(a, b, c, val):: can also be written as ]

@racketblock[x[a, b..c] = val::
]

@racketblock[
(
var y, z;
z = [1, 2, 3, 4, 5, 6];
y = z.putSeries(0, 2, 5, "foo");
y.postln;
)
::

]
@section{method}
 ++
Concatenate the contents of the two collections into a new ArrayedCollection.

@racketblock[
(
var y, z;
z = [1, 2, 3, 4];
y = z ++ [7, 8, 9];
z.postln;
y.postln;
)
::

]
@section{method}
 reverse
Return a new ArrayedCollection whose elements are reversed.

@racketblock[
(
var y, z;
z = [1, 2, 3, 4];
y = z.reverse;
z.postln;
y.postln;
)
::

]
@section{method}
 do
Iterate over the elements in order, calling the function for each element. The function is passed two arguments, the element and an index.

@racketblock[
['a', 'b', 'c'].do({ arg item, i; [i, item].postln; });
::

]
@section{method}
 reverseDo
Iterate over the elements in reverse order, calling the function for each element. The function is passed two arguments, the element and an index.

@racketblock[
['a', 'b', 'c'].reverseDo({ arg item, i; [i, item].postln; });
::

]
@section{method}
 collect
Answer a new collection which consists of the results of function evaluated for each item in the collection. The function is passed two arguments, the item and an integer index. See link::Classes/Collection:: helpfile for examples.

@section{method}
 deepCollect
The same as link::#-collect::, but can look inside sub-arrays up to the specified strong::depth::.

@racketblock[
a = [99, [4,6,5], [[32]]];
a.deepCollect(1, {|item| item.isArray}).postln;
a.deepCollect(2, {|item| item.isArray}).postln;
a.deepCollect(3, {|item| item.isArray}).postln;
::

]
@section{method}
 windex
Interprets the array as a list of probabilities which should sum to 1.0 and returns a random index value based on those probabilities.

@racketblock[
(
Array.fill(10, {
	[0.1, 0.6, 0.3].windex;
}).postln;
)
::

]
@section{method}
 normalizeSum
Returns the Array resulting from :

@racketblock[
(this / this.sum)
::
so that the array will sum to 1.0.

This is useful for using with windex or wchoose.
]

@racketblock[
[1, 2, 3].normalizeSum.postln;
::

]
@section{method}
 normalize
Returns a new Array with the receiver items normalized between strong::min:: and strong::max::.

@racketblock[
[1, 2, 3].normalize;			//default min=0, max= 1
[1, 2, 3].normalize(-20, 10);
::

]
@section{method}
 perfectShuffle
Returns a copy of the receiver with its items split into two equal halves, then reconstructed by interleaving the two halves. @section{note}
 use an even number of item pairs in order to not loose any items in the shuffle.::

@racketblock[
(
var y, z;
z = [ 1, 2, 3, 4, 5, 6 ];
y = z.perfectShuffle;
z.postln;
y.postln;
)
::

]
@section{method}
 performInPlace
Performs a method in place, within a certain region [from..to], returning the same array.

@racketblock[
a = (0..10);
a.performInPlace(\normalizeSum, 3, 6);
::

]
@section{method}
 rank
Rank is the number of dimensions in a multidimensional array.

@racketblock[
a = [4,7,6,8];
a.rank;
a = [[4,7],[6,8]];
a.rank;
::

]
@section{method}
 shape
For a multidimensional array, returns the number of elements along each dimension.

@racketblock[
a = [4,7,6,8];
a.shape;
a = [[4,7],[6,8]];
a.shape;
::

]
@section{method}
 reshape
For a multidimensional array, rearranges the data using the desired number of elements along each dimension. The data may be extended using wrapExtend if needed.

@racketblock[
a = [4,7,6,8];
a.reshape(2,2);
a.reshape(2,3);
::

]
@section{method}
 find
Finds the starting index of a number of elements contained in the array.

@racketblock[
a = (0..10);
a.find([4, 5, 6]);
::

]
@section{method}
 replace
Return a new array in which a number of elements have been replaced by another.

@racketblock[
a = (0..10) ++ (0..10);
a.replace([4, 5, 6], 100);
a.replace([4, 5, 6], [1734, 1985, 1860]);
::
this method is inherited by link::Classes/String:: :
]

@racketblock[
a = "hello world";
a.replace("world", "word");
::

]
@section{method}
 asRandomTable
Return an integral table that can be used to generate random numbers with a specified distribution.
(see link::Guides/Randomness:: helpfile for a more detailed example)

@racketblock[
(
a = (0..100) ++ (100..50) / 100; // distribution
a = a.asRandomTable;
)
::

]
@section{method}
 tableRand
Returns a new random number from a random table.

@racketblock[
(
a = (0..100) ++ (100..50) / 100; // distribution
a = a.asRandomTable;
20.do { a.tableRand.postln };
)
::

]
@section{method}
 msgSize
Return the size of an osc message in bytes

@racketblock[
a = ["/s_new", "default", -1, "freq", 440];
a.msgSize;
::

]
@section{method}
 bundleSize
Return the size of an osc bundle in bytes

@racketblock[
a = [["/s_new", "default", -1, "freq", 440], ["/s_new", "default", -1, "freq", 220]];
a.bundleSize;
::

]
@section{method}
 asciiPlot
For an ArrayedCollection containing numbers (e.g. audio data) this renders a plot in the post window using asterisks and spaces (works best if you use a monospace font in your post window).

@racketblock[
a = (0, pi/10 .. 5pi).collect{|val| val.sin};
a.asciiPlot;
::
]


