#lang scribble/manual
@(require (for-label racket))

@title{Dictionary}
associative collection mapping keys to values@section{related}
 Classes/Environment, Classes/Event
@section{categories}
 Collections>Unordered

@section{description}

A Dictionary is an associative collection mapping keys to values.
Two keys match if they are strong::equal::. (i.e. == returns true.)

The contents of a Dictionary are strong::unordered::. You must not depend on the order of items in a Dictionary.
You must only rely on equality for the keys (e.g. symbols are ok, strings not). For identity matching see: link::Classes/IdentityDictionary::.

@section{CLASSMETHODS}
 

@section{method}
 new
Creates a Dictionary with an initial capacity for strong::n:: key value mappings.

@section{method}
 newFrom
Creates a new Dictionary from another collection.

@racketblock[
d= Dictionary.newFrom(List[\a, 1, \b, 2, \c, 4]);
::
]
@section{argument}
 aCollection
any Object that responds to keysValuesDo (usually a List or an Array).

@section{discussion}
 
A new Dictionary can also be created from an array of link::Classes/Association::s:

@racketblock[
Dictionary.with(*[\a->1,\b->2,\c->3])
::

]
@section{INSTANCEMETHODS}
 

@section{subsection}
 Adding and Removing

@section{method}
 add
Add strong::anAssociation:: to the Dictionary. If the key value pair already exists in the Dictionary, the key's value will be replaced.

@racketblock[
(
d = Dictionary.new;
d.add(\monkey -> 0).postln;
d.add(\robot -> 1).postln;	// Add robot as a key with a value of 1
d.add(\monkey -> 2).postln;	// Replaces the value for the key monkey with 2
)
::

]
@section{method}
 put
Associate two objects and add them to the Dictionary.

@racketblock[
d = Dictionary.new;
d.put("abc", 10);

// using an event:
d = ();
d.put("abc", 10);
::
]
@section{argument}
 key
key to associate with object. This can be any objects, but is often a link::Classes/Symbol::.
@section{argument}
 value
an object

@section{method}
 removeAt
Remove the key and the value associated with it from the Dictionary.

@racketblock[
d = (monkey: 99);
d.removeAt(\monkey);
::

]
@section{method}
 putAll
Add all items of each argument to the dictionary.

@racketblock[
d = ();
d.putAll(Dictionary[\hello -> 9, \whello -> "world"], Dictionary["abd" -> 6]);
::
]
@section{argument}
  ... dictionaries
any Object that responds to keysValuesDo (usually a Dictionary).

@section{method}
 putPairs
Add all items to the dictionary, using them as key and value pairwise.

@racketblock[
d = ();
d.putPairs([\hello, 10, \whello, "lord", "abc", 7]);
::

]
@section{subsection}
 Accessing

@section{method}
 at
Access the value associated with the key.

@racketblock[
d = (robot: 99);
d.at(\robot);	// Get the value associated with key
d[\robot];	// different syntax, same behaviour
d.at(\monkey);	// Key doesn't exist: return Nil
::

]
@section{method}
 atFail
Access the value associated with the key. If the key does not exist, return the result of teletype::function::.

@section{method}
 keys
Return a link::Classes/Set:: of all keys.

@racketblock[
d = (hello: 9, whello: "world");
d.keys;
::

]
@section{method}
 values
Return a link::Classes/@section{List}
  of all values.

@racketblock[
d = (hello: 9, whello: "world");
d.values;
::

]
@section{method}
 atAll
Return an link::Classes/Array:: of all values for the given keys.

@racketblock[
d = (hello: 9, whello: "world", z: 99, c: 0.33);
d.atAll([\hello, \z, \hello, \c, \whello]);
::

]
@section{method}
 getPairs
Return an link::Classes/Array:: with all keys and values pairwise.

@racketblock[
d = (hello: 9, whello: 77, z: 99);
d.getPairs;
::

]
@section{method}
 associationAt
Access the link::Classes/Association:: that has the given key.

@racketblock[
d = (robot: 99);
d.associationAt(\robot);	// Get the value associated with key
::

]
@section{method}
 findKeyForValue
Try to find a given value and return its key.

@racketblock[
d = (hello: 1, whello: 1976);
d.findKeyForValue(1);
::

]
@section{method}
 matchAt
The dictionary's keys are used as conditions against which the arbitrary item is matched. See: link::Reference/matchItem::
@section{note}
 
if an item matches multiple criteria, the value returned is arbitrary. This is because a dictionary is an unordered collection. It's the user's responsibility to make sure that criteria are mutually exclusive.
::
@section{list}
 
## If the key is an object, the item will be matched by identity (if key === item, the value will be returned).
## If the key is a collection, the item is matched if it's contained in the collection.
## If the key is a function, the function is evaluated with the item as an argument and the item is matched if the function returns true.
::

@racketblock[
(
d = (
	0: \zero,
	\abc: \alpha,
	[1, 2, 3, 5, 8, 13, 21]: \fibonacci,
	{ |x| try { x.even } }: \even // try is needed because argument might not be a number
	);
);

d.matchAt(0)
d.matchAt(1)
d.matchAt(2)	// matches both 'fibonacci' and 'even', either may be returned
d.matchAt(4)
d.matchAt(\abc)
::

]
@section{method}
 trueAt
Returns link::Classes/True:: if the item at the key is true, otherwise false. This method is also valid in link::Classes/Object::.

@section{method}
 falseAt
Returns link::Classes/False:: if the item at the key is not true, otherwise true. This method is inherited from link::Classes/Object::.

@section{subsection}
 Iteration/Enumeration
Most methods for iteration work analogously to Dictionary's superclasses, see e.g. link::Classes/Collection::.

@section{method}
 do, collect, reject, select

@racketblock[
// do, collect, reject, select
d = Dictionary[\a -> "hello", \b -> "robot", \c -> [1, 2, 3]];
d = (a: "hello", b: "robot", c: [1, 2, 3]); // equivalent
d.do { |item, i| [item, i].postln };
d.collect { |item| item + 100 };
d.reject { |item| item.size > 4 };
d.select { |item| item.size > 4 };
::

]
@section{method}
 keysValuesDo
Iterate over the associations, and evaluate the function for each, passing key and value as argument.

@racketblock[
d = (a: "hello", b: "robot", c: [1, 2, 3]);
d.keysValuesDo { |key, value| postln("the key: " ++ key ++ " the value: " ++ value) };
::

]
@section{method}
 keysValuesChange
Iterate over the associations, and evaluate the function for each, passing key and value as argument. Replace the value with the return value from the function (similar to link::#-collect::, but modifies the dictionary strong::in place::).

@racketblock[
d = (a: "hello", b: "robot", c: [1, 2, 3]);
d.keysValuesChange { |key, value| "the key: " ++ key ++ " the value: " ++ value };
d;
::

]
@section{method}
 keysDo
Iterate over the associations, and evaluate the function for each, passing key as argument.

@racketblock[
d = (a: "hello", b: "robot", c: [1, 2, 3]);
d.keysDo { |key| postln("the key: " ++ key) };
::

]
@section{method}
 associationsDo
Iterate over the associations, and evaluate the function for each.

@racketblock[
d = (a: "hello", b: "robot", c: [1, 2, 3]);
d.associationsDo { |assoc| postln("the association: " ++ assoc) };
::

]
@section{method}
 pairsDo
Iterate over the associations, and evaluate the function for each, passing key and value as argument. Identical to link::#-keysValuesDo::

@section{method}
 invert
Return a new dictionary with all the values as keys and vice versa.

@racketblock[
d = (a: "hello", b: "robot", c: [1, 2, 3]);
d.invert;
::

]
@section{subsection}
 Other instance methods

@section{method}
 order
Return an array of keys which corresponds to the order of the values of the dictionary.

@racketblock[
d = (a: 5, b: 7, c: 1, d: 0);
d.order;
d.atAll(d.order);	// returns items in order
::

]
@section{method}
 powerset
Return the set of all subsets: here an array of all sub-dictionaries.

@racketblock[
d = (a: 5, b: 7, c: 1, d: 0);
d.powerset;
::

]
@section{method}
 merge
Combine two dictionaries into a new one by applying a function to each value. If strong::fill:: is true (default: true), values missing from one of them are kept as they are.

@racketblock[
d = (a: 5, b: 7, d: 0);
e = (a: 3, b: -3, c: 1);
merge(d, e, { |a, b| a + b });
merge(d, e, { |a, b| a + b }, false);
::
]
@section{argument}
 that
another dictionary.
@section{argument}
 func
a link::Classes/Function::.
@section{argument}
 fill
a link::Classes/Boolean::.

@section{method}
 blend
Blend two dictionaries into a new one by interpolating each value. If strong::fill:: is true (default: true), values missing from one of them are kept as they are.

@racketblock[
d = (a: 5, b: 7, d: 0);
e = (a: 3, b: -3, c: 1);
blend(d, e, 0.3);
blend(d, e, 0.3, false);

d = (a: 500, b: 0.001);
e = (a: 300, b: 0.1);
blend(d, e, 0.3, specs: (a: \freq, b: \rq));
::
]
@section{argument}
 that
another dictionary.
@section{argument}
 blend
the blend ratio as a link::Classes/Float:: between 0.0 and 1.0.
@section{argument}
 fill
a link::Classes/Boolean::.
@section{argument}
 specs
a dictionary of link::Classes/Spec::s that are applied to each before blending.

@section{method}
 asSortedArray
Return the values in a sorted array of key value pairs.

@racketblock[
d = (a: 5, b: 7, c: 1, d: 0);
d.asSortedArray;
::

]
@section{method}
 asKeyValuePairs
Return the values in an array of alternating key value pairs.

@racketblock[
d = (a: 5, b: 7, c: 1, d: 0);
d.asKeyValuePairs;
::

]
@section{method}
 embedInStream

@section{argument}
 event
The inval, usually in an event stream. See also link::Classes/Event::.

If the event is not nil, yields a copy, adding all the elements of the receiver event (this leaves the receiver unchanged). If it is nil, return the receiver.


@racketblock[
a = (note: 2);
b = (note: [3, 5]);
Pseq([a, b]).play;
::

If a key "embedInStream" is given, use this function instead. The behaviour of the event can be configured easily this way.

The arguments event (the receiver) and inevent (the inevent) are passed to the function. ]
@section{note}
 In infinite patterns, you strong::must:: call yield or embedInStream in the function, otherwise it will loop forever.::



@racketblock[
(
a = (
	pattern: Pbind(\note, Pgeom(1, 1.1, { 20.rand }), \dur, 0.05),
	embedInStream: { |event, inevent| event[\pattern].embedInStream(inevent) }
);
b = (note: [3, 5]);
c = (freq: 402, dur: 0.3);
Prand([a, b, c], inf).trace.play;
)

// change the events while playing
c[\freq] = [900, 1002, 1102];
c[\freq] = [200, 101, 1102];
::


A generator for dictionaries:

]

@racketblock[
(
d = (
	a: 5, b: 7, c: 1,
	rout: Routine { |inval|
		inf.do { |i|
			var event = d.copy.put(\count, i);
			inval = event.embedInStream(inval);
		}
	}
);
)

// draw new values
d.rout.((z:999));
d.rout.((z:1, a:0));
d.rout.(());
::

]
@section{SECTION}
 Overview

@section{subsection}
 The Difference between Dictionary, IdentityDictionary, Environment, and Event

Often, the subclass link::Classes/Event:: is used as an IdentityDictionary, because there is a syntactical shortcut:

@racketblock[
a = (foo: 7);	// return a new Event.
a.put(\foo, 2.718);
a.at(\foo);
a[\foo] = 3.5;	// different syntax for put
::

Event, Environment and IdentityDictionary differ mainly insofar from Dictionary as the strong::keys:: are taken to be identical (===) objects (see IdentityDictionary), instead of equal (==) objects. By consequence, the subclasses are also faster for indexing. Apart from this, the subclasses add specific functionality only. Because of its very common usage, the examples often use the shortcut for the subclass Event.
]

@racketblock[
// preliminary identity and equality of strings and symbols
"hello" == "hello";	// true, but
"hello" === "hello";	// false. However:
\hello === \hello;	// true

// compare
Dictionary["hello" -> 0, "hello" -> 1]; // Dictionary[ (hello -> 1) ]
("hello": 0, "hello": 1); // ( "hello": 1, "hello": 0 )

// for symbols as keys, the behaviour is identical:
Dictionary[\hello -> 1, \hello -> 0];
( \hello: 1, \hello: 0 );
::


]


