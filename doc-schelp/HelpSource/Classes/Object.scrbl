#lang scribble/manual
@(require (for-label racket))

@title{Object}
 abstract superclass of all objects@section{categories}
  Core>Kernel, Language>OOP

@section{related}
  Classes/Class, Guides/Intro-to-Objects, Reference/Classes

@section{description}

Object is the root class of all other classes. All objects are indirect instances of class Object. We call the "receiver" the object the message is sent to: 
@racketblock[receiver.method(argument)::.


]
@section{classmethods}
 
@section{private}
  prNewCopyArgs, prNew

@section{method}
 readArchive

Read in an object from a text archive.


@racketblock[
(
a = Array.fill(100, { 100.rand });
a.writeArchive(PathName.tmp ++ "myArray");
b = Object.readArchive(PathName.tmp ++ "myArray");
a == b; // true
)

/////////

// closed Function
(
f = { 1 + 2 };
f.writeArchive(PathName.tmp ++ "myFunc"); // succeeds
)
// open Function
(
var num;
num = 2;
f = { num + 2 };
f.writeArchive(PathName.tmp ++ "myFunc"); // fails
)
::

]
@section{argument}
 pathname
A String containing the archive file's path.

@section{method}
 new

Create a new instance. The creation of new instances of any Object actually happens in this method (or with 
@racketblock[newCopyArgs::) when it is called by a child class. See link::Guides/WritingClasses::.

]
@section{method}
 newCopyArgs

Creates a new instance and copies the arguments to the instance variables in the order that the variables were defined. If the superclass's 
@racketblock[new:: method requires arguments, the first arguments passed to ]

@racketblock[newCopyArgs:: will be passed on to that method, and the following arguments will be copied to this class's instance variables. Class variables are ignored.

]

@racketblock[
MyClass {
	var a, b, c;

	*new {
		arg arg1, arg2, arg3;
		// will copy arg1, arg2, arg3 to variables a, b, c
		^super.newCopyArgs(arg1, arg2, arg3);
	}
}
::

]
@section{instancemethods}
 
@section{private}
 addFunc, addFuncTo, removeFunc, removeFuncFrom

@section{subsection}
 Class Membership

@section{method}
 class

Answer the class of the receiver.


@racketblock[
5.class;
::

]
@section{method}
 respondsTo

Answer a link::Classes/Boolean:: whether the receiver understands the message selector.


@racketblock[
5.respondsTo('+'); // true
5.respondsTo('indexOf'); // false
::


]
@section{argument}
 aSymbol
A selector name. Must be a link::Classes/Symbol::.

@section{method}
 isKindOf

Answer a Boolean indicating whether the receiver is a direct or indirect instance of aClass. Use of this message in code must be questioned, because it often indicates a missed opportunity to exploit object polymorphism.


@racketblock[
5.isKindOf(Number); // true
5.isKindOf(String); // false
::

]
@section{method}
 isMemberOf

Answer a Boolean whether the receiver is a direct instance of aClass. Use of this message in code is almost always a design mistake.


@racketblock[
5.isMemberOf(Number); // false
5.isMemberOf(Integer); // true
::

]
@section{subsection}
 Accessing

@section{method}
 size

Different classes respond to this message differently. Object always returns 0.

@section{subsection}
 Copying

@section{method}
 copy

Make a copy of the receiver. The implementation of this message depends on the object's class.  In class Object, copy calls shallowCopy.

@section{method}
 shallowCopy

Makes a copy of the object. The copy's named and indexed instance variables refer to the same objects as the receiver.

@section{method}
 deepCopy

Recursively copies the object and all of the objects contained in the instance variables, and so on down the structure. This method works with cyclic graphs.

@section{method}
 copyImmutable

If object is immutable then return a shallow copy, else return receiver.

@section{subsection}
 Conversion

To convert an object of a certain class into a similar object of another class, Object provides a number of methods.

@section{method}
 as

Returns a similar new Object of a different class.


@racketblock[
[1, 2, 3].as(Set);
Pwhite(0.0, 1.0, 10).as(Set);
::

]
@section{method}
 asArray

Returns an Array with the receiver, unless it is an Array already.


@racketblock[
[1, 2, 3].asArray;
5.asArray;
::

]
@section{method}
 asCompileString

Returns a String that can be interpreted to reconstruct a copy of the receiver. For the complementary method, see link::Classes/String#-interpret::.


@racketblock[
a = { 10.do { 10.postln } };
a.asCompileString.postcs;
a.postcs;
::

]
@section{method}
 cs

Shorthand for link::#-asCompileString::.


@racketblock[
{ 10.do { 10.postln } }.cs;
"Strings don't post with surrounding quotes.".cs;
::

]
@section{subsection}
 Archiving

Object implements methods for writing and retrieving objects from disk. Note that you cannot archive instances of Thread and its subclasses (i.e. Routine), or open Functions (i.e., a Function which refers to variables from outside its own scope).

@section{method}
 writeArchive

Write an object to disk as a text archive.

@section{argument}
 pathname
A String containing the resulting file's path.


@section{subsection}
 Equality and Identity

@section{method}
 ==

Answer whether the receiver equals anotherObject. The definition of equality depends on the class of the receiver. The default implementation in Object is to answer if the two objects are identical.

@section{note}
  Whenever == is overridden in a class, hash should be overridden as well.::


@racketblock[
5.0 == 5; // true
5.0 === 5; // false
a = [1, 2, 3]; b = [1, 2, 3];
a == b; // equal
a === b; // not identical
"worth trying" == "worth trying"; // equal
::


]
@section{method}
 ===

Answer whether the receiver is the exact same object as anotherObject.


@racketblock[
5.0 === 5; // false
"worth trying" === "worth trying"; // not identical
'worth trying' === 'worth trying'; // identical (symbols are unique)
::

]
@section{method}
 !=

Answer whether the receiver does not equal anotherObject. The default implementation in Object is to answer if the two objects are not identical (see below).

@section{method}
 fuzzyEqual

Returns the degree of equality between two objects with regard to a given precision. Objects to compare must support max, substraction, and division.


@racketblock[
5.0.fuzzyEqual(5.0, 0.5); // 1 - full equality
5.25.fuzzyEqual(5.0, 0.5); // 0.5 - 50 % equality
5.9.fuzzyEqual(5.0, 0.5); // 0 - no equality
::

]
@section{returns}
 A number in the range 0 to 1.

@section{method}
 compareObject

Tests if two Objects (of the same class) are the same in a certain respect: It returns true if instVarNames are equal in both. If none are given, all instance variables are tested (see also: link::#-instVarHash::)


@racketblock[
a = Pseq([1, 2, 3], inf); b = Pseq([100, 200, 300], inf);
a.compareObject(b, [\repeats]); // true
a.compareObject(b, [\list]); // false
::

]
@section{method}
 hash

Answer a code used to index into a hash table. This is used by Dictionary and Set and their subclasses to implement fast object lookup.  Objects which are equal == should have the same hash values. Whenever == is overridden in a class, hash should be overridden as well.


@racketblock[
a = "worth trying"; b = "worth trying";
a.hash;
b.hash;
::

]
@section{method}
 identityHash

Answer a code used to index into a hash table. This method is implemented by a primitive and is not overridden. Objects which are identical === should have the same hash values.


@racketblock[
a = "worth trying"; b = "worth trying";
a.identityHash;
b.identityHash;
::

]
@section{method}
 instVarHash

Returns a combined hash value for the object's instance variables and the object's class. If none are given, all instance variables are tested (see also: link::#-compareObject::).



@racketblock[
a = Pseq([1, 2, 3], inf); b = Pseq([100, 200, 300], inf);

a.instVarHash([\repeats]); // same
b.instVarHash([\repeats]);

a.instVarHash([\list]); // different
b.instVarHash([\list]);

a = Pseq([1, 2, 3], inf); b = Prand([1, 2, 3], inf);
a.instVarHash([\list]); // different
b.instVarHash([\list]);
::


]
@section{subsection}
 Testing

@section{method}
 isNil

Answer a Boolean indicating whether the receiver is nil.

@section{method}
 notNil

Answer a Boolean indicating whether the receiver is not nil.

@section{method}
 isNumber

Answer a Boolean indicating whether the receiver is an instance of Number.

@section{method}
 isInteger

Answer a Boolean indicating whether the receiver is an instance of Integer.

@section{method}
 isFloat

Answer a Boolean indicating whether the receiver is an instance of Float.

@section{method}
 ?

If the receiver is nil then answer anObject, otherwise answer the receiver.

@section{method}
 ??

If the receiver is nil, evaluate the link::Classes/Function:: and return the result.

@section{method}
 !?

If the receiver is not nil, evaluate the link::Classes/Function:: passing in the receiver as argument and return the result, otherwise return nil.

@section{note}
 
The function will be inlined if it contains no variables or arguments.
::

@section{discussion}
 
This method allow building up chains of actions to be performed on an object (possibly across several methods) without having to check if the object is nil or not. After all the desired actions are performed, link::#-??:: can be used to check if result the result is nil and supply a default value in that case.

Examples:

@racketblock[
x !? ( _ * 3 ) ?? { "It was a nil, so I give a default value".postln; Point(1,1) }
::
With ]

@racketblock[x = nil::, this will result in:
teletype::
It was a nil, so I give a default value
Point( 1, 1 )
::
But if ]

@racketblock[x = Point(3,4)::, the result will be:
teletype::
Point( 9, 12 )
::

Nested nil checks:
]

@racketblock[
(
x = nil;
y = Point(3,4);
z = Point(5,6);
x !? { |x| y !? { |y| z !? { |z|  x.rho * y.rho * z.rho } } }
)
::
Results in teletype::nil::
]

@racketblock[
(
x = Point(1,2);
y = Point(3,4);
z = Point(5,6);
x !? { |x| y !? { |y| z !? {  |z| x.rho * y.rho * z.rho } } }
)
::
Results in teletype::87.321245982865::

]
@section{method}
 pointsTo

Returns true if receiver has a direct reference to obj.


@racketblock[
a = 9;
b = [1, a, 6, 8];
c = [1, b, 5];
c.pointsto(b); // true
c.pointsto(a); // false
::

]
@section{method}
 mutable

Returns true if receiver is mutable.


@racketblock[
a = #[1, 2, 3]; b = [1, 2, 3];
a.mutable; // false
b.mutable; // true
::

]
@section{method}
 frozen

Returns true if receiver is frozen.

@section{method}
 switch

Object implements a switch method which allows for conditional evaluation with multiple cases. These are implemented as pairs of test objects (tested using if this == test.value) and corresponding functions to be evaluated if true. In order for switch to be inlined (and thus be as efficient as nested if statements) the matching values must be literal Integers, Floats, Chars, Symbols and the functions must have no variables or arguments.

@section{discussion}
 

@racketblock[
(
var x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
switch (z.choose.postln,
	1,   { \no },
	1.1, { \wrong },
	1.3, { \wrong },
	1.5, { \wrong },
	2,   { \wrong },
	0,   { \true }
).postln;
)
::

or:

]

@racketblock[
(
var x, z;
z = [0, 1, 1.1, 1.3, 1.5, 2];
x = switch (z.choose)
	{1}   { \no }
	{1.1} { \wrong }
	{1.3} { \wrong }
	{1.5} { \wrong }
	{2}   { \wrong }
	{0}   { \true };
x.postln;
)
::

]
@section{subsection}
 Messaging

Instead of directly sending a method to an object, a method may be invoked given a method selector only (a Symbol). The other arguments may be provided by passing them directly, from an environment. If it is not known whether the receiver implements the method, tryPerform only sends if it does, and superPerform invokes the method of the superclass.


@section{method}
 perform

The selector argument must be a Symbol. Sends the method named by the selector with the given arguments to the receiver.

If the first argument is an Array or List, this method behaves like 
@racketblock[performMsg::. However, this usage is discouraged, and ]

@racketblock[performMsg:: ought to be used instead.

]
@section{method}
 performList

The selector argument must be a Symbol. Sends the method named by the selector with the given arguments to the receiver. If the last argument is a List or an Array, then its elements are unpacked and passed as arguments.


@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performList(\value, [1, 2, 3]);
::

]
@section{method}
 performMsg

The argument must be a List or Array whose first element is a Symbol representing a method selector. The remaining elements are unpacked and passed as arguments to the method named by the selector.


@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performMsg([\value, 1, 2, 3]);
::

]
@section{method}
 performWithEnvir

@section{argument}
  selector
A Symbol representing a method selector.
@section{argument}
  envir
The remaining arguments derived from the environment and passed as arguments to the method named by the selector.
@section{discussion}
 

@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performWithEnvir(\value, (a: 1, c: 3, d: 4, b: 2));
::

]
@section{method}
 performKeyValuePairs

@section{argument}
  selector
A Symbol representing a method selector.
@section{argument}
  pairs
Array or List with key-value pairs.
@section{discussion}
 

@racketblock[
a = { |a, b, c| postf("% plus % plus % is %\n", a, b, c, a + b + c); "" };
a.performKeyValuePairs(\value, [\a, 1, \b, 2, \c, 3, \d, 4]);
::

]
@section{method}
 tryPerform

Like 'perform', but tryPerform passes the method to the receiver only if the receiver understands the method name. If the receiver doesn't implement that method, the result is nil. Note that this does not catch errors like 'try' does (see Exception). If the receiver does have a matching method but that method throws an error, execution will halt. But, 'tryPerform' is faster than 'try'.


@racketblock[
(a: 1, b: 2, c: 3).tryPerform(\keysValuesDo, { |key, value| [key, value].postln });

// Array does not understand keysValuesDo -- result is nil
[1, 2, 3].tryPerform(\keysValuesDo, { |key, value| [key, value].postln });

// Error occurs within keysValuesDo -- error is thrown back to halt execution
(a: 1, b: 2, c: 3).tryPerform(\keysValuesDo, { |key, value| [key, value].flippityblargh });
::

]
@section{method}
 superPerform

Like perform, superPerform calls a method, however it calls the method on the superclass.
selector: A Symbol representing a method selector.
args: Method arguments.


@section{method}
 superPerformList

Like performList, superPerformList calls a method, however it calls the method on the superclass.
selector: A Symbol representing a method selector.
args: Method arguments. If the last argument is a List or an Array, then its elements are unpacked and passed as arguments.

@section{method}
 multiChannelPerform
Perform selector with multichannel expansion. See also: link::Guides/Multichannel-Expansion::.

@section{argument}
  selector
A Symbol representing a method selector.
@section{argument}
  ... args
Method arguments which, if they contain an array, will call the method multiple times for each sub-element.
@section{discussion}
 

@racketblock[
a = { |a, b, c| format("% plus % times % is %", a, b, c, a + b * c).quote; };
a.multiChannelPerform(\value, [1, 10, 100, 1000], [2, 7, 9], [3, 7]);

["foo","bar"].multiChannelPerform('++',["l","bro","t"]);
::

]
@section{subsection}
 Unique Methods

Method definitions not yet implemented may be added to an Object instance.

@section{method}
 addUniqueMethod

Add a unique method.


@racketblock[
a = 5;
a.addUniqueMethod(\sayHello, { |to| "hello " ++ to ++ ", I am 5" });
a.sayHello;
::

]
@section{method}
 removeUniqueMethod

Remove a unique method.


@racketblock[
a.removeUniqueMethod(\sayHello);
a.sayHello;
::

]
@section{method}
 removeUniqueMethods

Remove all unique methods of an Object.

@section{subsection}
 Dependancy

@section{method}
 addDependant

Add aDependant to the receiver's list of dependants.

@section{method}
 removeDependant

Remove aDependant from the receiver's list of dependants.

@section{method}
 dependants

Returns an IdentitySet of all dependants of the receiver.

@section{method}
 changed

Notify the receiver's dependants that the receiver has changed. The object making the change should be passed as theChanger.

@section{method}
 update

An object upon which the receiver depends has changed. theChanged is the object that changed and theChanger is the object that made the change.

@section{method}
 release

Remove all dependants of the receiver. Any object that has had dependants added must be released in order for it or its dependants to get garbage collected.

@section{subsection}
 Error Support

Object implements a number of methods which throw instances of Error. A number of methods (e.g. doesNotUnderstand) are 'private' and do not normally need to be called directly in user code. Others, such as those documented below can be useful for purposes such as object oriented design (e.g. to define an abstract interface which will be implemented in subclasses) and deprecation of methods. The reserved keyword thisMethod can be used to refer to the enclosing method. See also Method and Function (for exception handling).

@section{method}
 throw

Throws the receiver as an Exception, which may or may not be caught and handled by any enclosing Function.

@section{method}
 subclassResponsibility

Throws a SubclassResponsibilityError. Use this to indicate that this method should be defined in all subclasses of the receiver.

@section{discussion}
 

@racketblock[
someMethod {
	this.subclassResponsibility(thisMethod);
}
::

]
@section{method}
 shouldNotImplement

Throws a ShouldNotImplementError. Use this to indicate that this inherited method should not be defined or used in the receiver.

@section{method}
 deprecated

Throws a DeprecatedError. Use this to indicate that the enclosing method has been replaced by a better one (possibly in another class), and that it will likely be removed in the future. Unlike other errors, DeprecatedError only halts execution if 
@racketblock[Error.debug == true::. In all cases it posts a warning indicating that the method is deprecated and what is the recommended alternative.

]
@section{discussion}
 

@racketblock[
foo {
	this.deprecated(thisMethod, ThisOrSomeOtherObject.findMethod(\foo);
	... // execution of this method will continue unless Error.debug == true
}

// For a class method:
*bar {
	this.deprecated(thisMethod, OtherClass.class.findMethod(\bar));
	...
}
::

]
@section{subsection}
 Printing and Introspection

@section{method}
 post

Print a string representation of the receiver to the post window.

@racketblock[
"hello".post; "hello".post; "";
::

]
@section{method}
 postln

Print a string representation of the receiver followed by a newline.

@racketblock[
"hello".postln; "hello".postln; "";
::

]
@section{method}
 postc

Print a string representation of the receiver preceded by comments.

@racketblock[
"hello".postc; "hello".postc; "";
::

]
@section{method}
 postcln

Print a string representation of the receiver preceded by comments, followed by a newline.

@racketblock[
"hello".postcln; "hello".postcln; "";
::

]
@section{method}
 postcs

Print the compile string representation of the receiver, followed by a newline.

@racketblock[
"hello".postcs; "hello".postcs; "";
::

]
@section{method}
 dump

Print a detailed low level representation of the receiver to the post window.

@racketblock[
List[1, 2, 3].dump;
::

]
@section{subsection}
 System Information

@section{method}
 gcInfo

Posts garbage collector information in a table format.

@section{discussion}
 
@section{list}
 
## flips: the number of times the GC "flipped", i.e. when it finished incremental scanning of all reachable objects
## collects: the number of partial collections performed
## nalloc: total number of allocations
## alloc: total allocation in bytes
## grey: the number of "grey" objects, i.e. objects that point to reachable objects and are not determined to be (un)reachable yet
::

Then for each size class: numer of black, white and free objects, total number of objects and the total set size.


@racketblock[
flips 241  collects 689096   nalloc 40173511   alloc 322496998   grey 346541
0  bwf t sz:    882      0 368573   369455    2955640
1  bwf t sz:   6197    122 5702377   5708696   91339136
2  bwf t sz:    947      4 1500009   1500960   48030720
3  bwf t sz:   8056  65201 301800   375057   24003648
4  bwf t sz:   4047    145   3457     7649     979072
5  bwf t sz:    422      1    431      854     218624
6  bwf t sz:    124      2     72      198     101376
7  bwf t sz: 153504      1      0   153505   157189120
8  bwf t sz:     22      0      0       22      45056
9  bwf t sz:      5      0      0        5      20480
10  bwf t sz:      5      0      0        5      40960
12  bwf t sz:      2      0      0        2      65536
13  bwf t sz:      1      0      0        1      65536
19  bwf t sz:      1      0      3        4   16777216
tot bwf t sz: 174215  65476 7876722   8116413   341832120
::


You can also query the amount of free memory with ]

@racketblock[Object.totalFree:: and dump the currently grey objects with ]

@racketblock[Object.dumpGrey::. More memory status methods are: largestFreeBlock, gcDumpSet, and gcSanity.

]
@section{subsection}
 Iteration

@section{method}
 do

Object evaluates the function with itself as an argument, returning the result. Different classes respond to this message differently.
@section{discussion}
 

@racketblock[
f = { |x, i| [x, i].postln; };
[1, 2, 3].do(f); // Array.do
10.do(f); // Integer.do
($Q).do(f); // Object.do
::

]
@section{method}
 generate

Object iterates by the message do, sent to the receiver.
This method is used internally by list comprehensions.

@section{method}
 dup

Duplicates the receiver n times, returning an array of n copies. Different classes respond to this message differently.  The shortcut "!" can be used in place.
@section{discussion}
 

@racketblock[
8.dup(10);
8 ! 10; // same as above
x = [[1], [2], [3]].dup(5);
x[0] === x[1]; // false: copies receiver.
x[0][0] === x[1][0] // true: doesn't deepCopy receiver
{ 1.0.rand }.dup(5) // other objects respond differently to dup
::

]
@section{subsection}
  Scheduling

@section{method}
  awake

This method is called by a link::Classes/Clock:: on which the object was
scheduled when its scheduling time is up. It calls link::#-next::, passing
on the scheduling time in beats as an argument.

@section{argument}
  beats
The scheduling time in beats. This is equal to the current logical time
(link::Classes/Thread#-beats::).

@section{argument}
  seconds
The scheduling time in seconds. This is equal to the current logical time
(link::Classes/Thread#-seconds::).

@section{argument}
  clock
The clock on which the object was scheduled.


@section{subsection}
  Stream Support

@section{method}
  next

Does nothing; simply returns the object itself.

@section{method}
  reset

Does nothing; simply returns the object itself.





@section{subsection}
 Routine Support

Objects support the basic interface of Stream, just returning itself in response to the following messages:
next, reset, stop, free, clear, removedFromScheduler, asStream.

@section{method}
 yield

Must be called from inside a Routine. Yields control to the calling thread. The receiver is the result passed to the calling thread's method. The result of yield will be the value passed to the Routine's next method the next time it is called.

@section{method}
 yieldAndReset

Must be called from inside a Routine. Yields control to the calling thread. The receiver is the result passed to the calling thread's method. The Routine is reset so that the next time it is called, it will start from the beginning. yieldAndReset never returns within the Routine.

@section{method}
 alwaysYield

Must be called from inside a Routine. Yields control to the calling thread. The receiver is the result passed to the calling thread's method. The Routine, when called subsequently will always yield the receiver until it is reset. alwaysYield never returns within the Routine.

@section{method}
 embedInStream

Yields the receiver

@section{method}
 idle

within a routine, return values (the receiver) until this time is over. (see also link::Classes/Routine#play::)
Time is measured relative to the thread's clock.

@racketblock[
a = Routine { 1.yield; 0.idle(3); 400.yield };
fork { loop { a.next.postln; 0.5.wait } };
::

]
@section{method}
 iter

Returns a link::Classes/OneShotStream:: with the receiver as return value.

@racketblock[
a = 9.iter;
a.nextN(4);
::

]
@section{method}
 cyc

Embeds the receiver in the stream n times (default: inf), each time resetting it.

@racketblock[
a = 9.cyc(2);
a.nextN(4);
::

]
@section{method}
 fin

Calls next with the receiver n times only (default: 1), yielding the result.

@racketblock[
a = (10..0).iter.fin(2);
a.nextN(4);
::

]
@section{method}
 repeat

Repeatedly embeds the receiver in the stream using a Pn (may thus be used for patterns and other objects alike)

@racketblock[
a = (0..3).iter.repeat(2);
a.nextN(9)
::

]
@section{method}
 loop

Indefinitely embeds the receiver in the stream


@racketblock[
a = (0..3).iter.loop;
a.nextN(9)
::

]
@section{method}
  nextN

Returns an array with the results of calling link::#-next:: a given number of times

@section{argument}
  n

Number of message calls

@section{argument}
  inval

argument passed to the next message


@racketblock[
Routine { inf.do { |i| i.rand.yield } }.nextN(8)
::


]
@section{method}
 streamArg
	Dependent on whether an object that is passed to a stream the object will behave differently: it may be embedded in the stream or used as stream directly.

	This method allows to switch between the two behaviors. For efficiency, the subclasses link::Classes/Pattern:: and link::Classes/Stream:: implement this method simply as "asStream".
	@section{argument}
 embed
	If set to true, the object embeds itself into the stream (and thus return only once). If set to false, it returns itself forever. For simplicity, subclasses implement this method without this switch.



@racketblock[
// embedding an event
a = (z: 77);
b = Pset(\y, 8, a.streamArg(true)).asStream;
c = Pset(\y, 8, a.streamArg(false)).asStream;
b.nextN(3, ()); // this ends
c.nextN(3, ()); // this loops

// embedding a pattern
a = Pbind(\note, Pseq([1, 2]));
b = Pset(\y, 8, a.streamArg(true)).asStream;
c = Pset(\y, 8, a.streamArg(false)).asStream;
b.nextN(3, ()); // this ends
c.nextN(3, ()); // this ends, too
::


]
@section{method}
 addFunc
@section{method}
 addFuncTo
@section{method}
 removeFunc
@section{method}
 removeFuncFrom


The messages link::Classes/Function#-addFunc::  link::Classes/Function#-addFuncTo::, link::Classes/Function#-removeFunc::, link::Classes/Function#-removeFuncFrom:: are supported by Object.

@section{method}
 instill
@section{method}
 obtain
The messages link::Classes/SequenceableCollection#-instill:: and link::Classes/SequenceableCollection#-obtain::, are supported by Object.

@section{subsection}
  Math Support
@section{method}
 blend
Lineraly interpolate between this and argument

@racketblock[
blend(10, 100, 0.3);
blend([1, 2, 3], [1, 3, 4], 0.5);
blend((a: 6, b: 7), (a: 0, b: [1, 2], c: 9), 0.5);
::

]


