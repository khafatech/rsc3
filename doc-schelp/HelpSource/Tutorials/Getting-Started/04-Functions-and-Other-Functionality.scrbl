#lang scribble/manual
@(require (for-label racket))

@title{04. Functions and Other Functionality}
 Getting Started With SuperCollider@section{categories}
  Tutorials>Getting-Started
@section{related}
  Tutorials/Getting-Started/00-Getting-Started-With-SC

The easiest way to get sound from SC is to use a Function. Below is a simple example of this. Execute this (after making sure the server is booted), and when you're sick of it, press Cmd - . (that's hold down the command key and press the period or fullstop key) to stop the sound. This will always stop all sound in SC. You'll be using it a lot, so commit it to memory.


@racketblock[
{ [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.play;
::

Not too inspiring? Don't worry, we're just getting started, and this is just a simple example to demonstrate Functions and sound. We'll take it apart a bit below.

Before we get to doing that though, let's learn a little about Functions in general.

A Function is just a reusable bit of code. You define a Function by enclosing code in 'curly brackets': { }. Here's an example:

]

@racketblock[
f = { "Function evaluated".postln; };
::

The stuff within the curly brackets is what will get executed each time you reuse, or evaluate the Function. Note that this is written like an equation, i.e. ]

@racketblock[f = {...}::. This is not an equation in the mathematical sense, it's what's called an assignment. Basically it allows me to name the Function I've created, by storing it in a variable called ]

@racketblock[f::. A variable is just a name representing a slot in which we can store things, such as a Function, a number, a list, etc. Execute the following lines one at a time and watch the post window:

]

@racketblock[
f = { "Function evaluated".postln; };
f;
::

Both times it should say ]

@racketblock['a Function'::. Now whenever we want to refer to our Function we can just use the letter f. That's in fact what makes it reusable! Otherwise we'd need to type the Function in every time.

So how do we reuse it? Execute the following lines one at a time and watch the post window:

]

@racketblock[
f = { "Function evaluated".postln; };
f.value;
f.value;
f.value;
::

Our Function is an object, (i.e a thing that does something or represents something), which we have defined and stored in the variable ]

@racketblock[f::. The bit of code that says ]

@racketblock['.value':: says evaluate this function now. This is an example of sending a message to an object. This follows the syntax someObject.someMessage. The dot must go in between.

Now this next bit is a little bit tricky. In a given object, each emphasis::message:: calls (calls means executes) a particular emphasis]
@section{method}
 . Different types of objects may have methods with the same name, and thus respond to the same message in different ways. Whoah, get that? Read it again slowly, as this is pretty important:

emphasis::Different types of objects may have methods with the same name, and thus respond to the same message in different ways.::

What's interesting about this is that the actual methods may differ in what they do, but as long as they implement a method with that name, they become interchangeable in your code.

A good example is 'value'. All objects in SC respond to the message 'value'. When you 'call' a method, it always 'returns' something, such as a value or a result. When you call the method 'value' on a Function it will evaluate and return the result of its last line of code. The example below will return the number 5.


@racketblock[
f = { "Evaluating...".postln; 2 + 3; };
f.value;
::

Often methods simply return the object itself. This is the case with most objects and the message 'value'. The example below demonstrates this. (Everything to the right of the ]

@racketblock[//:: is a 'comment', which means that SC just ignores it. Comments are a good idea to make your code clearer.)

]

@racketblock[
f = 3;			// Here I make f equal to a number
f.value;		// Post window says: 3, i.e it returns itself
f.value;		// Still says 3

f = { 3.0.rand; };	// Here it's a Function.
f.value;		// 3.0.rand means return a random value from 0 to 3.0 exclusive.
f.value;		// something different
f.value;		// something different
::

This means that by using the 'value' method Functions and other objects can be interchangeable in your code. This is an example of emphasis::polymorphism::, which is one of the powerful features of what's called Object Oriented Programming. Polymorphism just means that different objects are interchangeable (at least providing they return something sensible for what you're doing) if they respond to the same message. Object Oriented Programming (or OOP, as it's called for short) just means programming with objects. Simple, yes? Here's another short example showing this in action:

]

@racketblock[
f = { arg a; a.value + 3 };	// call 'value' on the arg; polymorphism awaits!
f.value(3);			// 3.value = 3, so this returns 3 + 3 = 6
g = { 3.0.rand; };
f.value(g);			// here the arg is a Function. Cool, huh?
f.value(g);			// try it again, different result
::

Start to see how this could be useful?

Functions can also have what are called arguments. These are values which are passed into the Function when it is evaluated. The example below demonstrates how this works. See if you can guess what the result will be before executing it.

]

@racketblock[
(
f = { arg a, b;
	a - b;
};
f.value(5, 3);
)
::

Arguments are declared at the beginning of the Function, using the keyword ]

@racketblock['arg'::. You can then refer to them just like variables. When you call value on a Function, you can pass in arguments, in order, by putting them in parentheses: ]

@racketblock[someFunc.value(arg1, arg2)::. This is the same with any method that takes arguments, not just value.

You can specify different orders by using what are called keyword arguments:

]

@racketblock[
f = { arg a, b; a / b; };	// '/' means divide
f.value(10, 2);			// regular style
f.value(b: 2, a: 10);		// keyword style
::

You can mix regular and keyword style if you like, but the regular args must come first:

]

@racketblock[
f = { arg a, b, c, d; (a + b) * c - d };
f.value(2, c:3, b:4, d: 1); // 2 + 4 * 3 - 1
::

(Note that SC has no operator precedence, i.e. math operations are done in order, and division and multiplication are not done first. To force an order use parentheses. e.g. 4 + (2* 8) )

Sometimes it's useful to set default values for arguments. You can do this like so:

]

@racketblock[
f = { arg a, b = 2; a + b; };
f.value(2); 			// 2 + 2
::

Default values must be what are called literals. Literals are basically numbers, strings, symbols (more on these later), or collections of them. Don't worry if that doesn't totally make sense, it will become clearer as we go on.

There is an alternate way to specify args, which is to enclose them within two vertical lines. (On most keyboards the vertical line symbol is Shift-\ ) The following two Functions are equivalent:

]

@racketblock[
f = { arg a, b; a + b; };
g = { |a, b| a + b; };
f.value(2, 2);
g.value(2, 2);
::

Why have two different ways? Well some people like the second one better and consider it a shortcut. SC has a number of syntax shortcuts like this, which can make writing code a little faster. In any case you will encounter both forms, so you need to be aware of them.

You can also have variables in a Function. These you need to declare at the beginning of the Function, just after the args, using the keyword ]

@racketblock['var'::.

]

@racketblock[
(
f = { arg a, b;
	var firstResult, finalResult;
	firstResult = a + b;
	finalResult = firstResult * 2;
	finalResult;
};
f.value(2, 3);	// this will return (2 + 3) * 2 = 10
)
::

Variable and argument names can consist of letters and numbers, but must begin with a lower-case letter and cannot contain spaces.

Variables are only valid for what is called their scope. The scope of a variable declared in a Function is that Function, i.e. the area between the two curly brackets. Execute these one at a time:

]

@racketblock[
f = { var foo; foo = 3; foo; };
f.value;
foo;			// this will cause an error as 'foo' is only valid within f.
::

You can also declare variables at the top of any block of code which you execute altogether (i.e. by selecting it all). In such a case that block of code is the variable's scope. Execute the block (in parentheses) and then the last line.

]

@racketblock[
(
var myFunc;
myFunc = { |input| input.postln; };
myFunc.value("foo");	// arg is a String
myFunc.value("bar");
)

myFunc;			// throws an error
::

You may be wondering why we haven't needed to declare variables like ]

@racketblock[f::, and why they don't seem to have any particular scope (i.e. they keep their values even when executing code one line at a time). The letters a to z are what are called interpreter variables. These are pre-declared when you start up SC, and have an unlimited, or 'global', scope. This makes them useful for quick tests or examples. You've already encountered one of these, the variable 's', which you'll recall by default refers to the localhost server.

For more information see:

link::Reference/Functions::, link::Classes/Function::, link::Reference/Assignment::, link::Guides/Intro-to-Objects::, link::Reference/Literals::, link::Reference/Scope::

____________________

This document is part of the tutorial strong::Getting Started With SuperCollider::.

Click here to go on to the next section: link::Tutorials/Getting-Started/05-Functions-and-Sound::

Click here to return to the table of Contents: link::Tutorials/Getting-Started/00-Getting-Started-With-SC::
]


