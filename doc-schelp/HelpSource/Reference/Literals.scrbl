#lang scribble/manual
@(require (for-label racket))

@title{Literals}
 values with a direct syntactic representation@section{related}
  Classes/Number, Classes/Integer, Classes/Float, Classes/SimpleNumber, Classes/String, Classes/Array, Classes/Symbol
@section{categories}
  Language

Literals are values which have a direct syntactic representation.
The following sections describe the types of literals that can be represented.

@section{section}
 Numbers

@section{subsection}
 Integers
An integer is any series of digits optionally preceded by a minus sign:

@racketblock[
-13
666
2112
96
::

They can also also be expressed in hexidecimal with the prefix ]

@racketblock[0x:: and
either uppercase or lowercase letters:
]

@racketblock[
0xa    // 10
-0xd   // -13
0x29A  // 666
0x840  // 2112
0x60   // 96
::

]
@section{subsection}
 Floats
A float, or floating-point number, is one or more decimal digits, followed by a
decimal point, followed by one or more decimal digits.
You must have digits on both sides of the decimal point.
In SuperCollider, floating-point numbers are always 64-bit, except within a
link::Classes/FloatArray::.

Examples of floats:

@racketblock[
0.39
98.6
1.0
-0.5
::

Exponential notation is also supported:
]

@racketblock[
1.2e4
1E-4
::

The keyword ]

@racketblock[pi:: can also be used by itself, or appended to a float or
integer to create a floating point constant:
]

@racketblock[
pi
2pi
0.5pi
-0.25pi
::

The keyword ]

@racketblock[inf:: represents infinity, and is also treated as an instance
of Float.
]

@racketblock[
inf   // and beyond!
-inf
::

]
@section{subsection}
 Radix
Numbers can also be written in radices other than base 10 up to base 36.
The radix is specified in base 10, followed by the letter 'r', followed by the
value written in that radix using characters 0-9 and A-Z (or a-z) for digit
values from 0 to 35. For example, you can write hexadecimal numbers as follows:

@racketblock[
16rF // 15
16ra9 // 169
36rZIGZAG // 2147341480
::

Binary numbers can be written as follows:
]

@racketblock[
2r01101011
::

Floating point values may also be specified in any base. However, only
uppercase letters may be used past the decimal point for bases greater than 10.
This eliminates ambiguity: if lowercase letters were allowed,
]

@racketblock[36rA.bitNot:: might be a function call, or it might be
]

@racketblock[36rA.BITNOT == 10.320080118934::.
]

@racketblock[
12r4a.abc // wrong
12r4a.ABC // works
12r4A.ABC // better
::

Hexidecimal numbers notated with ]

@racketblock[0x:: may only be expressed as integers.

]
@section{subsection}
 Scale Degrees
Integer numbers as scale degrees supports accidentals notation by adding the suffixes strong::s:: for sharp and strong::b:: for flat. Accidentals are represented as floating point values.


@racketblock[
2s == 2.1  // scale degree two, sharp
2b == 1.9  // scale degree two, flat
2ss == 2.2 // scale degree two, double sharp
2bb == 1.8 // scale degree two, double flat
::

Up to four:

]

@racketblock[
2ssss == 2.4
2bbbb == 1.6
::

With negative scale degrees it reverses:

]

@racketblock[
-2s == -1.9
-2b == -2.1
-2ss == -1.8
-2bb == -2.2
::

Accidentals can also specify cents deviation up to 499 cents:

]

@racketblock[
2b50 == 1.95 // scale degree two, fifty cents flat
2s204 == 2.204 // scale degree two, 204 cents sharp
::

]
@section{section}
 Characters

Characters are preceded by a dollar sign:

@racketblock[
$A
$B
$C
$.
$$
::

As in C and Java, backslash ('\') is the emphasis::escape character::.
Escaping has two main purposes. First, to insert non-printing characters into a
String. Secondly, to allow a String or Symbol delimiter to be included in the
contents. For String, double-quote marks indicate the beginning and ending of
the String literal. To put a double-quote in the middle of the string, the
normal meaning of double-quote must be suspended ("escaped"), as in
]

@racketblock["He repeated, \"Madam, I'm Adam,\" only this time he had said it backward."::

In all cases, the \ as an escape character does not appear in the String or
Symbol. This is a frequent source of confusion for Windows file paths:
e.g., ]

@racketblock["C:\Users\Somebody\SuperCollider":: translates into
teletype::C:UsersSomebodySuperCollider::. The way to notate a literal backslash
inside a String or Symbol is with a double-backslash:
]

@racketblock["C:\\Users\\Somebody\\SuperCollider"::. (Note, however, that it is
preferable to write file paths using forward slashes, regardless of platform:
]

@racketblock["C:/Users/Somebody/SuperCollider"::.

The following are the recognized escape characters in SuperCollider. A
backslash before any other character will simply produce that character.
]

@racketblock[
$\t  // tab (horizontal tab)
$\f  // form feed
$\v  // vertical tab
$\n  // newline (linefeed)
$\r  // return
$\\  // backslash`
::

]
@section{section}
 Symbols and Strings

@section{subsection}
 Symbols
A symbol can be written in two ways. One method is to enclose the contents in
single quotes. Any printing character may be used within a symbol except for
non-space whitespace characters (
@racketblock[\f, \n, \r, \t, \v::). Any single quotes
within the symbol must be escaped (]

@racketblock[\'::).
]

@racketblock[
'x'
'aiff'
'BigSwiftyAndAssoc'
'nowhere here'
'somewhere there'
'.+o*o+.'
'\'symbol_within_a_symbol\''
::

A second way of notating symbols is by prefixing the word with a backslash.
This is only legal if the symbol consists of a single word (a sequence
of alphanumeric and/or underscore characters).
]

@racketblock[
\x
\aiff
\Big_Swifty_And_Assoc
\not really a symbol  // illegal
::

]
@section{subsection}
 Strings

Strings are written in double quotes:

@racketblock[
"This is a string."
::

If two or more strings are lexically adjacent, then they combine into a larger string:
]

@racketblock[
a = "st" "ri" "ng"
a.size // 6
::

Strings may span more than one line. The newline characters become part of the string:
]

@racketblock[
a = "st
ri
ng
"
a.size // 9
::

The SuperCollider IDE uses UTF-8 to decode and display strings.
See link::Classes/String#Character encodings##String:: for more information.

]
@section{section}
 Identifiers

Names of methods and variables begin with a lower case alphabetic character,
followed by zero or more alphanumeric or underscore characters.

@racketblock[
var abc, z123, trigger_func;
var 1var; // not legal
var _hmm; // not legal
var hmm_; // Although this is legal, avoid it, since setter functions in
          // SuperCollider typically end with underscores.
::

]
@section{section}
 Class Names

Class names always begin with a capital letter followed by zero or more
alphanumeric or underscore characters.

@racketblock[
Object
Point
Synth
SinOsc
Pan2
MyClass_LikesUnderscores // legal
MyClass_ // legal, but avoid
::

]
@section{section}
 Special Values

The singular instances of the classes True, False and Nil can
be expressed with keywords 
@racketblock[true::, ]

@racketblock[false::, and ]

@racketblock[nil::.
]

@racketblock[
x = true;
y = false;
z = nil;
::

]
@section{section}
 Arrays

link::Classes/Array::s of literals are created at compile time and are written with a # preceding the array as follows:

@racketblock[
#[1, 2, 'abc', "def", 4]
::
Literal Arrays must be used as is and may not be altered at run time.

In literal Arrays names are interpreted as symbols. This is not the case in regular Arrays, where they are interpreted as variable names:
]

@racketblock[
#[foo, bar]     // this is legal; an Array of Symbols
[foo, bar]      // this is only legal if foo and bar have been declared as variables
::
Arrays and other collections may also be created dynamically which is explained in link::Classes/Collection::.
Using a literal Array is faster than building an array dynamically every time you need it.

When nesting literal arrays, only the outermost literal array needs the '#' character.
]

@racketblock[
#[[1, 2, 3], [4, 5, 6]]
::

Literal Arrays can be useful for things such as tables of constants, for example note names:
]

@racketblock[
(
// build a table of note names
var table = ();
value {
    var semitones = [0, 2, 4, 5, 7, 9, 11];
    var naturalNoteNames = ["c", "d", "e", "f", "g", "a", "b"];

    (0..9).do {|o|
        naturalNoteNames.do {|c, i|
            var n = (o + 1) * 12 + semitones[i];
            table[(c ++ o).asSymbol] = n;
            table[(c ++ "s"  ++ o).asSymbol] = n + 1;
            table[(c ++ "ss" ++ o).asSymbol] = n + 2;
            table[(c ++ "b"  ++ o).asSymbol] = n - 1;
            table[(c ++ "bb" ++ o).asSymbol] = n - 2;
        };
    };
};

// translate note names to midi keys
table.atAll(#[c4, e4, gs4, c5, e5, gs5, c6])
)
::

]
@section{section}
  Compiler limits

There is no theoretical limit on the number of literals in a single function, if those literals are used as freestanding objects. (Of course, there remains the practical limits of system memory and the processor time required to keep track of all the objects.)

The following are a special category of literal, called emphasis::selectors::.

@section{list}
 
## Class names
## Method names
## Function definitions (enclosed in curly braces 
@racketblock[{  }::)
::

Here, there are four selectors: ]

@racketblock[SinOsc::, ]

@racketblock[ar::, ]

@racketblock[play:: and the entire function containing SinOsc.

]

@racketblock[{ SinOsc.ar(440, 0, 0.1) }.play;::

A single function may contain no more than 256 selectors. If this limit is exceeded, a compiler error is printed:

teletype::ERROR: Selector table too big: too many classes, method selectors or function definitions in this function. Simplify the function.::

]
@section{note}
 
Code submitted for interactive execution is first compiled into a function. A very large block of code, spanning several thousand lines, may run into this limitation if it doesn't break down the tasks into smaller functions. In general, code is easier to maintain if it is reasonably emphasis::modular::, with small functions handling clearly-defined parts of the problem. This error message is a signal that the code has become too complex for a loose or "flat" code structure.
::

@section{subsection}
  What counts as "inside the function"?

Selectors are counted only toward the function definition currently being compiled.


@racketblock[
{ x.foo };

{ x.bar };
::

Both functions contain exactly one selector. They are separate functions. The use of "foo" in one function doesn't affect the number of selectors in another function.

]

@racketblock[
{
	var f = { |n|
		if(n > 1) { n * f.value(n-1) } { 1 }
	};

	f.value(10);
}.value;
::

The outer function includes only the selector ]

@racketblock[value::. The other selectors -- ]

@racketblock[>::, ]

@racketblock[*::, ]

@racketblock[-:: -- belong to the inner function definition and don't affect the outer function's number of selectors.

So, one possible easy way to work around the limitation is to break up a large block of code into several functions that are value'd successively:

]

@racketblock[
{
	... a bunch of code ...
}.value;

{
	... a bunch of code ...
}.value;

{
	... a bunch of code ...
}.value;
::
]


