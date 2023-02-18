#lang scribble/manual
@(require (for-label racket))

@title{Post}
posts text to the post window@section{categories}
  Files

@section{description}

The class Post is a stream destination. Its main use is that it can sometimes make code more readable and execution slightly more efficient.

@racketblock[
Post <<< a << " " <<< b << " " <<< c << " " <<< d << Char.nl;
::
vs
]

@racketblock[
(a.asCompileString + b.asCompileString + c.asCompileString + d.asCompileString).postln;
::

]
@section{warning}
 
<< also means object left shift.
::

@section{CLASSMETHODS}
 

@section{method}
 <<
Post as string

@racketblock[
Post << "string";
::

]
@section{method}
 <<<
Post as compile string

@racketblock[
Post <<< "string";
::

]
@section{method}
 comma
Prints a comma

@racketblock[
Post.comma;
::

]
@section{method}
 space
Prints a space

@racketblock[
Post.space;
::

]
@section{method}
 nl
Prints a newline

@racketblock[
Post.nl;
::

]
@section{method}
 ff
Prints the char $\f

@racketblock[
Post.ff;
::

]
@section{method}
 tab
Prints a tab

@racketblock[
Post.tab;
::

]
@section{EXAMPLES}
 


@racketblock[
a = "a string";
b = 'a symbol';
c = 4;
d = [1,2,3,4,a,b];

// post as string
Post << a << Char.nl;
// post as compile string
Post <<< a << Char.nl;

// post as string
Post << d << Char.nl;
// post as compile string
Post <<< d << Char.nl;

//This is the equivalent of :
d.postln;
//or
d.asCompileString.postln;
::
]


