#lang scribble/manual
@(require (for-label racket))

@title{Assignment Statements}
@section{categories}
  Language
 assigning values to variables
@section{section}
  Single Assignment

A single assignment assigns the value of an expression on the right hand side to a variable on the left hand side. A single assignment is in the form:

@racketblock[
<variable> = <an expression>
::
examples:
]

@racketblock[
x = [1, 2, 3, 4].rotate(1);
c = a + b;
::

]
@section{section}
  Multiple Assignment

A multiple assignment statement assigns the elements of a link::Classes/Collection:: which is the result of an expression on the right hand side, to a list of variables on the left hand side.
A multiple assignment statement is preceded by the symbol 
@racketblock[#::. If the last variable on the left is preceded by three dots, then the entire remainder of the collection is assigned to that variable. There must be at least one variable name before the ellipsis.

The form of a multiple assignment is:
]

@racketblock[
# <list of variables> = <expression>
::
-- or --
]

@racketblock[
# <list of variables> ... <variable> = <expression>
::

examples:
]

@racketblock[
# a, b, c = [1, 2, 3, 4, 5, 6]; // afterwards a=1, b=2, c=3

# a, b ... c = [1, 2, 3, 4, 5, 6]; // afterwards a=1, b=2, c = [3, 4, 5, 6]

# ... a = [1, 2, 3, 4, 5, 6]; // ILLEGAL, just use:    a = [1, 2, 3, 4, 5, 6];
::

Multiple assignment is implemented using the 'at' method and the 'copyToEnd' method.
Your right hand side expression can return any object that responds to these messages.

]
@section{section}
  Instance Variable Assignment

The basic syntax for setting the value of an instance variable is to use the variable's setter method which is the name of the variable with an underscore appended.

@racketblock[
point.x_(5); // set point's x coordinate to 5
::
An alternative syntax is to use instance variable assignment.
]

@racketblock[
point.x = 5;
::
This type of assignment is translated to the first form by the compiler. The two syntaxes are equivalent.

]
@section{section}
  Series Assignment to an ArrayedCollection or List

There is a special syntax for doing assignments to a range of values in an link::Classes/ArrayedCollection:: or link::Classes/@section{List}
 .

@racketblock[
a = (0,10..200);
a[5..10] = 1;  // series stepping by 1

a = (0,10..200);
a[7,9..13] = 1; // a series by any step size

a = (0,10..200);
a[..5] = 1;  // from zero to n

a = (0,10..200);
a[12..] = 1;  // from n to the end of the array

a = (0,10..200);
a[1,3..] = 1;  // a series to the end of the array
::

]


