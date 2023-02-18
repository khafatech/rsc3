#lang scribble/manual
@(require (for-label racket))

@title{Char}
ASCII character@section{related}
 Classes/String
@section{categories}
  Core

@section{description}

An ASCII character represented as a signed 8-bit integer (-128 to 127).
Valid ASCII values are in the range 0-127.
Chars may be written as literals using the $ sign. For example: $a, $b, $c.
Some special characters can be expressed as literals using escape sequences (for example, 
@racketblock[$\n:: for newline).
See link::Reference/Literals#Characters:: for more information.

Chars may be created from link::Classes/Integer##Integers:: using the methods link::Classes/Integer#-asAscii:: and link::Classes/Integer#-asDigit::.

Note that, while Char does not support encodings aside from ASCII—such as
multi-byte encodings like UTF-8 and UTF-16, or the full Latin-1 (ISO 8859-1)
character set—Chars with negative values are perfectly legal, and may be strung
together in strings that use these encodings.

The SuperCollider IDE uses UTF-8 to decode and display strings.
See link::Classes/String:: for more information.

]
@section{classmethods}
 

@section{method}
 nl
Newline 
@racketblock[($\n)::.

]
@section{method}
 ret
Carriage return 
@racketblock[($\r)::.

]
@section{method}
 tab
Horizontal tab 
@racketblock[($\t)::.

]
@section{method}
 ff
Form feed 
@racketblock[($\f)::.

]
@section{method}
 vtab
Vertical tab 
@racketblock[($\v)::.

]
@section{method}
 space
Single space 
@racketblock[($ )::.

]
@section{method}
 comma
Comma 
@racketblock[($,)::.

]
@section{method}
 bullet
Asterisk 
@racketblock[($*)::.

]
@section{instancemethods}
 
@section{private}
  archiveAsCompileString

@section{subsection}
 conversion

@section{method}
 ascii

@section{returns}
  the integer ASCII value of a Char.

@section{method}
 digit

@section{returns}
  an integer value from 0 to 9 for chars $0 to $9, and values 10 to 35 for chars $a to $z
or $A to $Z.

@section{method}
 toUpper

@section{returns}
  the upper case version of a char. Nonalphabetic chars return themselves.

@section{method}
 toLower

@section{returns}
  a lower case version of a char. Nonalphabetic chars return themselves.

@section{subsection}
  Testing

@section{method}
 isAlpha

@section{returns}
  whether the char is an alphabetic character.

@section{method}
 isAlphaNum

@section{returns}
  whether the char is an alphabetic or numeric character.

@section{method}
 isPrint

@section{returns}
  whether the char is printable.

@section{method}
 isPunct

@section{returns}
  whether the char is a punctuation character.

@section{method}
 isSpace

@section{returns}
  true if the char is white space: any of 
@racketblock[[$ , $\f, $\n, $\r, $\t, $\v]::.

]
@section{method}
 isDecDigit

@section{returns}
  true if the char is a decimal digit $0 to $9.

@section{method}
 isFileSafe

@section{returns}
  true if the char is safe for use in a filename.
Excludes the path separators / and :
@section{discussion}
 

@racketblock[
 for(0,255,{ arg i;
	var a;
	[i,a = i.asAscii,a.isAlphaNum,a.isPrint,a.isPunct,a.isControl].postln;
});
::
]


