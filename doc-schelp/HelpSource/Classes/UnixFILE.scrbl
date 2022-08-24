#lang scribble/manual
@(require (for-label racket))

@title{UnixFILE}
 An abstract class@section{related}
  Classes/File, Classes/Pipe
@section{categories}
  Files

@section{InstanceMethods}
 

@section{private}
 prGetLine, addOpenFile

@section{method}
 isOpen
Returns whether the file is open. An open request can fail if a file cannot be found for example. This method lets you test that the open call succeeded.

@section{method}
 pos
Answer the current file position

@section{argument}
 offset
an offset in bytes.

@section{argument}
 origin
one of the following link::Classes/Integer::s:
@section{definitionList}
 
## 0 || seek from beginning of file.
## 1 || seek from current position in file.
## 2 || seek from end of file.
::

@section{method}
 write
Writes an item to the file.

@section{argument}
 item
one of the following:
@section{definitionList}
 
## link::Classes/Float:: ||
## link::Classes/Integer:: ||
## link::Classes/Char:: ||
## link::Classes/Color:: ||
## link::Classes/Symbol:: || writes the name of the Symbol as a C string.
## link::Classes/RawArray:: || write the bytes from any RawArray in big endian.
::

@section{method}
 getLine
reads and returns a link::Classes/String:: up to lesser of next newline or 1023 chars.

@section{method}
 getChar
read one byte and return as a link::Classes/Char::.

@section{method}
 getInt8
read one byte and return as a link::Classes/Integer::.

@section{method}
 getInt16
read two bytes and return as an link::Classes/Integer::.

@section{method}
 getInt32
read four bytes and return as an link::Classes/Integer::.

@section{method}
 getFloat
read four bytes and return as a link::Classes/Float::.

@section{method}
 getDouble
read eight bytes and return as a link::Classes/Float::.

@section{method}
 putChar
write a link::Classes/Char:: as one byte.

@section{method}
 putInt8
write an link::Classes/Integer:: as one byte. That is a signed link::Classes/Integer:: value between -128 and 127.

@section{method}
 putInt16
write an link::Classes/Integer:: as two bytes.

@section{method}
 putInt32
write an link::Classes/Integer:: as four bytes.

@section{method}
 putFloat
write a link::Classes/Float:: as four bytes.

@section{method}
 putDouble
write a link::Classes/Float:: as eight bytes.

@section{method}
 putString
write a null terminated link::Classes/String::.


