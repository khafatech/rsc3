#lang scribble/manual
@(require (for-label racket))

@title{File}
 A class for reading and writing files@section{related}
  Classes/FileReader
@section{categories}
  Files

@section{description}

A class for reading and writing files. Not sound files.

@section{ClassMethods}
 

@section{private}
 prGetcwd, prType

@section{method}
 new
Create a File instance and open the file. If the open fails, link::Classes/UnixFILE#-isOpen#isOpen:: will return false.

@section{argument}
 pathName
a link::Classes/String:: containing the path name of the file to open.

@section{argument}
 mode
a link::Classes/String:: indicating one of the following modes:
@section{definitionList}
 
## "r" || Opens a file for reading. The file must exist.
## "w" || Creates an empty file for writing. If a file with the same name already exists its content is erased and the file is treated as a new empty file.
## "a" || Appends to a file. Writing operations append data at the end of the file. The file is created if it does not exist.
## "rb", "wb", "ab" || same as above, but data is binary
## "r+" || Opens a file for update both reading and writing. The file must exist.
## "w+" || Creates an empty file for both reading and writing. If a file with the same name already exists its content is erased and the file is treated as a new empty file.
## "a+" || Opens a file for reading and appending. All writing operations are performed at the end of the file, protecting the previous content to be overwritten. You can reposition the internal pointer using the seek method to anywhere in the file for reading, but writing operations will move it back to the end of file. The file is created if it does not exist.
## "rb+", wb+", "ab+" || same as above, but data is binary
::

@section{method}
 open
same as link::#*new::, but a more intuitive name.

@section{method}
 getcwd
POSIX standard 'get current working directory'.

@racketblock[
// example;
File.getcwd;
::

]
@section{method}
 use
open the file, evaluate the function with the file and close it.

@section{subsection}
  Filesystem utilities

@section{method}
 exists
answers if a file exists at that path.
@section{note}
 
Some filesystems, like the one used by macOS, are case insensitive.
On such systems, this method will return true for "fOo" even if the file is actually named "Foo".
For a workaround, see link::#*existsCaseSensitive:: below.
::
@section{returns}
  a link::Classes/Boolean::

@section{method}
 existsCaseSensitive
Like link::#*exists:: but ensure case sensitivity emphasis:: of the last path component :: on case insensitive filesystems. On case sensitive systems, it falls back to using 
@racketblock[exists::.

]
@section{note}
 
This is slower than the normal 
@racketblock[exists:: method, so use it only when really needed.
::

]
@section{method}
 systemIsCaseSensitive
answers if the filesystem is case sensitive or not.

@section{method}
 mkdir
create directory at path, including any missing parent directories.

@section{method}
 delete
deletes the file at that path. Use only for good, never for evil.

@section{method}
 realpath
follow symbolic links (and aliases on macOS) and any parent directory references (like "..") and return the true absolute path.
@section{returns}
  a link::Classes/String:: or 
@racketblock[nil:: if path did not exist.

]
@section{method}
 copy
copy file, symlink or directory. this method will fail if pathNameTo already exists.

symlinks are copied as symlinks (re-created).

@section{method}
 type
get file type as one of 
@racketblock[\error, \not_found, \regular, \directory, \symlink, \block, \character, \fifo, \socket, \unknown::
]
@section{returns}
  a link::Classes/Symbol::

@section{method}
 fileSize
get size of file in bytes.
@section{returns}
  an link::Classes/Integer::

@section{method}
 mtime
get last modification time in seconds since the Epoch.
@section{returns}
  an link::Classes/Integer::


@section{InstanceMethods}
 

@section{private}
 prOpen, prClose

@section{method}
 open
Open the file. Files are automatically opened upon creation, so this call is only necessary if you are closing and opening the same file object repeatedly.
@section{note}
 
it is possible when saving files with a standard file dialog to elect to "hide the extension" and save it as RTF. When opening the file you must specify the real filename: "filename.rtf", even though you can't see in file load dialogs or in the Finder.
::

@section{method}
 close
Close the file.

@section{method}
 readAllString
Reads the entire file as a link::Classes/String::.

@section{method}
 readAllStringRTF
Reads the entire file as a link::Classes/String::, stripping RTF formatting.

@section{method}
 seek
moves the read/write pointer to a given location in the file, where offset is location given in bytes, and origin is the reference of the offset:
@section{definitionList}
 
## 0 || offset is from the beginning of the file
## 1 || offset is relative to the current position in the file
## 2 || offset is from the end of the file
::

@section{method}
 pos
sets or returns the current position in the file (in bytes).
when used as a setter, this method is a shortcut for seek(0, value). so setting the pos moves the current file position to a given location from the beginning of the file. the value is clipped so that it lies between 0 inclusively and the file length exclusively.

@section{method}
 length
returns the current file size in bytes.

@section{Examples}
 


@racketblock[
// write some string to a file:
(
var f, g;
f = File("~/test.txt".standardizePath,"w");
f.write("Does this work?\n is this thing on ?\n");
f.close;
)

// read it again:
(
g = File("~/test.txt".standardizePath,"r");
g.readAllString.postln;
g.close;
)

// try the above with File.use:

File.use("~/test.txt".standardizePath, "w", { |f| f.write("Doesn't this work?\n is this thing really on ?\n"); });
File.use("~/test.txt".standardizePath, "r", { |f| f.readAllString.postln });


// more file writing/reading examples:
(
var h, k;
h = File("~/test.dat".standardizePath, "wb");
h.inspect;
h.write( FloatArray[1.1, 2.2, 3.3, pi, 3.sqrt] );
h.close;

k = File("~/test.dat".standardizePath, "rb");
(k.length div: 4).do({ k.getFloat.postln; });
k.close;
)


(
var f, g;
f = File("~/test.txt".standardizePath,"w");
100.do({ f.putChar([$a, $b, $c, $d, $e, $\n].choose); });
f.close;

g = File("~/test.txt".standardizePath,"r");
g.readAllString.postln;
g.close;

g = File("~/test.txt".standardizePath,"r");
g.getLine(1024).postln;
"*".postln;
g.getLine(1024).postln;
"**".postln;
g.getLine(1024).postln;
"***".postln;
g.getLine(1024).postln;
"****".postln;
g.close;
)

(
//var f, g;
f = File("~/test.dat".standardizePath,"wb");
f.inspect;
100.do({ f.putFloat(1.0.rand); });

f.inspect;
f.close;

g = File("~/test.dat".standardizePath,"rb");
100.do({
	g.getFloat.postln;
});
g.inspect;
g.close;
)

(
//var f, g;
f = File("~/test.dat".standardizePath,"r");
f.inspect;
f.close;
)
::
]


