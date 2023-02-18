#lang scribble/manual
@(require (for-label racket))

@title{Archive}
storing objects to file@section{categories}
  Collections, Files
@section{related}
 Classes/Library, Classes/Object, Classes/LibraryBase

@section{description}

Archives can write any object to disk and read from file again. Complex structures of objects can thus be restored. Writing an object to file as a strong::compile string:: is usually more readable, but does not account for the internal structure of the object.

There is only one global instance: Archive.global, which is initialized automatically.

@section{CLASSMETHODS}
 

@section{private}
 initClass

@section{method}
 global
set or get the global archive instance

@section{method}
 archiveDir
set or get the directory that the archive is written to.
Default: link::Classes/Platform::.userAppSupportDir.

@section{method}
 write
write the global archive now. This is called automatically when SuperCollider quits.
The default filename is "/archive.sctxar"

@section{method}
 read
read the global archive now. This is called automatically when SuperCollider recompiles or starts.
The default filename is "/archive.sctxar"

@section{EXAMPLES}
 


@racketblock[
// make a storage place for various objects:
q = (); // Event

q[\a_long_array] = Array.rand(128, 0.0, 1.0);
q[\pi_squared] = pi * pi;
q[\favourite_sound] = { { SinOsc.ar([300, 330]).sum * LFPulse.kr(2 + [0, 0.01]) * 0.1 }.play };
q[\same_long_array] = q[\a_long_array]; // same objects may appear several times

Archive.global.put(\myData, q);


Archive.global.at(\myData).postcs;

// after a recompile:
s.boot;

q = Archive.global.at(\myData);
q.postcs;
q[\favourite_sound].value;
::
]


