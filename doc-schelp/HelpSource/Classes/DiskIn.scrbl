#lang scribble/manual
@(require (for-label racket))

@title{DiskIn}
 Stream in audio from a file.@section{related}
  Classes/PlayBuf, Classes/DiskOut
@section{categories}
   UGens>InOut, UGens>Buffer

@section{description}


Continuously play a longer soundfile from disk. This requires a buffer to
be preloaded with one buffer size of sound.


@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar

@section{argument}
 numChannels
Number of channels. This must match the number of channels in the buffer.

@section{argument}
 bufnum
Buffer number
@section{Note}
  The Buffer's numFrames must be a power of two and is recommended to be at least 65536 -- preferably 131072 or 262144. Smaller buffer sizes mean more frequent disk access, which can cause glitches. ::

@section{argument}
  loop
If set to 1, the soundfile will loop.

@section{Note}
  If the buffer has a larger number of frames than the sound file there will be a noticeable gap between the first and the following loop iterations. In that case chose a smaller buffer size or use link::Classes/PlayBuf##PlayBuf:: instead::

@section{discussion}
 
This UGen will set the link::Classes/Done##'done' flag:: when finished playing.





@section{instancemethods}
 
@section{private}
  init

@section{Examples}
 


@racketblock[
s.boot; // start the server

// examples below will use this synthdef
(
SynthDef("help-Diskin", { arg bufnum = 0;
	Out.ar(0, DiskIn.ar(1, bufnum));
}).add
)
::

]
@section{subsection}
  Normal usage (with Buffer; "Object Style")

@racketblock[
b = Buffer.cueSoundFile(s, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 1);

x = { DiskIn.ar(1, b.bufnum) }.play;

b.close;

// again
// note the like named instance method, but different arguments
b.cueSoundFile(Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0);

x.free; b.close; b.free;



// loop it (for better looping use PlayBuf!)
(
p = Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff";
a = SoundFile.new;
a.openRead(p);
d = a.numFrames/s.sampleRate; // get the duration
a.close; // don't forget
b = Buffer.cueSoundFile(s, p, 0, 1);
f = { DiskIn.ar(1, b.bufnum) };
x = f.play;
r = Routine({
	loop({ d.wait; x.free; x = f.play; b.close( b.cueSoundFileMsg(p, 0)) });
}).play; )
r.stop; x.free; b.close; b.free; // you need to do all these to properly cleanup



// cue and play right away
(
SynthDef("help-Diskin", { arg bufnum = 0;
	Out.ar(0, DiskIn.ar(1, bufnum));
}).add;
)
(
x = Synth.basicNew("help-Diskin");
m = { arg buf; x.addToHeadMsg(nil, [\bufnum,buf.bufnum])};

b = Buffer.cueSoundFile(s,Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff",0,1, completionMessage: m);

)

::

]
@section{subsection}
  OSC Messaging Style

@racketblock[
// allocate a disk i/o buffer
s.sendMsg("/b_alloc", 0, 65536, 1);

// open an input file for this buffer, leave it open
s.sendMsg("/b_read", 0, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 65536, 0, 1);

// create a diskin node
s.sendMsg("/s_new", "help-Diskin", x = s.nextNodeID, 1, 1);

s.sendMsg("/b_close", 0); // close the file (very important!)


// again
// don't need to reallocate and Synth is still reading
s.sendMsg("/b_read", 0, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 0, 0, 1);

s.sendMsg("/n_free", x); // stop reading

s.sendMsg("/b_close", 0); // close the file.

s.sendMsg("/b_free", 0); // frees the buffer
::

]


