#lang scribble/manual
@(require (for-label racket))

@title{VDiskIn}
 Stream in audio from a file, with variable rate@section{categories}
  UGens>InOut, UGens>Buffer
@section{related}
  Classes/PlayBuf, Classes/BufRd, Classes/DiskIn

@section{description}

Continuously play a longer soundfile from disk. This requires a buffer to be preloaded with one buffer size of sound.

@section{classmethods}
 
@section{method}
  ar

@section{argument}
  numChannels
number of channels

@section{argument}
  bufnum
buffer number
@section{Note}
  The Buffer's numFrames must be a power of two and is recommended to be at least 65536 -- preferably 131072 or 262144. Smaller buffer sizes mean more frequent disk access, which can cause glitches. ::

@section{argument}
  rate
controls the rate of playback. Values below 4 are probably fine, but the higher the value, the more disk activity there is, and the more likelihood there will be a problem.

@section{warning}
 
the rate does have a practical limit. The following must be true: rate < Buffer's size / ( 2 * s.options.blockSize) e.g with typical default values, this will be 32768 / (2 * 64) = 256.
::

If the rate is too high, the UGen will not execute, posting a warning.

@section{argument}
  loop
If loop is set to 1, the soundfile will loop.

@section{argument}
  sendID
If a sendID is given, the UGen sends an OSC message with this ID and the frame index emphasis::relative to the onset of the Synth:: each time it reloads the buffer: 
@racketblock[['/diskin', nodeID, sendID, frame]::.
The true frame index into the file is determined by the ]

@racketblock[startFrame:: given when reading the initial buffer's worth of data, using link::Classes/Buffer#-cueSoundFile:: or link::Classes/Buffer#-read::.
VDiskIn does not have access to the frame index used to cue the buffer. So, the frame index sent by OSC is always 0 at the start of the synth node.
The user is responsible for adding the cue point: ]

@racketblock[cueStartFrame + msg[3]::.

]
@section{discussion}
 
This UGen will set the link::Classes/Done##'done' flag:: when finished playing.

@section{examples}
 

@racketblock[
b = Buffer.cueSoundFile(s, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 1);

x = { VDiskIn.ar(1, b, LFNoise2.kr(0.2).range(0.5, 2), 1, loop:1) }.play;

b.close;

// again
// note the like named instance method, but different arguments
b.cueSoundFile(Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0);

x.free; b.close; b.free;


// cue and play right away
(
SynthDef("help-VDiskin", { arg bufnum = 0;
	Out.ar(0, VDiskIn.ar(1, bufnum, MouseX.kr(0.5, 2.0)));
}).add;
)
(
x = Synth.basicNew("help-VDiskin");
m = { arg buf; x.addToHeadMsg(nil, [\bufnum, buf])};

b = Buffer.cueSoundFile(s,Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff",0,1, completionMessage: m);
)

x.free; b.close; b.free;	// clean up



// sending back the file position.
// note:
// the ugen knows nothing of the loop (apply a modulo).
// if you load another file, you need to free the buffer and re-allocate it (see below)

b = Buffer.cueSoundFile(s, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 1, bufferSize: 4096);
c = SoundFile(Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff").info;
x = { VDiskIn.ar(1, b, LFNoise2.kr(0.2).range(0.2, 0.9), 1, sendID: 14) }.play;

// register to receive this message

(
o = OSCFunc({ arg msg;
	var sendID = msg[2];
	var index = msg[3];
	msg.postln;
	"id: % pos: % frames (% sec)\n"
		.postf(sendID, index % c.numFrames, (index % c.numFrames / c.sampleRate));
},'/diskin', s.addr)
);

b.close; b.free;

b.alloc; b.cueSoundFile(Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0); c = SoundFile(Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff").info;

x.free; b.close; b.free; o.free; // clean up eventually
::

The same example in OSC Messaging style, see link::Guides/NodeMessaging::
]

@racketblock[
// allocate a disk i/o buffer
s.sendMsg("/b_alloc", 0, 65536, 1);

// open an input file for this buffer, leave it open
s.sendMsg("/b_read", 0, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 65536, 0, 1);

// create a diskin node
s.sendMsg("/s_new", "help-VDiskin", x = s.nextNodeID, 1, 1);

s.sendMsg("/b_close", 0); // close the file (very important!)


// again
// don't need to reallocate and Synth is still reading
s.sendMsg("/b_read", 0, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff", 0, 0, 0, 1);

s.sendMsg("/n_free", x); // stop reading

s.sendMsg("/b_close", 0); // close the file.

s.sendMsg("/b_free", 0); // frees the buffer
::
]


