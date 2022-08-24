#lang scribble/manual
@(require (for-label racket))

@title{Recorder}
 Write Audio to Harddisk@section{categories}
  Server>Abstractions
@section{related}
  Classes/Server, Classes/DiskOut, Guides/Non-Realtime-Synthesis

@section{description}
 A Recorder allows you to write audio to harddisk, reading from a given bus and a certain number of channels, relative to a given node. A link::Classes/Server:: has one instance, which is accessible also through the link::Classes/ScIDE::. You can use the server directly to record its output



@racketblock[
(
{ SinOsc.ar(
	SinOsc.ar(
		XLine.kr(1, 100, 5)).exprange(*XLine.kr([20, 800], [7000, 200], 10)
    )
   ) * 0.1

}.play;
s.record(duration: 10);
)
::


This functionality is also available through the recording button on the server windows.
Pressing it once calls record, and pressing it again calls stopRecording (see below). When doing so the file created will be in your recordings folder and be named for the current date and time.
The default location of the recordings folder varies from platform to platform. Setting this variable allows you to change the default.

]

@racketblock[
// find where the recordings are written to
thisProcess.platform.recordingsDir
::

]
@section{NOTE}
 
By default, record creates the recording synth after the Server's default group and uses In.ar. Thus if you add nodes after the recording synth their output will not be captured.
To avoid this, either use Node objects (which use the default node as their target) or (when using messaging style) use a target nodeID of 1.

@racketblock[
s.sendMsg("/s_new", "default", s.nextNodeID, 1, 1);
::
::

For more detail on this subject see link::Guides/Order-of-execution::, link::Reference/default_group::, and link::Guides/NodeMessaging::.

See link::Classes/SoundFile:: for information on the various sample and header formats.
Not all sample and header formats are compatible. Note that the sampling rate of the output file will be the same as that of the server app. This can be set using the Server's link::Classes/ServerOptions::.



]
@section{ClassMethods}
 

@section{method}
 new
Create a new instance for a given server.

@section{argument}
 server


@section{InstanceMethods}
 

@section{method}
  prepareForRecord
Allocates the necessary buffer, etc. for recording the server's output. (See 
@racketblock[record:: below.)

]
@section{argument}
  path
a link::Classes/String:: representing the path and name of the output file.

@section{argument}
 numChannels
The number of output channels to record.

@section{discussion}
 
If you do not specify a path than a file will be created in your recordings folder (see the note above on this) called 
@racketblock[SC_thisDateAndTime::. Changes to the header or sample format, or to the number of channels must be made strong::before:: calling this.



]
@section{method}
  record
Starts or resumes recording the output.
@section{argument}
  path
this is optional, and is passed to 
@racketblock[prepareForRecord:: (above).

]
@section{argument}
  bus
The bus (link::Classes/Bus:: object or integer bus index), the offset at which to start to count the number of channels. You can record any adjacent number of bus channels.

@section{argument}
  numChannels
The number of output channels to record.

@section{argument}
  node
The link::Classes/Node:: to record immediately after. By default, this is the default group 1.

@section{argument}
  duration
If set, this limits recording to a given time in seconds.

@section{note}
 The recording starts when the buffer has been allocated, and after the usually very short network latency. It will last for the 
@racketblock[duration:: exactly down to one server block size (64 samples). For scheduling the starting point of a recording precisely, call link::#-prepareForRecord:: first, and then call link::#-record:: a bundle (see link::Classes/Server#-bind:: and  link::Classes/Server#-sync::).::

]
@section{discussion}
 
If you have not called prepareForRecord first (see above) then it will be invoked for you (but that adds a slight delay before recording starts for real).


@racketblock[
r = Recorder(s);
{ GVerb.ar(Dust.ar(4)) }.play; // play on bus 64
r.record(numChannels:2);
r.stopRecording;
::

]
@section{method}
  pauseRecording
Pauses recording. Can be resumed by executing record again, or by calling resumeRecording.

@section{method}
  resumeRecording
Start recording again.

@section{method}
  stopRecording
Stops recording, closes the file, and frees the associated resources.
@section{discussion}
 
You must call this when finished recording or the output file will be unusable. Cmd-. while recording has the same effect.

@section{method}
 filePrefix
a string used as prefix for the path when recording. This can be used to separate the outputs of several recorders. The default is 
@racketblock["SC_"::.

]
@section{method}
 numChannels
a number of sound file channels that is used always when using this recorder, unless a different one is specified in the link::#-record:: method. When not set, we use link::Classes/Server#-recChannels::.

@section{method}
  recHeaderFormat
Get/set the header format (string) of the output file. The default is "aiff". Must be called strong::before:: prepareForRecord.

@section{method}
  recSampleFormat
Get/set the sample format (string) of the output file. The default is "float". Must be called strong::before:: prepareForRecord.

@section{method}
 recBufSize
Get/set the size of the link::Classes/Buffer:: to use with the link::Classes/DiskOut:: UGen. This must be a power of two. The default is the 
@racketblock[sampleRate.nextPowerOfTwo:: or the first power of two number of samples longer than one second. Must be called strong::before:: prepareForRecord.

]
@section{method}
 isRecording
returns true if we are in the process of recording

@section{method}
 paused
returns true if recording is paused

@section{method}
 duration
returns the number of seconds we have been recording so far

@section{method}
 path
returns the path of the current recording

@section{method}
 numFrames
returns the number of frames of the recording buffer

@section{method}
 notifyServer
if set to true, it will send 
@racketblock[changed:: notifications to the server instance. This is used internally by the link::Classes/Server:: class.


]
@section{method}
 server
server to record from

@section{private}
 cmdPeriod
@section{private}
 prRecord
@section{private}
 prStopRecord
@section{private}
 makePath
@section{private}
 changedServer

@section{section}
 Examples



@racketblock[
s.boot; // start the server

// something to record
(
SynthDef("bubbles", {
	var f, zout;
	f = LFSaw.kr(0.4, 0, 24, LFSaw.kr([8,7.23], 0, 3, 80)).midicps; // glissando function
	zout = CombN.ar(SinOsc.ar(f, 0, 0.04), 0.2, 0.2, 4); // echoing sine wave
	Out.ar(0, zout);
}).add;
SynthDef("tpulse", { arg out=0,freq=700,sawFreq=440.0;
	Out.ar(out, SyncSaw.ar(freq,  sawFreq,0.1) )
}).add;

)

x = Synth.new("bubbles");

s.prepareForRecord; // if you want to start recording on a precise moment in time, you have to call this first.

s.record; // start recording. This can also be called directly, if it isn't imprtant when precisely you need to start.

s.pauseRecording; // pausable

s.record // start again

s.stopRecording; // this closes the file and deallocates the buffer recording node, etc.

x.free; // stop the synths

// look in your recordings folder and you'll find a file named for this date and time
::
]


