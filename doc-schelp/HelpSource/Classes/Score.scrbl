#lang scribble/manual
@(require (for-label racket))

@title{Score}
 score of timed OSC commands@section{related}
  Guides/Non-Realtime-Synthesis
@section{categories}
  Control, Server>NRT, External Control>OSC

@section{description}

Score encapsulates a list of timed OSC commands and provides some methods for using it, as well as support for the creation of binary OSC files for non-realtime synthesis. See link::Guides/Non-Realtime-Synthesis:: for more details.

The list should be in the following format, with times in ascending order. Bundles are okay.


@racketblock[
[
[beat1, [OSCcmd1]],
[beat2, [OSCcmd2], [OSCcmd3]],
...
[beat_n, [OSCcmdn]],
[beatToEndNRT, [\c_set, 0, 0]] // finish
]
::

For NRT synthesis the final event should a dummy event, after which synthesis will cease. It is thus important that this event be timed to allow previous events to complete.

Score scheduling defaults to link::Classes/TempoClock::. A setting of ]

@racketblock[TempoClock.default.tempo = 1 :: (60 beats per minute), may be used to express score events in seconds if desired.

]
@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new
returns a new Score object with the supplied list.

@section{argument}
 list
can be an link::Classes/Array::, a link::Classes/@section{List}
 , or similar object.

@section{method}
 newFromFile
as link::#*new::, but reads the list in from a text file.

@section{argument}
 path
a link::Classes/String:: indicating the path of the file. The file must contain a valid SC expression.

@section{method}
 play
as link::#*new:: but immediately plays it. (See also the instance method below.)

@section{argument}
 list
the list.

@section{argument}
 server
If no value is supplied it will play on the default link::Classes/Server::.

@section{method}
 playFromFile
as link::#*play::, but reads the list from a file.

@section{method}
 write
a convenience method to create a binary OSC file for NRT synthesis. Does not create an instance.

@section{argument}
 list
the list.

@section{argument}
 oscFilePath
a link::Classes/String:: containing the desired path of the OSC file.

@section{argument}
 clock
Use clock as a tempo base. 
@racketblock[TempoClock.default:: is used if clock is nil.

]
@section{method}
 writeFromFile
as link::#*write:: but reads the list from a file.

@section{argument}
 path
a path to a file with a list.

@section{argument}
 oscFilePath
a link::Classes/String:: containing the desired path of the OSC file.

@section{argument}
 clock
Use clock as a tempo base. 
@racketblock[TempoClock.default:: is used if clock is nil.

]
@section{method}
 recordNRT
a convenience method to synthesize strong@section{list}
  in non-realtime. This method writes an OSC file to strong::oscFilePath:: (you have to do your own cleanup if desired) and then starts a server app to synthesize it. For details on valid headerFormats and sampleFormats see link::Classes/SoundFile::. Use 
@racketblock[TempoClock.default:: as a tempo base. Does not return an instance.

]
@section{argument}
 list
the list.

@section{argument}
 oscFilePath
the path to which the binary OSC file will be written.

@section{argument}
 outputFilePath
the path of the resultant soundfile.

@section{argument}
 inputFilePath
an optional path for an input soundfile.

@section{argument}
 sampleRate
the sample rate at which synthesis will occur.

@section{argument}
 headerFormat
the header format of the output file. The default is 'AIFF'.

@section{argument}
 sampleFormat
the sample format of the output file. The default is 'int16'.

@section{argument}
 options
an instance of link::Classes/ServerOptions::. If not supplied the options of the default link::Classes/Server:: will be used.

@section{argument}
 completionString

@section{argument}
 duration

@section{argument}
 action
A function to be evaluated once the NRT server has finished rendering its score.

@section{InstanceMethods}
 

@section{method}
 play
play the list on strong::server::, use strong::clock:: as a tempo base and quantize start time to strong::quant::. If strong::server:: is nil, then on the default server. 
@racketblock[TempoClock.default:: if strong::clock:: is nil. now if strong::quant:: is 0.

]
@section{method}
 stop
stop playing.

@section{method}
 write
create a binary OSC file for NRT synthesis from the list. Use strong::clock:: as a tempo base. 
@racketblock[TempoClock.default:: if strong::clock:: is nil.

]
@section{method}
 score
get or set the list.

@section{method}
 add
adds bundle to the list.

@section{method}
 sort
sort the score time order. This is recommended to do strong::before recordNRT or write:: when you are not sure about the packet order.

@section{method}
 recordNRT
synthesize the score in non-realtime. For details of the arguments see link::#*recordNRT:: above.

@section{method}
 saveToFile
save the score list as a text file to strong::path::.

@section{Examples}
 

@section{subsection}
 NRT Examples


@racketblock[
// A sample synthDef
(
SynthDef("helpscore",{ arg freq = 440;
	Out.ar(0,
		SinOsc.ar(freq, 0, 0.2) * Line.kr(1, 0, 0.5, doneAction: Done.freeSelf)
	)
}).add;
)

// write a sample file for testing
(
var f, g;
TempoClock.default.tempo = 1;
g = [
	[0.1, [\s_new, \helpscore, 1000, 0, 0, \freq, 440]],
	[0.2, [\s_new, \helpscore, 1001, 0, 0, \freq, 660]],
	[0.3, [\s_new, \helpscore, 1002, 0, 0, \freq, 220]],
	[1, [\c_set, 0, 0]] // finish
	];
f = File("score-test","w");
f.write(g.asCompileString);
f.close;
)

//convert it to a binary OSC file for use with NRT
Score.writeFromFile("score-test", "test.osc");
::

From the command line, the file can then be rendered from within the build directory:

]

@racketblock[
./scsynth -N test.osc _ test.aif 44100 AIFF int16 -o 1
::

Score also provides methods to do all this more directly:

]

@racketblock[
(
var f, o;
g = [
	[0.1, [\s_new, \helpscore, 1000, 0, 0, \freq, 440]],
	[0.2, [\s_new, \helpscore, 1001, 0, 0, \freq, 660],
		[\s_new, \helpscore, 1002, 0, 0, \freq, 880]],
	[0.3, [\s_new, \helpscore, 1003, 0, 0, \freq, 220]],
	[1, [\c_set, 0, 0]] // finish
	];
o = ServerOptions.new.numOutputBusChannels = 1; // mono output
Score.recordNRT(g, "help-oscFile", "helpNRT.aiff", options: o); // synthesize
)
::

]
@section{subsection}
 Real-time Examples


@racketblock[
s.boot; // boot the default server

// A sample synthDef
(
SynthDef("helpscore",{ arg freq = 440;
	Out.ar(0,
		SinOsc.ar(freq, 0, 0.2) * Line.kr(1, 0, 0.5, doneAction: Done.freeSelf)
	)
}).add;
)

// write a sample file for testing
(
var f, g;
TempoClock.default.tempo = 1;
g = [
	[0.1, [\s_new, \helpscore, 1000, 0, 0, \freq, 440]],
	[0.2, [\s_new, \helpscore, 1001, 0, 0, \freq, 660],
		[\s_new, \helpscore, 1002, 0, 0, \freq, 880]],
	[0.3, [\s_new, \helpscore, 1003, 0, 0, \freq, 220]],
	[1, [\c_set, 0, 0]] // finish
	];
f = File("score-test","w");
f.write(g.asCompileString);
f.close;
)

z = Score.newFromFile("score-test");

// play it on the default server
z.play;

// change the list
(
x = [
[0.0, [ \s_new, \helpscore, 1000, 0, 0, \freq, 1413 ]],
[0.1, [ \s_new, \helpscore, 1001, 0, 0, \freq, 712 ]],
[0.2, [ \s_new, \helpscore, 1002, 0, 0, \freq, 417 ]],
[0.3, [ \s_new, \helpscore, 1003, 0, 0, \freq, 1238 ]],
[0.4, [ \s_new, \helpscore, 1004, 0, 0, \freq, 996 ]],
[0.5, [ \s_new, \helpscore, 1005, 0, 0, \freq, 1320 ]],
[0.6, [ \s_new, \helpscore, 1006, 0, 0, \freq, 864 ]],
[0.7, [ \s_new, \helpscore, 1007, 0, 0, \freq, 1033 ]],
[0.8, [ \s_new, \helpscore, 1008, 0, 0, \freq, 1693 ]],
[0.9, [ \s_new, \helpscore, 1009, 0, 0, \freq, 410 ]],
[1.0, [ \s_new, \helpscore, 1010, 0, 0, \freq, 1349 ]],
[1.1, [ \s_new, \helpscore, 1011, 0, 0, \freq, 1449 ]],
[1.2, [ \s_new, \helpscore, 1012, 0, 0, \freq, 1603 ]],
[1.3, [ \s_new, \helpscore, 1013, 0, 0, \freq, 333 ]],
[1.4, [ \s_new, \helpscore, 1014, 0, 0, \freq, 678 ]],
[1.5, [ \s_new, \helpscore, 1015, 0, 0, \freq, 503 ]],
[1.6, [ \s_new, \helpscore, 1016, 0, 0, \freq, 820 ]],
[1.7, [ \s_new, \helpscore, 1017, 0, 0, \freq, 1599 ]],
[1.8, [ \s_new, \helpscore, 1018, 0, 0, \freq, 968 ]],
[1.9, [ \s_new, \helpscore, 1019, 0, 0, \freq, 1347 ]],
[2.0, [\c_set, 0, 0]] // finish
];

z.score_(x);
)

// play it
z.play;

// play and stop after one second
(
z.play;
SystemClock.sched(1.0, {z.stop;});
)
::

]
@section{subsection}
 creating Score from a pattern


@racketblock[
SynthDescLib.read;

// new pattern
(
p = Pbind(
	\dur, Prand([0.3, 0.5], inf),
	\freq, Prand([200, 300, 500],inf)
);
)

// make a score from the pattern, 4 beats long
z = p.asScore(4.0);

z.score.postcs;
z.play;

// rendering a pattern to sound file directly:

// render the pattern to aiff (4 beats)

p.render("asScore-Help.aif", 4.0);
::
]


