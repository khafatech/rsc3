#lang scribble/manual
@(require (for-label racket))

@title{Speech}
 [DEPRECATED] lets you use the Apple speech synthesizer@section{categories}
  Platform>macOS (OS X)

@section{description}


@section{warning}
 
Speech has been deprecated for a number of reasons:

@section{list}
 
## The audio output is independent of the server, so it has major limitations when attempting to use it in SC compositions.
## The core library should be small, and speech synthesis is too niche to merit inclusion.
## It is macOS only, and implementing cross-platform compatibility is nontrivial.
::

Generally, the replacement is to use unixCmd to invoke a speech synthesizer yourself. There is also a third-party quark that simplifies this for you: https://github.com/adcxyz/say.
::


@racketblock[
"hi i'm talking with the default voice now, i guess".speak;
::

Speech consists of an link::Classes/Array:: of SpeechChannels. By default Speech is initialized with only one channel, but can be set up to use up to 16 by providing an argument to init. Channels may be used through a SpeechChannel object or by setting the channel in Speech's methods (see examples below).

Speech is a function of the operating system and not the server. By consequence, strong::it is not possible to use UGens to filter or record the output directly::. You may be able to patch system output to system input (either by hardware of by software) to rout it to the server.

]
@section{note}
 
Currently only supported on macOS. In SwingOSC there is the equivalent JSpeech.
::

@section{ClassMethods}
 

@section{private}
 prInitSpeech

@section{Examples}
 


@racketblock[
Speech.init(2);
Speech.channels[0].speak("hallo");
Speech.channels[0].isActive;
Speech.channels[0].voice_(14);
Speech.channels[0].speak("hallo");
Speech.channels[0].pitch_(60);
Speech.channels[0].speak("hallo");
Speech.channels[0].volume_(-20.dbamp);
Speech.channels[0].pitchMod_(50);
Speech.channels[0].speak("hallo");
Speech.channels[0].stop(\immediate);
Speech.channels[0].stop(\endOfWord);
Speech.channels[0].stop(\endOfSentence);
::

Force the voice to speaking something different by setting the second argument of speak to true.
]

@racketblock[
Speech.channels[0].speak("Force the voice to speaking something different.");
Speech.channels[0].speak("Force the voice to speaking something different.".reverse, true);
::

First argument is always the voice channel number, second the value.
]

@racketblock[
Speech.setSpeechVoice(0,14);
Speech.setSpeechPitch(0, 40); //pitch in MIDI Num
Speech.setSpeechRate(0, 10);
Speech.setSpeechVolume(0,0.1);
Speech.setSpeechPitchMod(0, 200);
Speech.stop(0, 1);
::

Two actions can be applied:
]

@racketblock[
Speech.wordAction = {arg voiceNum;
	//i.postln;
	// the currently speaking text may not be changed
	//Speech.setSpeechPitch(voiceNum,[41,60].choose);
	//Speech.setSpeechRate(voiceNum,[60,80, 10].choose);
};
Speech.doneAction_({arg voiceNum;
	Speech.setSpeechPitch(voiceNum,[41,48,40,43,30,60].choose);
});
::

Pause the speech while speaking: 1=pause, 0= start
]

@racketblock[
Speech.pause(0,1);
::

Initialization happens automatically, by default with one voice channel.
You may explicitly initialize with more channels, up to 16:
]

@racketblock[
(
Speech.init(16);

Task({
	16.do ({arg i;
		[0.1, 0.18, 0.2].choose.wait;
		Speech.setSpeechRate(i,[90, 30, 60].choose);
		Speech.setSpeechVolume(i,0.07);
		"no this is private. float . boolean me. char[8] ".speak(i);
	});
}).play;
)

//jan@sampleAndHold.org 04/2003
//update 10/2007
::
]


