#lang scribble/manual
@(require (for-label racket))

@title{04}
 Mark Polishook tutorial (JP)@section{categories}
  Tutorials>Mark_Polishook_tutorial>Japanese_version
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 ユニット・ジェネレータのコントロール のレート

いくつかのSuperColliderのugenは、後に.arメッセージが続けられているということに気づいているでしょう。例えば、次の様に


@racketblock[
SinOsc.kr
::

というものもあれば、他のugenは、次の様に.arメッセージを使うということもあります。

]

@racketblock[
SinOsc.ar
::


]
@section{section}
 Audio rate

.arメッセージを持つugenはオーディオ・レートで動作します。デフォルトでは、一秒間あたり44,100サンプルです。


@racketblock[
SinOsc.ar(440, 0, 1);
::

耳に聞こえるオーディオ・チェーンの一部分である場合にはいつでも、.arメッセージをユニット・ジェネレータに送ります。

]
@section{section}
 Control rate

コントロール・レート

.krメッセージが後に続くugenはコントロール・レートで動作します。デフォルトでは、コントロール・レートのugenは、オーディオ・レートのugenが生成する64サンプルごとに1回だけ生成します。


@racketblock[
SinOsc.kr(440, 0, 1);
::

.krメッセージをユニット・ジェネレータに送るのは、それら（ユニット・ジェネレータ）をモジュレータ、すなわち、オーディオ信号を形作るまたはコントロールするもの、として使いたいときだけです。

コントロール・レート・シグナルは、一方のオーディオ・レートと比較して計算負荷が低くなります。オーディオ・レートのugenのパラメータを変調するために使う場合には、変化のレートが異なるにも関わらず、ほとんどスムーズに聞こえます。

////////////////////////////////////////////////////////////////////////////////////////////////////

これは1つのUGenがオーディオ・レートで動作し、他のUGenがコントロール・レートで動作するという一例です。

]

@racketblock[
(
SynthDef("anExample", {
	Out.ar(
		0,
		SinOsc.ar(
			[440, 443] + SinOsc.kr([7, 8], 0, 7), // コントロール・レートはCPUサイクルをセーブする
			0,
			0.2
		)
	)
}).add;
)

Synth("anExample")
::

コントロール・レートで動作するこのSinOscーSinOsc.kr([7, 8], 0, 7)ーのインスタンスは、オーディオ・レートで聞くSinOscの周波数を変調します。

////////////////////////////////////////////////////////////////////////////////////////////////////

将来的には、増加し続けるコンピュータの速度によって（完全に廃止されてしまわない限りは）オーディオ・レートとコントロール・レートを使い分ける必要性は減るでしょう。コントロール・レートでugenを使うことにより、オーディオ・レートでデータを生成する様な厳密さが要求されない場合には、処理にかかる負荷を抑えることができます。

go to link::Tutorials/Mark_Polishook_tutorial/Japanese_version/05::
]


