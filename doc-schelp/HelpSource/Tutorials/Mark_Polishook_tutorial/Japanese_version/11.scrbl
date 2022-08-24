#lang scribble/manual
@(require (for-label racket))

@title{11}
 Mark Polishook tutorial (JP)@section{categories}
  Tutorials>Mark_Polishook_tutorial>Japanese_version
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 複数のSynthDef

ほとんど全てのシンセシス・プロセスは単独のSynthDefの中で指定することができます。しかしながら、複雑な場合には、コンポーネントの部分部分に分解することもできます。

SuperColliderでは、groupがシンセを組み合わせるメカニズムを提供します。

////////////////////////////////////////////////////////////////////////////////////////////////////

@section{section}
 Groupはリンク・リスト

groupの最も重要なアスペクトは、それが参照するノードが順序づけられるということです。順序づけるという考え方は重要です。なぜなら、SuperColliderでは、１つのシンセシスのプロセスが他よりも先に起きるということを保証するということを意味するからです。

SuperColliderは、ノードを順番に並べるために、リンク・リストというメカニズムを使います。リンク・リストはダイナミックなデータ構造で、簡単にノードを移動することができます。例えば最初のノードを最後にするという様なことができるのです。または、リストの中のある位置にあるノードをリストの中の任意の位置に置くこともできます。リンク・リストの最初のアイテムは常に".head"です。リンク・リストの最後のアイテムは常に".tail"です。

置きたいと思うところにノードを置くことができるというのは、ディレイやリバーブの様に、ソースをフィルターに送る時には重要です。こうした場合には、ソースのシンセシス・プロセスが亜エフェクトよりも先に行われるというのが重要なのです。

////////////////////////////////////////////////////////////////////////////////////////////////////

@section{section}
 ルートノード

ルートノードはいつもサーバーに存在するグループです。全てのノードは、それがシンセであってもグループであっても、木の葉や枝の様にそれに接続されます。

１つのシンセしか実行されていない場合、それは次の様にルートノード・グループに接続されています。


@racketblock[
    Group (RootNode, ID 0)
      /
     /
Synth (ID 1000)
::

この図の状態を生成するためのコードは

]

@racketblock[
(
SynthDef("ringModulation", {
	Out.ar(
		0,
		Mix.ar(
			SinOsc.ar([440.067, 441.013], 0, 1)
			*
			SinOsc.ar([111, 109], 0, 0.2)
		)
	)
}).add;
)

Synth("ringModulation");
::

です。


////////////////////////////////////////////////////////////////////////////////////////////////////

ルートノードに二つのシンセが接続されている図は次の様になります。

]

@racketblock[
    Group (RootNode, ID 0)
      /\
     /  \
Synth    Synth
(ID 1000) (ID 1001)
::

この図の状態を生成するためのコードは

]

@racketblock[
(
SynthDef("pitchFromNoise", { arg out = 0;
	Out.ar(
		out,
		Resonz.ar(
			WhiteNoise.ar(15),
			LFNoise0.kr(2, 110, 660),
			0.005
		)
	)
}).add;
)


(
Synth("ringModulation");
Synth("pitchFromNoise", [\out, 1]);
)
::

です。


////////////////////////////////////////////////////////////////////////////////////////////////////

ノードが確実に正しい順序に並べられる様にするというのはユーザーの責任です。そういうわけで、２つのシンセは与えられた順序で評価されなければならないのです。

]

@racketblock[
(
SynthDef("firstNode-source", {
	Out.ar(
		0,
		Saw.ar([200, 201], 0.05)
	)
}).add;

SynthDef("secondNode-filter", {
	ReplaceOut.ar(
		0,
		LPF.ar(
			In.ar(0, 2),
			Lag.kr(
				LFNoise0.kr([4, 4.001], 500, 1000),
				0.1
			)
		)
	)
}).add;
)

// "firstNode-source"は最初に実行されなければならない
//  もし "secondNode-filter"が先に評価されれば、"firstNode-source"はそれを通して処理されなくなってしまう
(
Synth("firstNode-source");
Synth("secondNode-filter")
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

この２つのシンセを記述するための安全な方法は次の様になります。

]

@racketblock[
(
Synth.head(s, "firstNode-source");
Synth.tail(s, "secondNode-filter");
)
::

この例では、適切な順序でシンセを配置するのに.headメッセージと.tailメッセージに頼っています。ここでの適切な順序とは、"firstNode-source"がルートノード・グループの先頭に、"secondNode-filter"がルートノード・グループの最後に、ということです。

それゆえ、"firstNode-source"はリンクリストの中で第1番のアイテムになり、"secondNode-filter"は第2のアイテムになります。この順序付けは２つのシンセの評価順序が変更されたとしても同じです。

]

@racketblock[
(
Synth.tail(s, "secondNode-filter");
Synth.head(s, "firstNode-source");
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

先の例を記述するためのよりよい方法は、２つのシンセをそれぞれのグループに配置するということです。

]

@racketblock[
    Group (RootNode, ID 0)
      /\
     /  \
Group    Group
  |        |
  |        |
Synth    Synth
::

]

@racketblock[
(
~source = Group.head(s);	// グループをルートノードの先頭に加える
~effect = Group.tail(s);	// グループをルートノードの最後に加える
)

(
// シンセを適切なグループに追加する
Synth.head(~effect, "secondNode-filter");
Synth.head(~source, "firstNode-source");
)
::

この考え方は、ルートノードに対してシンセではなくグループを求められる順序で結びつけるということです。一度グループがルートノードに求められる順序で結びつけられると、それによってシンセはしかるべきグループに結びつけられるようにできます。

////////////////////////////////////////////////////////////////////////////////////////////////////

グループは、シンセを確実に適切な順序で実行するようにするという以外の目的にも用いることができます。例えば、個々のシンセに対してではなく、グループに対して１つのメッセージを送ることで、グループの中の全てのシンセに対して１つのコントロールをセットすることができます。

]

@racketblock[
// 以下のSynthDefは全て共通のコントロール名（mul）を持つ
(
// 3つのSynthDefとグループを作成する
SynthDef("synthNumber1", { arg mul = 0.2;
	Out.ar(
		0,
		BrownNoise.ar(mul) * LFNoise0.kr([1, 1.01])
	)
	}, [0.1]).add;
SynthDef("synthNumber2", { arg mul = 0.2;
	Out.ar(
		0,
		WhiteNoise.ar(mul) * LFNoise1.kr([2.99, 3])
	)
	}, [0.1]).add;
SynthDef("synthNumber3", { arg mul = 0.2;
	Out.ar(
		0,
		PinkNoise.ar(mul) * LFNoise2.kr([0.79, 0.67])
	)
}, [0.1]).add;
)

(
// グループを作成する
~myGroup = Group.new;
)

(
// 3つのシンセを加える
Synth.head(~myGroup, "synthNumber1");
Synth.head(~myGroup, "synthNumber2");
Synth.head(~myGroup, "synthNumber3");
)

// 3つのシンセのそれぞれの\mulコントロール、新しい（そして同じ）値にセットする
~myGroup.set(\mul, 0.05)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/Japanese_version/12::
]


