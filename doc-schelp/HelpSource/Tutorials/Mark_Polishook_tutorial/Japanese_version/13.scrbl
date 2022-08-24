#lang scribble/manual
@(require (for-label racket))

@title{13}
 Mark Polishook tutorial (JP)@section{categories}
  Tutorials>Mark_Polishook_tutorial>Japanese_version
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 時間ベースのフィルター

Delay、CombそしてAlpassといったUGenは時間ベースのエフェクトを生成します。これは空間や位置の感覚を与えるリバーブなどのエフェクトを作るのに適しています。

////////////////////////////////////////////////////////////////////////////////////////////////////


@racketblock[
// 2つのSynthDef。第1のSynthDefはグレインを生成し、第2のSynthDefはそれらを遅らせる
//  グレインを生成するSynthDefは左チャンネル
// グレインを遅らせるSynthDefは右チャンネル
(
SynthDef("someGrains", { arg centerFreq = 777, freqDev = 200, grainFreq = 2;
	var gate;
	gate = Impulse.kr(grainFreq);
	Out.ar(
		0,
		SinOsc.ar(
			LFNoise0.kr(4, freqDev, centerFreq),
			0,
			EnvGen.kr(Env.sine(0.1), gate, 0.1)
		)
	)
}).add;

SynthDef("aDelay", { arg delay = 0.25;
	Out.ar(
		1,
		DelayN.ar(
			In.ar(0, 1),
			delay,
			delay
		)
	)
}).add;
)

// グレインをテストして ... オフにしなさい
// 全て左チャンネルだけ ... グッド！
Synth("someGrains");


// ２つのグループを作成。第1はソース、第2はエフェクト。
(
~source = Group.head(s);
~effects = Group.tail(s);
)

// グレインをディレイに配置する ... ソースは左でディレイをかけられたソースは右
(
Synth.head(~source, "someGrains");
Synth.head(~effects, "aDelay");
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 フィードバック・フィルター

CombやAllpassフィルターは、自分の出力を入力にフィードバックするUGenの例です。Allpassフィルターは一般的にCombフィルターと比較した場合に音はあまり違って聞こえません。しかしながら、Allpassフィルターはそれを通過する信号の位相を変えます。このため、これはUGenのネットワーク中で用いる時に便利なのです。


@racketblock[
// インターナル・サーバーを起動！！
// 最初はCombフィルターで次がAllpassフィルター（パラメータは同じ）。両者を比較せよ。
(
{
	CombN.ar(
		SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
		0.3,
		0.25,
		6
	)
}.scope;
)

// 前の例とそんなに大きな違いは無い
(
{
	AllpassN.ar(
		SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
		0.3,
		0.25,
		6
	)
}.scope;
)

// 最初はCombフィルターで次がAllpassフィルター（パラメータは同じ）。両者を比較せよ。
// ２つの例はもっと短いディレイ・タイムを持つ ... 0.1秒
(
{
	CombN.ar(
		SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
		0.1,
		0.025,
		6
	)
}.scope;
)

// ディレイ・タイムを短くすると、allpassフィルターはcombフィルターよりより広がって聞こえるようになる
(
{
	AllpassN.ar(
		SinOsc.ar(500.rrand(1000), 0, 0.2) * Line.kr(1, 0, 0.1),
		0.1,
		0.025,
		6
	)
}.scope
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 リバーブレーション

次の例は、James McCartneyによるもので、.playではなく.scopeを使っているということと、変数が関数の中で宣言されているという変更を加えています。この例はSuperCollider 2の配布物の中の一部分のドキュメントであった01 Why SuperColliderからのものです。

これは事実上Schroederのリバーブの実装で、信号はCombフィルターの並列バンクを通過し、連続したいくつかのAllpassフィルターを通るというものです。


@racketblock[
(
{
var s, z, y;
	// ランダムな10ボイス分のサイン波のパーカッション・サウンド：
s = Mix.ar(Array.fill(10, { Resonz.ar(Dust.ar(0.2, 50), 200 + 3000.0.rand, 0.003)}) );
	// リバーブのプリ・ディレイ・タイム：
z = DelayN.ar(s, 0.048);
	// 並列に7つの長さを変調されたcombによるディレイ：
y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, LFNoise1.kr(0.1.rand, 0.04, 0.05), 15) }));
	// 4つのallpassによるディレイの２つの並列したチェーン（合計8個）：
4.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
	// オリジナルのサウンドをリバーブに加えてそれを再生：
s+(0.2*y)
}.scope
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 コンポーネント

次の例は、01 Why SuperColliderドキュメントの、つい先ほど説明したばかりのシンセシスのプロセスを、小さくシンプルな部分に分割する方法を示します。それはまた信号を並列にフィルタリングする方法（"combs"のSynthDefを参照）と、信号をシリーズでフィルタリングする方法（"allpass"のSynthDefを参照）を示します。この例はまた、どのようにシンセが実行されるたびにランダムに生成されるコントロール値を使うのかということをデモするものです。

この例はまた、便宜上、どのようにして複数チャンネルのオーディオを任意の数のバスに広げることができるのかということを示します。つまり、バスに渡ってオーディオを広げるバスのアサインはSynthDefの中で固定されているものの、簡単にコントロールにアサインすることができるということです。

前の例をより小さな部分に分解することのアドバンテージは、オーディオ・バスに送られる最初の要素にならなければならない、ソースのオーディオを除いて、残りの部分は任意の順序に組み替え直すことができるということです。


@racketblock[
(
SynthDef("filteredDust", {
	Out.ar(
		2,
		Mix.arFill(10, { Resonz.ar(Dust.ar(0.2, 50), Rand(200, 3200), 0.003) })
	)
}).add;

SynthDef("preDelay", {
	Out.ar(
		4,
		DelayN.ar(In.ar(2, 1), 0.048, 0.048)
	)
}).add;

SynthDef("combs", {
	Out.ar(
		6,
		Mix.arFill(7, { CombL.ar(In.ar(4, 1), 0.1, LFNoise1.kr(Rand(0, 0.1), 0.04, 0.05), 15) })
	)
}).add;

SynthDef("allpass", { arg gain = 0.2;
	var source;
	source = In.ar(6, 1);
	4.do({ source = AllpassN.ar(source, 0.050, [Rand(0, 0.05), Rand(0, 0.05)], 1) });
	Out.ar(
		8,
		source * gain
	)
}).add;

SynthDef("theMixer", { arg gain = 1;
	Out.ar(
		0,
		Mix.ar([In.ar(2, 1), In.ar(8, 2)]) * gain
	)
}).add;
)

// それぞれの行が実行されると、それぞれは最後のノードになる。結果は、"filteredDust"が最初のノードになり、
// "theMixer"が最後のノードになる。これは我々が望んだ通りである。
(
Synth.tail(s, "filteredDust");
Synth.tail(s, "preDelay");
Synth.tail(s, "combs");
Synth.tail(s, "allpass");
Synth.tail(s, "theMixer");
)
::

////////////////////////////////////////////////////////////////////////////////////////////////////

または、グループを使って前の例のシンセの実行の順序をコントロールします。

]

@racketblock[
(
~source = Group.tail(s);
~proc1 = Group.tail(s);
~proc2 = Group.tail(s);
~proc3 = Group.tail(s);
~final = Group.tail(s);
)

// 以下のノードは、上で並べた通りに、グループにアサインされる
// それゆえ、これらは正しい順序で実行される
(
Synth.head(~final, "theMixer");
Synth.head(~proc3, "allpass");
Synth.head(~proc2, "combs");
Synth.head(~proc1, "preDelay");
Synth.head(~source, "filteredDust");
)
::

上に示した様に正しい順序にグループを配置することはそれにアサインされるシンセもまた正しい順序になるということを保証します。

////////////////////////////////////////////////////////////////////////////////////////////////////

参考までに、以下はSuperCollider 2の配布物からの（James McCartheyによる）01 Why SuperColliderドキュメントの完全なテキストです。

////////////////////////////////////////////////////////////////////////////////////////////////////

For context, here, below, is the complete text of the strong::01 Why SuperCollider:: document (by James McCartney) from the SuperCollider 2 distribution.

]
@section{section}
 Why SuperCollider 2.0 ?

SuperCollider version 2.0 is a new programming language. strong::Why invent a new language and not use an existing language?:: Computer music composition is a specification problem. Both sound synthesis and the composition of sounds are complex problems and demand a language which is highly expressive in order to deal with that complexity. Real time signal processing is a problem demanding an efficient implementation with bounded time operations.
There was no language combining the features I wanted and needed for doing digital music synthesis. The SuperCollider language is most like Smalltalk. Everything is an object. It has class objects, methods, dynamic typing, full closures, default arguments, variable length argument lists, multiple assignment, etc. The implementation provides fast, constant time method lookup, real time garbage collection, and stack allocation of most function contexts while maintaining full closure semantics.
The SuperCollider virtual machine is designed so that it can be run at interrupt level. There was no other language readily available that was high level, real time and capable of running at interrupt level.

SuperCollider version 1.0 was completely rewritten to make it both more expressive and more efficient. This required rethinking the implementation in light of the experience of the first version. It is my opinion that the new version has benefitted significantly from this rethink. It is not simply version 1.0 with more features.

strong::Why use a text based language rather than a graphical language? ::
There are at least two answers to this. strong::Dynamism:: : Most graphical synthesis environments use statically allocated unit generators. In SuperCollider, the user can create structures which spawn events dynamically and in a nested fashion. Patches can be built dynamically and parameterized not just by floating point numbers from a static score, but by other graphs of unit generators as well. Or you can construct patches algorithmically on the fly. This kind of fluidity is not possible in a language with statically allocated unit generators.
strong::Brevity:: : In SuperCollider, symmetries in a patch can be exploited by either multichannel expansion or programmatic patch building. For example, the following short program generates a patch of 49 unit generators. In a graphical program this might require a significant amount of time and space to wire up. Another advantage is that the size of the patch below can be easily expanded or contracted just by changing a few constants.


@racketblock[
(
{
	// 10 voices of a random sine percussion sound :
s = Mix.ar(Array.fill(10, { Resonz.ar(Dust.ar(0.2, 50), 200 + 3000.0.rand, 0.003)}) );
	// reverb predelay time :
z = DelayN.ar(s, 0.048);
	// 7 length modulated comb delays in parallel :
y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, LFNoise1.kr(0.1.rand, 0.04, 0.05), 15) }));
	// two parallel chains of 4 allpass delays (8 total) :
4.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
	// add original sound to reverb and play it :
s+(0.2*y)
}.play )
::

Graphical synthesis environments are becoming a dime a dozen. It seems like a new one is announced every month. None of them have the dynamic flexibility of SuperCollider's complete programming environment. Look through the SuperCollider help files and examples and see for yourself.

go to link::Tutorials/Mark_Polishook_tutorial/Japanese_version/14::
]


