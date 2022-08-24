#lang scribble/manual
@(require (for-label racket))

@title{06}
 Mark Polishook tutorial (JP)@section{categories}
  Tutorials>Mark_Polishook_tutorial>Japanese_version
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 SynthDefのプロパティに関する議論を続けるために、次を評価してみて下さい。


@racketblock[
(
SynthDescLib.global.read;
SynthDescLib.global.browse;
)
::

そして、それぞれのシンセのためのコントロールをリストと表示するボックスに注目して下さい。

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 コントロールは（大抵は）アーギュメントである

シンセが生成される時や、動作中に情報を与えたいと思う時には、SynthDefにコントロールをセットします。このための最も一般的な方法は、1つ、またはそれ以上のアーギュメントをSynthDefに含めるということです。

////////////////////////////////////////////////////////////////////////////////////////////////////

次の例では、それぞれのアーギュメントはデフォルト値を持ちます。そのような場合には、もし必要なところにアーギュメントを与えるのを忘れたとしても、SynthDefが自動的に供給します。加えて、シンセが生成される時に必要となるものを示すアーギュメントに対するデフォルト値は、コードがより読みやすくします。


@racketblock[
(
// デフォルト値を持つ2つのアーギュメント
SynthDef("withControls", { arg freq = 440, mul = 0.22;
	Out.ar(
		0,
		SinOsc.ar([freq, freq+1], 0, mul)
	)
}).add;
)

Synth("withControls", [\freq, 440, \mul, 0.1]);
::

配列の中のアイテムはSynthDefのコントロールに渡される値です。

////////////////////////////////////////////////////////////////////////////////////////////////////

配列の中にそれぞれのコントロールを書く場合には、次の様にシンボル（バックスラッシュの後に名前が続く）を用いることもできますし、

]

@racketblock[
Synth("withControls", [\freq, 440, \mul, 0.1]);
::

または、次の様に文字列（引用符の中に名前）を用いることもでき、

]

@racketblock[
Synth("withControls", ["freq", 440, "mul", 0.1]);
::

その後に値が続きます。

////////////////////////////////////////////////////////////////////////////////////////////////////

いずれの場合でも、そのパターンは、

[ controlName, value, controlName, value].

です。

////////////////////////////////////////////////////////////////////////////////////////////////////

シンセにコントロールを渡す第３の方法もあります。前の例のシンセは次の様に書くこともできます。

]

@racketblock[
Synth("withControls", [0, 440, 1, 0.1]);
::

このパターンは次の様になります。

[ controlIndex, value, controlIndex, value].

この手法のディスアドバンテージは、コントロールをインデックス番号として表現するのは説明的ではなく、それゆえに名前をシンボルまたは文字列として与えるよりも読みにくくなるでしょう。

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 コントロールはリセットすることもできる

次の例は、シンセが生成された後に（動作中に）どのようにしてコントロールをリセットすることができるのかということを示しています。


@racketblock[
(
SynthDef("resetMyControls", { arg freq = 440, mul = 0.22;
	Out.ar(
		0,
		SinOsc.ar([freq, freq+1], 0, mul)
	)
}).add;
)

~aSynth = Synth("resetMyControls", [\freq, 440, \mul, 0.06]);
~aSynth.set(\freq, 600, \mul, 0.25);
::

aSynthの前の~はグローバル変数を定義します。このようにすることの主なアドバンテージは、次の様に明示的に宣言する必要がないということです。

]

@racketblock[
var aSynth; // '~'のない変数はまず宣言しなければならない！！
::

もっと正確に言えば、~という文字は「aSynth」という変数をcurrentEnvironmentという名前で知られるオブジェクトのインスタンスの中に配置します。詳しくは、SuperColliderのヘルプ・システムでEnvironmentというドキュメントを参照して下さい。

////////////////////////////////////////////////////////////////////////////////////////////////////

SynthDefはコントロールのそれぞれにラグ・タイムを定義することができます。ugenGraphFuncの後に配列として書くラグは、あるコントロール値から他のコントロール値にスムーズに変化する時間を指定します。

]

@racketblock[
(
SynthDef("controlsWithLags", { arg freq = 440, mul = 0.22;
	Out.ar(
		0,
		SinOsc.ar([freq, freq+1], 0, mul)
	)
}, [1, 1]).add;
)

~aSynth = Synth("controlsWithLags", [\freq, 550, \mul, 0.1]);
~aSynth.set(\freq, 600, \mul, 0.5);
::

////////////////////////////////////////////////////////////////////////////////////////////////////

SynthDefの中にラグタイムの配列を含めるというオプションがあるということは、前に議論したSynthDefのテンプレートは再定義されなければならないということを意味します。

]

@racketblock[
// 前のSynthDefのためのテンプレート。ラグタイムの配列なし。
SynthDef(
	"i am a synthdef",			// 第１アーギュメントは名前
	{ .... i am a ugenGraphFunc ... }	// 第２アーギュメントはugenGraphFunc
)

// ラグタイムの配列付きで再定義したSynthDefのためのテンプレート
// ラグタイムの配列のためのクラス定義は'rates'と呼ぶ
SynthDef(
	"i am a synthdef",			// 名前
	{ .... i am a ugenGraphFunc ... },	// ugenGraphFunc
	[ ... lagTimes ... ]			// rates
)
::

どちらのテンプレートを用いるかという選択はユーザー次第です。もし、SynthDefにラグタイムを含める必要があるのであれば、第２のテンプレート、すなわち、レートの配列を含むもの、を使います。

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/Japanese_version/07::
]


