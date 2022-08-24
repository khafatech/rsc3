#lang scribble/manual
@(require (for-label racket))

@title{08}
 Mark Polishook tutorial (JP)@section{categories}
  Tutorials>Mark_Polishook_tutorial>Japanese_version
@section{related}
  Tutorials/Mark_Polishook_tutorial/00_Introductory_tutorial

@section{section}
 1つの、ただ1つのUGen

最も簡単なシンセシスのプロセスはただ1つのUGenを実行するものです。例えば、


@racketblock[
{ Saw.ar(500, 0.1) }.scope;
::

または

]

@racketblock[
{ Formlet.ar(Saw.ar(22), 400, 0.01, 0.11, 0.022) }.scope
::

です。

様々なUGenのためのSuperColliderのヘルプ・ドキュメントのほとんどがそのような例を示してくれるでしょう。

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 マルチ・チャンネルへの拡張

任意のアーギュメント（コントロール）スロットに配列を与えることで、1つのUGenを2チャンネルに拡張できます。


@racketblock[
{ Saw.ar([500, 933], 0.1) }.scope;
::

同じことを他の（長い）書き方で書くと次の様になります。

]

@racketblock[
{ [ Saw.ar(500, 0.1), Saw.ar(933, 0.1)] }.scope;
::

////////////////////////////////////////////////////////////////////////////////////////////////////

]
@section{section}
 UnaryOpUGenの探求

UGenにメッセージを送ることで、多くのシンセシス・プロセスを生成することができます。UnaryOpsフォルダの中のSuperColliderヘルプ・システムの中に、多くの単項演算に関するヘルプファイルがあります。

////////////////////////////////////////////////////////////////////////////////////////////////////

これを


@racketblock[
{ SinOsc.ar(500, 0, 0.5) }.scope;
::

次のものと比較してみて下さい。

]

@racketblock[
{ SinOsc.ar(500, 0, 0.5).distort }.scope;
::

.distortメッセージ（UGenに送られたメッセージ）は単項演算です。.distortメッセージがSinOsc UGenの出力を変調しているのが聞こえるでしょう。これによってより多くのパーシャルが生まれます。

////////////////////////////////////////////////////////////////////////////////////////////////////

質問：.distortメッセージはどこから来ているのでしょうか？

答え：それはAbstractFunctionの中で定義されています。UGenクラスはAbstractFunctionのサブクラスです。原理的には、AbstractFunctionの中で定義されている全てのメソッドがUGenに適用できるはずです。

以下のものを

]

@racketblock[
{ SinOsc.ar(500, 0, 0.5) }.scope;
::

次のものと比較してみて下さい。

]

@racketblock[
// .cubedは単項演算の一つ
{ SinOsc.ar(500, 0, 0.5).cubed }.scope;
::

////////////////////////////////////////////////////////////////////////////////////////////////////

go to link::Tutorials/Mark_Polishook_tutorial/Japanese_version/09::
]


