#lang scribble/manual
@(require (for-label racket))

@title{Server Plugin API}
 Reference for writing unit generators@section{categories}
  Internals
@section{related}
  Guides/WritingUGens

@section{section}
  Input rates

These four constants identify the calculation rates of inputs in SuperCollider.

@section{definitionlist}
 
## 
@racketblock[calc_ScalarRate::
|| Initial rate. Conventionally known in the language as ".ir".

## ]

@racketblock[calc_BufRate::
|| Control rate. Conventionally known in the language as ".kr".

## ]

@racketblock[calc_FullRate::
|| Audio rate. Conventionally known in the language as ".ar".

## ]

@racketblock[calc_DemandRate::
|| Demand rate.
::

]
@section{section}
  UGen basics

These helper macros assume that there is a ugen object called 
@racketblock[unit:: in the local scope.

]
@section{definitionlist}
 
## 
@racketblock[IN(index)::
|| A single block of audio-rate input as a float* at the given index. Index 0 is the first input to the ugen, index 1 the second input, and so forth.

## ]

@racketblock[IN0(index)::
|| A single sample of control-rate input as a float, at the given index.

## ]

@racketblock[OUT(index)::
|| A single block of audio-rate output as a float* at the given index.

## ]

@racketblock[OUT0(index)::
|| A single sample of control-rate input as a float, at the given index.

## ]

@racketblock[INRATE(index)::
|| Get the rate of a given input index. This will be one of the four rates.

## ]

@racketblock[INBUFLENGTH(index)::
|| Get the block size of a given input index.

## ]

@racketblock[SAMPLERATE::
|| Sample rate of the server in Hertz.

## ]

@racketblock[SAMPLEDUR::
|| Sample period of the server in seconds.

## ]

@racketblock[BUFLENGTH::
|| Length in samples of an audio buffer (that is, the number of samples in a control period).

## ]

@racketblock[BUFRATE::
|| Control rate of the server in Hertz.

## ]

@racketblock[BUFDUR::
|| Control period of the server in seconds.

## ]

@racketblock[FULLRATE::
||

## ]

@racketblock[FULLBUFLENGTH::
||
::

]
@section{section}
  Buffers

@section{definitionlist}
 
## 
@racketblock[GET_BUF::
|| The recommended way to retrieve a buffer. Take the first input of this UGen and use it as a buffer number. This dumps
a number of variables into the local scope:

]
@section{list}
 
## 
@racketblock[buf:: - a pointer to the ]

@racketblock[SndBuf:: instance
## ]

@racketblock[bufData:: - the raw float data from the buffer
## ]

@racketblock[bufChannels:: - the number of channels in the buffer
## ]

@racketblock[bufSamples:: - the number of samples in the buffer
## ]

@racketblock[bufFrames:: - the number of frames in the buffer
::

The buffer is locked using the ]

@racketblock[LOCK_SNDBUF:: macro. Buffer lock operations are specific to supernova, and don't
do anything in vanilla scsynth.

## ]

@racketblock[GET_BUF_SHARED::
|| Like ]

@racketblock[GET_BUF::, but the buffer is locked using ]

@racketblock[LOCK_SNDBUF_SHARED::.

## ]

@racketblock[SIMPLE_GET_BUF::
|| Like ]

@racketblock[GET_BUF::, but only creates the ]

@racketblock[buf:: variable and does not lock the buffer.

## ]

@racketblock[SIMPLE_GET_BUF_EXCLUSIVE::
|| Like ]

@racketblock[SIMPLE_GET_BUF::, but locks the buffer with ]

@racketblock[LOCK_SNDBUF::.

## ]

@racketblock[SIMPLE_GET_BUF_SHARED::
|| Like ]

@racketblock[SIMPLE_GET_BUF::, but locks the buffer with ]

@racketblock[LOCK_SNDBUF_SHARED::.
::

The following macros are for use in supernova. They still exist in scsynth, but will have no effect.

]

@racketblock[
ACQUIRE_BUS_AUDIO(index)
ACQUIRE_BUS_AUDIO_SHARED(index)
RELEASE_BUS_AUDIO(index)
RELEASE_BUS_AUDIO_SHARED(index)
LOCK_SNDBUF(buf)
LOCK_SNDBUF_SHARED(buf)
LOCK_SNDBUF2(buf1, buf2)
LOCK_SNDBUF2_SHARED(buf1, buf2)
LOCK_SNDBUF2_EXCLUSIVE_SHARED(buf1, buf2)
LOCK_SNDBUF2_SHARED_EXCLUSIVE(buf1, buf2)
ACQUIRE_SNDBUF(buf)
ACQUIRE_SNDBUF_SHARED(buf)
RELEASE_SNDBUF(buf)
RELEASE_SNDBUF_SHARED(buf)
ACQUIRE_BUS_CONTROL(index)
RELEASE_BUS_CONTROL(index)
::

]
@section{section}
  RGen

RGen is a pseudorandom number generator API. Most ugen developers are not interested in seeding their own RGens and
would prefer to draw from a global RGen instance supplied by SuperCollider. This can be retrieved with the code:


@racketblock[
RGen& rgen = *unit->mParent->mRGen;
::

]
@section{definitionlist}
 
## 
@racketblock[uint32 RGen\::trand()::
|| Return a uniformly distributed random 32-bit integer.

## ]

@racketblock[double RGen\::drand()::
|| Return a uniformly distributed random double in [0,1).

## ]

@racketblock[float RGen\::frand()::
|| Random float in [0,1).

## ]

@racketblock[float RGen\::frand0()::
|| Random float in [1,2).

## ]

@racketblock[float RGen\::frand2()::
|| Random float in [-1,1).

## ]

@racketblock[float RGen\::frand8()::
|| Random float in [-0.125,0.125).

## ]

@racketblock[float RGen\::fcoin()::
|| Either -1 or +1.

## ]

@racketblock[float RGen\::flinrand()::
|| Linearly distributed random float in [0,1), with a bias towards the 0 end.

## ]

@racketblock[float RGen\::fbilinrand()::
|| Bilinearly distributed random float in (-1,1), with a bias towards 0.

## ]

@racketblock[float RGen\::fsum3rand()::
|| A crude but fast approximation to a Gaussian distribution. Results are always in the range (-1,1). The variance is
1/6 and the standard deviation is 0.41. ]
@section{footnote}
  The formula is 
@racketblock[(rand() + rand() + rand() - 1.5) * 2/3::,
technically a shifted and stretched order-3 Irwin-Hall distribution. ::

## ]

@racketblock[int32 RGen\::irand(int32 scale)::
|| Random int in [0,scale).

## ]

@racketblock[int32 RGen\::irand2(int32 scale)::
|| Random int in [-scale,+scale].

## ]

@racketblock[int32 RGen\::ilinrand(int32 scale)::
|| Linearly distributed random int in [0,scale), with a bias towards the 0 end.

## ]

@racketblock[int32 RGen\::ibilinrand(int32 scale)::
|| Bilinearly distributed random int in (-scale,scale), with a bias towards the 0.

## ]

@racketblock[double RGen\::linrand()::
|| Linearly distributed random double in [0,1), with a bias towards the 0 end.

## ]

@racketblock[double RGen\::bilinrand()::
|| Bilinearly distributed random double in (-1,1), with a bias towards 0.

## ]

@racketblock[double RGen\::exprandrng(double lo, double hi)::
|| Exponentially distributed random double in [lo,hi).

## ]

@racketblock[double RGen\::exprand(double scale)::
||

## ]

@racketblock[double RGen\::biexprand(double scale)::
||

## ]

@racketblock[double RGen\::exprand(double scale)::
||

## ]

@racketblock[double RGen\::sum3rand(double scale)::
|| Double version of ]

@racketblock[RGen\::fsum3rand::.
::

]
@section{section}
  Unary operators

@section{definitionlist}
 
## 
@racketblock[bool sc_isnan(float/double x)::
|| Checks whether ]

@racketblock[x:: is NaN. This is a legacy function, use ]

@racketblock[std\::isnan:: instead.

## ]

@racketblock[bool sc_isfinite(float/double x)::
|| Checks whether ]

@racketblock[x:: is finite. This is a legacy function, use ]

@racketblock[std\::isfinite:: instead.

## ]

@racketblock[int32 sc_grayCode(int32 x)::
|| Convert binary to Gray code.
::

The following unary functions are available for both float32 and float64, and are the same as in sclang (minus the "sc_"
prefixes):

]
@section{list}
 
## 
@racketblock[sc_midicps::
## ]

@racketblock[sc_cpsmidi::
## ]

@racketblock[sc_midiratio::
## ]

@racketblock[sc_ratiomidi::
## ]

@racketblock[sc_octcps::
## ]

@racketblock[sc_cpsoct::
## ]

@racketblock[sc_ampdb::
## ]

@racketblock[sc_dbamp::
## ]

@racketblock[sc_cubed::
## ]

@racketblock[sc_sqrt::
## ]

@racketblock[sc_hanwindow::
## ]

@racketblock[sc_welwindow::
## ]

@racketblock[sc_triwindow::
## ]

@racketblock[sc_rectwindow::
## ]

@racketblock[sc_scurve::
## ]

@racketblock[sc_ramp::
## ]

@racketblock[sc_sign::
## ]

@racketblock[sc_distort::
## ]

@racketblock[sc_softclip::
## ]

@racketblock[sc_ceil::
## ]

@racketblock[sc_floor::
## ]

@racketblock[sc_reciprocal::
## ]

@racketblock[sc_frac::
## ]

@racketblock[sc_log2:: (legacy -- use ]

@racketblock[std\::log2(std\::abs(x))::)
## ]

@racketblock[sc_log10:: (legacy -- use ]

@racketblock[std\::log10(std\::abs(x))::)
## ]

@racketblock[sc_trunc:: (legacy -- use ]

@racketblock[std\::trunc::)
::

The following unary functions are available for both float32 and float64, but have no sclang equivalent:

]
@section{definitionlist}
 
## 
@racketblock[zapgremlins(x)::
|| Replaces NaNs, infinities, very large and very small numbers (including denormals) with zero. This is useful in ugen
feedback to safeguard from pathological behavior. (Note lack of sc_ prefix.)

## ]

@racketblock[sc_bitriwindow(x)::
|| Alternative to ]

@racketblock[sc_triwindow:: using absolute value.

## ]

@racketblock[sc_scurve0(x)::
|| Same as ]

@racketblock[sc_scurve::, but assumes that ]

@racketblock[x:: is in the interval [0, 1].

## ]

@racketblock[sc_distortneg(x)::
|| A one-sided distortion function. Same as ]

@racketblock[distort:: for ]

@racketblock[x > 0::, and the identity function for ]

@racketblock[x <= 0::.

## ]

@racketblock[taylorsin(x)::
|| Taylor series approximation of ]

@racketblock[sin(x):: out to ]

@racketblock[x**9 / 9!::. (Note lack of sc_ prefix.)

## ]

@racketblock[sc_lg3interp(x1, a, b, c, d)::
|| Cubic Lagrange interpolator.

## ]

@racketblock[sc_CalcFeedback(delaytime, decaytime)::
|| Determines the feedback coefficient for a feedback comb filter with the given delay and decay times.

## ]

@racketblock[sc_wrap1(x)::
|| Wrap ]

@racketblock[x:: around ±1, wrapping only once.

## ]

@racketblock[sc_fold1(x)::
|| Fold ]

@racketblock[x:: around ±1, folding only once.
::

]
@section{section}
  Binary operators

@section{definitionlist}
 
## 
@racketblock[sc_wrap(in, lo, hi [, range])::
||

## ]

@racketblock[sc_fold(in, lo, hi [, range [, range2]])::
||

## ]

@racketblock[sc_pow(a, b)::
|| Compute ]

@racketblock[pow(a, b)::, retaining the sign of ]

@racketblock[a::.

## ]

@racketblock[sc_powi(x, unsigned int n)::
|| Compute ]

@racketblock[x^n::, not necessarily retaining the sign of ]

@racketblock[x::.

## ]

@racketblock[sc_hypotx(x, y)::
|| Compute ]

@racketblock[abs(x) + abs(y) - (min(abs(x), abs(y)) * (sqrt(2) - 1))::, the minimum distance one will have to travel
from the origin to (x,y) using only orthogonal and diagonal movements.
::

The following functions are the same as in sclang (minus the "sc_" prefixes):

]
@section{list}
 
## 
@racketblock[sc_mod(in, hi):: (floats, doubles, ints)
## ]

@racketblock[sc_round(x, quant):: (floats, doubles, ints)
## ]

@racketblock[sc_roundUp(x, quant):: (floats, doubles, ints)
## ]

@racketblock[sc_trunc(x, quant):: (floats, doubles, ints)
## ]

@racketblock[sc_gcd(a, b):: (ints, longs, floats)
## ]

@racketblock[sc_lcm(a, b):: (ints, longs, floats)
## ]

@racketblock[sc_bitAnd(a, b):: (ints)
## ]

@racketblock[sc_bitOr(a, b):: (ints)
## ]

@racketblock[sc_leftShift(a, b):: (ints)
## ]

@racketblock[sc_rightShift(a, b):: (ints)
## ]

@racketblock[sc_unsignedRightShift(a, b):: (ints)
## ]

@racketblock[sc_thresh(a, b)::
## ]

@racketblock[sc_clip2(a, b)::
## ]

@racketblock[sc_wrap2(a, b)::
## ]

@racketblock[sc_fold2(a, b)::
## ]

@racketblock[sc_excess(a, b)::
## ]

@racketblock[sc_scaleneg(a, b)::
## ]

@racketblock[sc_amclip(a, b)::
## ]

@racketblock[sc_ring1(a, b)::
## ]

@racketblock[sc_ring2(a, b)::
## ]

@racketblock[sc_ring3(a, b)::
## ]

@racketblock[sc_ring4(a, b)::
## ]

@racketblock[sc_difsqr(a, b)::
## ]

@racketblock[sc_sumsqr(a, b)::
## ]

@racketblock[sc_sqrsum(a, b)::
## ]

@racketblock[sc_sqrdif(a, b)::
## ]

@racketblock[sc_atan2(a, b):: (legacy -- use ]

@racketblock[std\::atan2::)
::

]
@section{section}
  Constants

The following constants are doubles:

@section{list}
 
## 
@racketblock[pi::
## ]

@racketblock[pi2:: = pi/2
## ]

@racketblock[pi32:: = 3pi/2
## ]

@racketblock[twopi:: = 2pi
## ]

@racketblock[rtwopi:: (1/2pi)
## ]

@racketblock[log001:: = log(0.001)
## ]

@racketblock[log01:: = log(0.01)
## ]

@racketblock[log1:: = log(0.1)
## ]

@racketblock[rlog2:: = 1/log(2)
## ]

@racketblock[sqrt2:: = sqrt(2)
## ]

@racketblock[rsqrt2:: = 1/sqrt(2)
## ]

@racketblock[truncDouble:: = 3 * 2^51 (used to truncate precision)
::

The following constants are floats:

]
@section{list}
 
## 
@racketblock[pi_f::
## ]

@racketblock[pi2_f::
## ]

@racketblock[pi32_f::
## ]

@racketblock[twopi_f::
## ]

@racketblock[sqrt2_f::
## ]

@racketblock[rsqrt2_f::
## ]

@racketblock[truncFloat:: = 3 * 2^22 (used to truncate precision)
::

]
@section{section}
  Unroll macros

The macros in this section are legacy features. They are seen in many of SuperCollider's built-in ugens, and are
intended to provide more efficient alternatives to the standard 
@racketblock[for (int i = 0; i < inNumSamples; i++) { out[i]
= in[i] }:: loop. These efficiency savings are negligible on modern systems and use of these macros is not recommended,
especially since they make debugging difficult.

]
@section{definitionlist}
 
## 
@racketblock[LOOP(length, stmt)::
|| Execute code ]

@racketblock[stmt::, ]

@racketblock[length:: times.

## ]

@racketblock[LOOP1(length, stmt)::
|| A faster drop-in alternative to ]

@racketblock[LOOP::, which assumes that ]

@racketblock[length > 0:: so a branch instruction is saved.

## ]

@racketblock[LooP(length) stmt::
|| An alternative to LOOP/LOOP1 that is more debugger-friendly. The body of the loop comes after the call to ]

@racketblock[LooP::.

## ]

@racketblock[ZIN(index)::
|| Similar to ]

@racketblock[IN::, but subtracts 1 from the pointer to correct for off-by-one errors when using ]

@racketblock[LOOP:: and ]

@racketblock[ZXP::.

## ]

@racketblock[ZOUT(index)::
|| Same as ]

@racketblock[OUT::, but subtracts 1 from the pointer to correct for off-by-one errors when using ]

@racketblock[LOOP:: and ]

@racketblock[ZXP::.

## ]

@racketblock[ZIN0(index)::
|| Alias for ]

@racketblock[IN0::.

## ]

@racketblock[ZOUT0(index)::
|| Alias for ]

@racketblock[OUT0::.

## ]

@racketblock[ZXP(z)::
|| Pre-increment and dereference ]

@racketblock[z::.

## ]

@racketblock[ZX(z)::
|| Dereference ]

@racketblock[z::.

## ]

@racketblock[PZ(z)::
|| Pre-increment ]

@racketblock[z::.

## ]

@racketblock[ZP(z)::
|| Does nothing.

## ]

@racketblock[ZOFF::
|| Return 1.
]


