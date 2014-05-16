
## TODO



* is there a linux commandline osc server (or could I make one) that dumps all messages it receives on the terminal?
	- Yes! dumpOSC in the rack-osc berkely libs




## SuperCollider stuff


### Installation/setup


scsynth in
	/Volumes/SuperCollider/SuperCollider/SuperCollider.app/Contents/Resources/scsynth

or /Applications/...


#### Ubuntu 12.10


Used [supercollider 3.6.6 ppa](https://launchpad.net/~supercollider/+archive/ppa) for ubuntu 12.10

Jackd needs to use the alsa driver, no real time, no mem lock, Audio: Playback Only.


* To setup PulseAudio to use jack instead of Alasa: (so other sounds will play besides jack sounds)
	`http://trac.jackaudio.org/wiki/WalkThrough/User/PulseOnJack`

1. Redirect alsa to PulseAudio, put in `~/.asoundrc`:

		pcm.pulse {
			type pulse
		}

		ctl.pulse {
			type pulse
		}

		pcm.!default {
			type pulse
		}
		ctl.!default {
			type pulse
		}
2. apt-get install pulseaudio-module-jack

3. put in `~/.pulse/default.pa`:

		load-module module-native-protocol-unix
		load-module module-jack-sink channels=2
		load-module module-jack-source channels=2
		load-module module-null-sink
		load-module module-stream-restore
		load-module module-rescue-streams
		load-module module-always-sink
		load-module module-suspend-on-idle
		set-default-sink jack_out
		set-default-source jack_in

#### Starting

start jackd using qjackctl.

Important note: If starting `scsynth`, need to use `qjackctl` to connect SuperCollider->out_1 to the system output using the Connect button.


---

starting using commandline:


    jack_control  start
    scsynth -u 57110
    # after scsynth starts:
    jack_connect -s default SuperCollider:out_1 system:playback_1
    jack_connect -s default SuperCollider:out_2 system:playback_2




### Programs

    { [SinOsc.ar(440, 0, 0.2), SinOsc.ar(442, 0, 0.2)] }.play;


// example of a function with args

f = { arg a; a.value + 3 };    // call 'value' on the arg; polymorphism awaits!
f.value(3);            // 3.value = 3, so this returns 3 + 3 = 6
g = { 3.0.rand; };

// can use keyword args
{SinOsc.ar(mul:0.1, freq:440)}.scope


* Synthdef with args. To run, select synthdef, then press Ctrl+Enter. Do same for each line

		(
		SynthDef("Switch", { |out, freq = 800, sustain = 1, amp = 0.1|
			Out.ar(out,
				SinOsc.ar(freq, 0, 0.2) * Line.kr(amp, 0, sustain, doneAction: 2)
			)
		}).add;
		)
		a = Synth("Switch");
		a.run(false)


* Synth functions

a = {arg freq=440; SinOsc.ar(freq)*0.1}.play
// or {| freq=440| SinOsc.ar(freq)*0.1}.play
a.set(\freq,330) //change frequency! 


### Server Architecture


Question: What's the relationship between nodes, groups, synthdefs, and ugens?

* a synthdef is the "class", or blueprint to create a synth (objects), which contains ugens, generates sounds.

* synths and groups are both nodes.



* On Linux, synthdefs loaded/saved by server are stored in `~/.local/share/SuperCollider/synthdefs/`




## Alternative clients


* SC Clients
http://supercollider.sourceforge.net/wiki/index.php/Systems_interfacing_with_SC


* Some osc clients for SC exist "including rsc3, a Scheme client, hsc3, based on Haskell, ScalaCollider, based on Scala, and Overtone, based on Clojure"

Rutz, H. H. (2010). "Rethinking the SuperCollider Client...". Proceedings of SuperCollider Symposium. Berlin. CiteSeerX: 10.1.1.186.9817.


### Overtone (SC Client)

- Tried it 12 Jan 2014. Works on my laptop. Easy to install and start.


- code to compile a synthdef: `src/overtone/sc/synth.clj`
- code defines syntdef format: `src/overtone/sc/machinery/synthdef.clj`


### RSC3 (Scheme)

#### Installing on Ubuntu 12.10


* Instructions from http://rd.slavepianos.org/ut/rsc3-texts/lss/rsc3-tutorial.html plus a little detective work.

- install darcs: `aptitude install darcs`
- "clone" the repos listed in http://rd.slavepianos.org/ut/rsc3-texts/lss/rsc3-tutorial.html

- install Ikarus (a scheme implementation) to compile the libraries.
	* `apt-get install ikarus`

	* Set the lib path. Put in ~/.bashrc
		IKARUS_LIBRARY_PATH=~/opt/lib/r6rs/
		export IKARUS_LIBRARY_PATH

- go to each of the 4 repos' mk dir and do `make prefix=~/opt`


- now `~/opt/lib/r6rs/` should look like:
		$ ls ~/opt/lib/r6rs/
		mk-r6rs.sls  rsc3.ikarus.sls    sosc.ikarus.sls
		rhs.sls      rsc3.mzscheme.sls  sosc.mzscheme.sls


- install libs into PLT scheme:
	
		$ plt-r6rs --install ~/opt/lib/r6rs/rhs.sls
		$ plt-r6rs --install ~/opt/lib/r6rs/sosc.mzscheme.sls
		$ plt-r6rs --install ~/opt/lib/r6rs/rsc3.mzscheme.sls




## References


* OSC Command Reference
http://doc.sccode.org/Reference/Server-Command-Reference.html

* Server Architecture
http://doc.sccode.org/Reference/Server-Architecture.html

* SynthDef specs
http://doc.sccode.org/Reference/Synth-Definition-File-Format.html

* Blogs about SC
http://supercollider.github.io/community/blogs-and-sites.html


## Porting rsc3

### rhs

things to change:

* nil -> null

* `(define-record-type duple (fields p q))` to

	`(struct duple (p q))`

* `orX`, `andX` to or, and

* `otherwise` to `else`

* "Beware that a pair in R6RS corresponds to a mutable pair in racket/base."
    from docs


### Sosc

* **Good examples of using udp/tcp in transport.scm and ip.scm**
    * e.g. timeout, wait, a new recv

Has the files:

"../src/plt/ip.ss"
"../src/bytevector.scm"
"../src/sosc.scm"
"../src/transport.scm"



### rsc3

Two paths:
1. either port sosc to Racket
2. or make rsc3 use Clements's osc package

* funcs used from sosc.scm:
	- message
	- encode-*
	- bundle

* How I found these (what rsc3.scm uses from sosc.scm)
    
        $ grep 'define' sosc.scm | awk '{print $2}' > funcs-in-sosc.txt
        $ for func in `cat funcs-in-sosc.txt`;
            do grep $func ../../rsc3/src/rsc3.scm;
          done
		


* transport.scm uses `encode-osc`


* funcs used from 

* Required libraries:
	- `bytevector-*` needs `(require rnrs/bytevectors-6)`
	
	`(require srfi/27)`

* adding import prefixes: e.g: `(require (prefix-in osc: osc))`




* Do synthdefs support named parameters, such as this sclang synthdef?

indeed.  see 'letc', which is syntax over 'make-control*'.  ie.

(letc ((freq 440)) (out 0 (mul (sin-osc ar freq 0) 0.1)))




### Errors

> (define msg  (message "/hi" (list 1 2)))
> msg
(mcons "/hi" (mcons 1 (mcons 2 '())))
> (encode-osc msg)

		 car: contract violation
		  expected: pair?
		  given: (mcons 1 (mcons 2 '()))

Doesn't show where the error is.


### Creating a package


Two main references:

* file:///home/pack/racket/doc/pkg/getting-started.html?q=package&q=creating%20package&q=packages&q=developing%20packages&q=package#%28part._how-to-create%29

* file:///home/pack/racket/doc/pkg/Package_Concepts.html?q=package&q=creating%20package&q=packages&q=developing%20packages&q=package#%28tech._package._catalog%29


git://github.com/‹user›/‹repo›
git://github.com/quakehead/rsc3


## Testing

* Simple expression testing

	(require rackunit)
	(check-equal? (+ 1 1) 2)

* Grouping expressions. From "The Philosophy of RackUnit"
	
	(test-begin
	  (setup-some-state!)
	  (check-equal? (foo! 1) 'expected-value-1)
	  (check-equal? (foo! 2) 'expected-value-2))
	
	or
	
	(test-case
		"name"
		... same as test-begin)

	

### functions in sosc collection

- encode-osc is equivalent to osc-element->bytes

	- call stack:
	
	encode-osc
		encode-message m
			encode-string
			encode-types
			encode-value




























## SC Sequencing


### Events

- "Spawning Events", P.319 Cottle
	
	Client creates events and determines when they occur.
	"The language side of the program manages when the events occur, how many there are, and what attributes they have."


### making keyboard play notes

- use Envelope and key-state (see P. 103 SC3 tutorial, Cottle.pdf)


* Envelopes. From SC3 tut.pdf, page 101:
"SC uses EnvGen and Env to create envelopes. Env describes the shape of the envelope and EnvGen generates that shape when supplied a trigger or gate."


## Find out in SC and rsc3:

- how to wire synths with effects using the global bus.


- see notes in 445 graph notebook


example: (p. 186 SC3 Cottle)
	// route LFNoise0 to kr bus 20 (actually, 20 and 21 since it's stereo)
	// SinOSC reads from input bus 20 and 21 (since 2nd arg is 2, gets a stereo signal.)
	{Out.kr(20, LFNoise0.kr([8, 11], 500, 1000))}.scope
	{Out.ar(0, SinOsc.ar(In.kr(20, 2), 0, 0.3))}.scope


- The order in which nodes are created matters if audio buses are linked.








