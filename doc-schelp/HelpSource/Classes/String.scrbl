#lang scribble/manual
@(require (for-label racket))

@title{String}
array of Chars@section{related}
 Classes/Char
@section{categories}
  Collections>Ordered

@section{description}

String represents an array of link::Classes/Char##Chars::.

Strings can be written literally using double quotes:

@racketblock[
"my string".class
::
A sequence of string literals will be concatenated together:
]

@racketblock[
x = "hel" "lo";
y = "this is a\n"
    "multiline\n"
    "string";
::

Backslash is the escape character. See link::Reference/Literals#Characters::.

]
@section{subsection}
  Character encodings

Note that, while Char does not support encodings aside from ASCII—such as
multi-byte encodings like UTF-8 and UTF-16, or the full Latin-1 (ISO 8859-1)
character set—Chars with negative values are perfectly legal, and may be strung
together in strings that use these encodings.

The SuperCollider IDE uses UTF-8 (a superset of ASCII) to decode and display
strings, which means that the string 
@racketblock["🎹🙄🎻😂🎚️🎛️🎤😍":: can be written in
the editor, posted in the post window, and treated for the most part like any
other string.
However, because non-ASCII UTF-8 characters consist of two or more bytes,
and a SuperCollider String's members are one-bit Chars, concepts of size and
indexing may not behave intuitively.
For instance, the "]

@racketblock[size::" of the string above is 38, not 8, and the value
of its first index is ]

@racketblock[-16::, which is not a valid ASCII value at all but
rather the signed 8-bit representation of the first byte of the UTF-8 value of
the piano keyboard emoji (🎹), ]

@racketblock[0xF09F8EB9::.

]
@section{CLASSMETHODS}
 

@section{private}
 initClass
@section{private}
 doUnixCmdAction
@section{private}
 unixCmdActions

@section{method}
 readNew
Read the entire contents of a link::Classes/File:: and return them as a new String.

@section{method}
 scDir
Provided for backwards compatibility.
@section{returns}
 
the value of 
@racketblock[Platform.resourceDir::, which is the base resource directory of SuperCollider.
]
@section{discussion}
 
Please use link::Classes/Platform#*resourceDir:: instead.

@section{INSTANCEMETHODS}
 

@section{private}
 prUnixCmd, prFormat, prCat, prBounds, hash, species, getSCDir, prDrawInRect, prDrawAtPoint, openTextFile

@section{subsection}
  Accessing characters

@section{method}
 @, at
Strings respond to .at in a manner similar to other indexed collections. Each element is a link::Classes/Char::.

@racketblock[
"ABCDEFG".at(2)
::

]
@section{method}
 ascii
Returns an Array of ASCII values of the Strings's characters.

@racketblock[
"wertvoll".ascii // [ 119, 101, 114, 116, 118, 111, 108, 108 ]
::

Note that if a string contains multi-byte UTF-8 characters, this array will not
be of the same length as the number of visible characters, nor will it
necessarily be an array of valid 7-bit ASCII values.

]

@racketblock[
// "face with tears of joy" is Unicode codepoint U+1F602, which is encoded in UTF-8 as hex value 0xF09F9882
a = "😂";

// although this is one UTF-8 character, it must be stored as 4 Chars because Chars can only hold 1 byte each
a.size // 4 (!)
a.ascii // [ -16, -97, -104, -126 ]

// "wrap(0, 255)" converts these numbers to their unsigned 8-bit values
b = a.ascii.wrap(0, 255) // [ 240, 159, 152, 130 ]

// if we represent these values in hexidecmial, it's the same as the UTF-8 above: 0xF09F9882
b.collect(_.asHexString(2)) // [ F0, 9F, 98, 82 ]
::

]
@section{subsection}
  Comparing strings

@section{method}
 compare
Returns a -1, 0, or 1 depending on whether the receiver should be sorted before the argument, is equal to the argument or should be sorted after the argument. This is a case sensitive compare.

@section{method}
 <
Returns a link::Classes/Boolean:: whether the receiver should be sorted before the argument.

@racketblock[
"same" < "samf"
::

]
@section{method}
 >
Returns a link::Classes/Boolean:: whether the receiver should be sorted after the argument.

@racketblock[
"same" > "samf"
::
]
@section{method}
 <=
Returns a link::Classes/Boolean:: whether the receiver should be sorted before the argument, including the same string.

@racketblock[
"same" <= "same"
"same" <= "samf"
::

]
@section{method}
 >=
Returns a link::Classes/Boolean:: whether the receiver should be sorted after the argument, including the same string.

@racketblock[
"same" >= "same"
"same" >= "samf"
::

]
@section{method}
 ==
Returns a link::Classes/Boolean:: whether the two Strings are equal.
@section{note}
 
This method is (now) case sensitive!
::

@racketblock[
"same" == "same"
"same" == "Same"; // false
::

]
@section{method}
 !=
Returns a link::Classes/Boolean:: whether the two Strings are not equal.

@racketblock[
"same" != "same"; // false
"same" != "Same";
::

]
@section{subsection}
  Posting strings

@section{method}
 post
Prints the string to the current post window.

@racketblock[
"One".post; "Two".post;"";
::

]
@section{method}
 postln
Prints the string and a carriage return to the current post window.

@racketblock[
"One".postln; "Two".postln;"";
::

]
@section{method}
 postc, postcln
As link::#-post:: and link::#-postln::, but formatted as a comment.

@racketblock[
"This is a comment.".postcln;
::

]
@section{method}
 postf
Prints a formatted string with arguments to the current post window. The % character in the format string is replaced by a string representation of an argument. To print a % character use \\% .

@racketblock[
postf("this % a %. pi = %, list = %\n", "is", "test", pi.round(1e-4), (1..4))

this is a test. pi = 3.1416, list = [ 1, 2, 3, 4 ]
::

]
@section{method}
 postcs
As link::#-postln::, but posts the link::#-asCompileString#compileString:: of the receiver.

@racketblock[
List[1, 2, ["comment", [3, 2]], { 1.0.rand }].postcs;
::

]
@section{method}
 error
Prepends an error banner and posts the string.

@racketblock[
"Do not press this button again".error;
::

]
@section{method}
 warn
Prepends a warning banner and posts the string.

@racketblock[
"Do not press this button again".warn;
::

]
@section{method}
 inform
Legacy method (although due to widespread use, it will not be removed). This is identical to 
@racketblock[postln::.

]
@section{subsection}
  Interpreting strings as code

@section{method}
 compile
Compiles a String containing legal SuperCollider code and returns a Function.

@racketblock[
(
var f;
f = "2 + 1".compile.postln;
f.value.postln;
)
::

]
@section{method}
 interpret
Compile and execute a String containing legal SuperCollider code, returning the result.

@racketblock[
"2 + 1".interpret.postln;
::

]
@section{method}
 interpretPrint
Compile, execute and print the result of a String containing legal SuperCollider code.

@racketblock[
"2 + 1".interpretPrint;
::

]
@section{subsection}
  Converting strings

@section{method}
 asCompileString
Returns a String formatted for compiling.

@racketblock[
(
var f;
f = "myString";
f.postln;
f.asCompileString.postln;
)
::

]
@section{method}
 asSymbol
Return a link::Classes/Symbol:: derived from the String.

@racketblock[
(
var z;
z = "myString".asSymbol.postln;
z.class.postln;
)
::

]
@section{method}
 asInteger
Return an link::Classes/Integer:: derived from the String. Strings beginning with non-numeric characters return 0.

@racketblock[
"4".asInteger.postln;
::

]
@section{method}
 asFloat
Return a link::Classes/Float:: derived from the String. Strings beginning with non-numeric characters return 0.

@racketblock[
"4.3".asFloat.postln;
::

]
@section{method}
 asSecs
Return a link::Classes/Float:: based on converting a time string in format (dd):hh:mm:ss.s. This is the inverse method to link::Classes/SimpleNumber#-asTimeString::.

@racketblock[
(45296.asTimeString).asSecs;
"32.1".asSecs;
"62.1".asSecs;		// warns
"0:0:59.9".asSecs;
"1:1:1.1".asSecs;
"-1".asSecs;		// neg sign supported
"-12:34:56".asSecs;
"12:-34:56".asSecs;	// warns
"-23:12.3456".asSecs;	//
"-1:00:00:00".asSecs;	// days too.
::

]
@section{subsection}
  Concatenate strings

@section{method}
 ++
Return a concatenation of the two strings.

@racketblock[
"hello" ++ "word"
::

]
@section{method}
 +
Return a concatenation of the two strings with a space between them.

@racketblock[
"hello" + "word"
::

]
@section{method}
 +/+
Path concatenation operator - useful for avoiding doubling-up slashes unnecessarily.

@racketblock["foo"+/+"bar":: returns ]

@racketblock["foo/bar"::

]
@section{method}
 catArgs
Concatenate this string with the following args.

@racketblock[
"These are some args: ".catArgs(\fish, SinOsc.ar, {4 + 3}).postln;
::

]
@section{method}
 scatArgs
Same as link::#-catArgs::, but with spaces in between.

@racketblock[
"These are some args: ".scatArgs(\fish, SinOsc.ar, {4 + 3}).postln;
::

]
@section{method}
 ccatArgs
Same as link::#-catArgs::, but with commas in between.

@racketblock[
"a String".ccatArgs(\fish, SinOsc.ar, {4 + 3}).postln;
::

]
@section{method}
 catList, scatList, ccatList
As link::#-catArgs::, link::#-scatArgs:: and link::#-ccatArgs:: above, but takes a Collection (usually a link::Classes/@section{List}
  or an link::Classes/Array::) as an argument.

@racketblock[
"a String".ccatList([\fish, SinOsc.ar, {4 + 3}]).postln;
::



]
@section{subsection}
  Regular expressions

Note the inversion of the arguments:

@section{List}
 
## 
@racketblock[regexp.matchRegexp(stringToSearch)::
## ]

@racketblock[stringToSearch.findRegexp(regexp):: (and similar for ]

@racketblock[findAllRegexp:: and ]

@racketblock[findRegexpAt::).
::

]

@racketblock[findRegexp:: follows the pattern established by link::Classes/String#-find::, where the receiver is the string to be searched. ]

@racketblock[matchRegexp:: follows the pattern of link::Reference/matchItem::, where the receiver is the pattern to match and the first argument is the object to be tested. This is a common source of confusion, but it is based on this precedent.

]
@section{method}
 matchRegexp
POSIX regular expression matching. Returns true if the receiver (a regular expression pattern) matches the string passed to it. The strong::start:: is an offset where to start searching in the string (default: 0), strong::end:: where to stop.

@section{note}
 This is 
@racketblock[regexp.matchRegexp(stringToSearch):: and not the other way around! See above: link::Classes/String#Regular expressions::.::

]

@racketblock[
"c".matchRegexp("abcdefg", 2, 5); // true: substring exists
"c".matchRegexp("abcdefg", 4, 5); // false: substring doesn't exist

"behaviou?r".matchRegexp("behavior"); // true: character may or may not exist
"behaviou?r".matchRegexp("behaviour"); // true: character may or may not exist
"behaviou?r".matchRegexp("behavir"); // false: but the rest does not match
"behavi(ou)?r".matchRegexp("behavir"); // true: the substring in parens may or may not exist
"b.h.v.r".matchRegexp("behavor"); // true
"b.h.v.r".matchRegexp("behaviiiiir"); // false: dot stands for exactly one char
"b.h.vi*r".matchRegexp("behaviiiiir"); // true: (kleene) star stands for any number of chars preceding, or none
"b.h.vi*r".matchRegexp("behavuuuur"); // false
"(a|u)nd".matchRegexp("und"); // true
"(a|u)nd".matchRegexp("and"); // true
"[a-c]nd".matchRegexp("ind"); // false
"[a-c]nd".matchRegexp("bnd"); // true: anything between a and c
"[a-c]*nd".matchRegexp("accacaccacand"); //  true: any combination of x, t, z, or none.
"[xtz]+nd".matchRegexp("xnd"); // true: any combination of x, t, z
::

]
@section{method}
 findRegexp
POSIX regular expression search.

@racketblock[
"foobar".findRegexp("o*bar");
"32424 334 /**aaaaaa*/".findRegexp("/\\*\\*a*\\*/");
"foobar".findRegexp("(o*)(bar)");
"aaaabaaa".findAllRegexp("a+");
::

]
@section{method}
 findAllRegexp
Like link::#-findAll::, but use regular expressions. So unlike findRegexp, it will just return the indices of the


@racketblock[
"foobar".findAllRegexp("o*bar");
"32424 334 /**aaaaaa*/".findAllRegexp("/\\*\\*a*\\*/");
"foobar".findAllRegexp("(o*)(bar)");
"aaaabaaa".findAllRegexp("a+");
::

]
@section{method}
 findRegexpAt
Match a regular expression at the given offset, returning the match and the length of the match in an Array, or nil if it doesn't match.
The match must begin right at the offset.


@racketblock[
"foobaroob".findRegexpAt("o*b+", 0); // nil
"foobaroob".findRegexpAt("o*b+", 1); // [ oob, 3 ]
"foobaroob".findRegexpAt("o*b+", 2); // [ ob,  2 ]
"foobaroob".findRegexpAt("o*b+", 3); // [ b,   1 ]
"foobaroob".findRegexpAt("o*b+", 4); // nil
"foobaroob".findRegexpAt("o*b+", 5); // nil
"foobaroob".findRegexpAt("o*b+", 6); // [ oob, 3 ]
"foobaroob".findRegexpAt("o*b+", 7); // [ ob,  2 ]
::

]
@section{subsection}
  Searching strings

@section{method}
 find
Returns the index of the string in the receiver, or nil if not found. If strong::ignoreCase:: is true, find makes no difference between uppercase and lowercase letters. The strong::offset:: is the point in the string where the search begins. string may be a String or a link::Classes/Char::.

@racketblock[
"These are several words".find("are").postln;
"These are several words".find("fish").postln;
::

]
@section{method}
 findBackwards
Same like link::#-find::, but starts at the end of the string.

@racketblock[
// compare:
"These words are several words".find("words"); // 6
"These words are several words".findBackwards("words"); // 24
::

]
@section{method}
 findAll
Returns the indices of the string in the receiver, or nil if not found.

@racketblock[
"These are several words which are fish".findAll("are").postln;
"These are several words which are fish".findAll("fish").postln;
::

]
@section{method}
 contains
Returns a link::Classes/Boolean:: indicating if the String contains string.

@racketblock[
"These are several words".contains("are").postln;
"These are several words".contains("fish").postln;
::

]
@section{method}
 containsi
Same as link::#-contains::, but case insensitive.

@racketblock[
"These are several words".containsi("ArE").postln;
::

]
@section{method}
 containsStringAt
Returns a link::Classes/Boolean:: indicating if the String contains string beginning at the specified index.

@racketblock[
"These are several words".containsStringAt(6, "are").postln;
::

]
@section{method}
 icontainsStringAt
Same as link::#-containsStringAt::, but case insensitive.

@section{method}
 beginsWith
@section{method}
 endsWith
Returns true if this string begins/ends with the specified other string.
@section{argument}
  string
The other string
@section{returns}
 
A link::Classes/Boolean::

@section{subsection}
  Manipulating strings

@section{method}
 rotate
Rotate the string by n steps.

@racketblock[
"hello word".rotate(1)
::

]
@section{method}
 scramble
Randomize the order of characters in the string.

@racketblock[
"hello word".scramble
::


]
@section{method}
 replace
Like link::#-tr::, but with Strings as well as Chars as arguments.

@racketblock[
"Here are several words which are fish".replace("are", "were");
::

]
@section{method}
 format
Returns a formatted string with arguments. The % character in the format string is replaced by a string representation of an argument. To print a % character use \\% .

@racketblock[
format("this % a %. pi = %, list = %\n", "is", "test", pi.round(1e-4), (1..4))

this is a test. pi = 3.1416, list = [ 1, 2, 3, 4 ]
::

]
@section{method}
 escapeChar
Add the escape character (\) before any character of your choice.

@racketblock[
// escape spaces:
"This will become a Unix friendly string".escapeChar($ ).postln;
::

]
@section{method}
 quote
Return this string enclosed in double-quote ( teletype::":: ) characters.

@racketblock[
"tell your" + "friends".quote + "not to tread onto the lawn"
::

]
@section{method}
 zeroPad
Return this string enclosed in space characters.

@racketblock[
"spaces".zeroPad.postcs;
::

]
@section{method}
 underlined
Return this string followed by dashes in the next line ( teletype::-:: ).

@racketblock[
"underlined".underlined;
"underlined".underlined($~);
::

]
@section{method}
 tr
Transliteration. Replace all instances of strong::from:: with strong::to::.

@racketblock[
":-(:-(:-(".tr($(, $)); //turn the frowns upside down
::


]
@section{method}
 padLeft
@section{method}
 padRight
Pad this string with strong::string:: so it fills strong::size:: character.
@section{argument}
  size
Number of characters to fill
@section{argument}
  string
Padding string

@racketblock[
"this sentence has thirty-nine letters".padRight(39, "-+");
"this sentence has thirty-nine letters".padLeft(39, "-+");
"this sentence more than thirteen letters".padRight(13, "-+"); // nothing to pad.
::

]
@section{method}
 toUpper
Return this string with uppercase letters.

@racketblock[
"Please, don't be impolite".toUpper;
::

]
@section{method}
 toLower
Return this string with lowercase letters.

@racketblock[
"SINOSC".toLower;
::

]
@section{method}
 stripRTF
Returns a new String with all RTF formatting removed.

@racketblock[
(
// same as File-readAllStringRTF
g = File("/code/SuperCollider3/build/Help/UGens/Chaos/HenonC.help.rtf","r");
g.readAllString.stripRTF.postln;
g.close;
)
::

]
@section{method}
 split
Returns an Array of Strings split at the separator. The separator is a link::Classes/Char::, and is strong::not:: included in the output array.

@racketblock[
"These are several words".split($ );

// The default separator $/ is handy for Unix paths.
"This/could/be/a/Unix/path".split;
::

]
@section{subsection}
  Stream support

@section{method}
 printOn
Print the String on stream.

@racketblock[
"Print this on Post".printOn(Post);

// equivalent to:
Post << "Print this on Post";
::

]
@section{method}
 storeOn
Same as link::#-printOn::, but formatted link::#-asCompileString::.

@racketblock[
"Store this on Post".storeOn(Post);

// equivalent to:
Post <<< "Store this on Post";
::



]
@section{subsection}
 Unix Support

Where relevant, the current working directory is the same as the location of the SuperCollider app and the shell is the Bourne shell (sh). Note that the cwd, and indeed the shell itself, does not persist:

@racketblock[
"echo $0".unixCmd; // print the shell (sh)
"pwd".unixCmd;
"cd Help/".unixCmd;
"pwd".unixCmd;

"export FISH=mackerel".unixCmd;
"echo $FISH".unixCmd;
::
It is however possible to execute complex commands:
]

@racketblock[
"pwd; cd Help/; pwd".unixCmd;
"export FISH=mackerel; echo $FISH".unixCmd;
::
Also on os x applescript can be called via osascript:
]

@racketblock[
"osascript -e 'tell application \"Safari\" to activate'".unixCmd;
::
Should you need an environment variable to persist you can use link::#-setenv::.

]
@section{note}
 
Despite the fact that the method is called 'unixCmd', strong::it does work in Windows::. The string must be a DOS command, however: "dir" rather than "ls" for instance.
::

@section{method}
 unixCmd
Execute a UNIX command strong::asynchronously:: using the standard shell (sh).
@section{argument}
  action
A link::Classes/Function:: that is called when the process has exited. It is passed two arguments: the exit code and pid of the exited process.
@section{argument}
  postOutput
A link::Classes/Boolean:: that controls whether or not the output of the process is displayed in the post window.
@section{returns}
 
An link::Classes/Integer:: - the pid of the newly created process. Use link::Classes/Integer#-pidRunning:: to test if a process is alive.
@section{discussion}
 
Example:

@racketblock[
"ls Help".unixCmd;
"echo one; sleep 1; echo two; sleep 1".unixCmd { |res, pid| [\done, res, pid].postln };
::

]
@section{method}
 unixCmdGetStdOut
Similar to link::#-unixCmd:: except that the stdout of the process is returned (strong::synchronously::) rather than sent to the post window.

@racketblock[
~listing = "ls Help".unixCmdGetStdOut; // Grab
~listing.reverse.as(Array).stutter.join.postln; // Mangle
::

]
@section{method}
 systemCmd
Executes a UNIX command strong::synchronously:: using the standard shell (sh).

@section{returns}
  Error code of the UNIX command

@section{method}
 runInTerminal
Execute the String in a new terminal window (strong::asynchronously::).
@section{argument}
 shell
The shell used to execute the string.
@section{discussion}
 
@section{note}
  On macOS, the string is incorporated into a temporary script file and executed using the shell. ::
Example:

@racketblock[
"echo ---------Hello delightful SuperCollider user----------".runInTerminal;
::

]
@section{method}
 setenv
Set the environment variable indicated in the string to equal the String strong::value::. This value will persist until it is changed or SC is quit. Note that if strong::value:: is a path you may need to call link::#-standardizePath:: on it.

@racketblock[
// all defs in this directory will be loaded when a local server boots
"SC_SYNTHDEF_PATH".setenv("~/scwork/".standardizePath);
"echo $SC_SYNTHDEF_PATH".unixCmd;
::

]
@section{method}
 getenv
Returns the value contained in the environment variable indicated by the String.

@racketblock[
"USER".getenv;
::

]
@section{method}
 unsetenv
Set the environment variable to nil.

@section{method}
 mkdir
Make a directory from the given path location.

@section{method}
 pathMatch
Returns an link::Classes/Array:: containing all paths matching this String. Wildcards apply, non-recursive.

@racketblock[
Post << "Help/*".pathMatch;
::

]
@section{method}
 load
Load and execute the file at the path represented by the receiver.

@section{method}
 loadPaths
Perform link::#-pathMatch:: on this String, then load and execute all paths in the resultant link::Classes/Array::.



@racketblock[
//first prepare a file with some code...
(
File.use("/tmp/loadPaths_example.scd", "w", { |file|
	file << "\"This text is the result of a postln command which was loaded and executed by loadPaths\".postln;";
	file <<	"\"I will now throw a dice for you: \".post; 7.rand;"
})
)

// then load the file...
 // ... it posts some text, and the return value pf loadPaths is an array of the return values of each file
"/tmp/loadPaths_example.scd".loadPaths;
::

]
@section{argument}
 warn
Post a warning if path doesn't point to any file.

@section{argument}
 action
If a function is passed, it is called with each path as argument.



@section{method}
 loadRelative
Load and execute the file at the path represented by the receiver, interpreting the path as relative to the current document or text file. Requires that the file has been saved. This can be used e.g. to load initialization code from files in the same folder.

@section{argument}
 warn
Warn if a file is not found.

@section{argument}
 action
A function that is called for each file path that is found.

@section{method}
 resolveRelative
Convert the receiver from a relative path to an absolute path, relative to the current document or text file. Requires that the current text file has been saved. Absolute paths are left untransformed.

@section{method}
 standardizePath
Expand ~ to your home directory, and resolve aliases on macOS. See link::Classes/PathName:: for more complex needs. See link::Classes/File#*realpath:: if you want to resolve symlinks.

@racketblock[
"~/".standardizePath; //This will print your home directory
::


]
@section{method}
 openOS
Open file, directory or URL via the operating system. On macOS this is implemented via teletype::open::, on Linux via
teletype::xdg-open:: and on Windows via teletype::start::.

@racketblock[
Platform.userConfigDir.openOS;
"http://supercollider.sf.net".openOS;
::

]
@section{subsection}
 Pathname Support

Also see link::#-+/+:: for path concatenation.

@section{method}
 shellQuote
Return a new string suitable for use as a filename in a shell command, by enclosing it in single quotes ( teletype::':: ).
If the string contains any single quotes they will be escaped.
@section{discussion}
 
You should use this method on a path before embedding it in a string executed by link::#-unixCmd:: or link::#-systemCmd::.

@racketblock[
unixCmd("ls " + Platform.userExtensionDir.shellQuote)
::
]
@section{note}
 
This works well with shells such as strong::bash::, other shells might need different quotation/escaping.
Apart from usage in the construction of shell commands, strong::escaping is not needed:: for paths passed to methods like pathMatch(path) or File.open(path).
::

@section{method}
 absolutePath
@section{method}
 asAbsolutePath
Return this path as an absolute path by prefixing it with link::Classes/File#*getcwd:: if necessary.

@section{method}
 asRelativePath
Return this path as relative to the specified path.
@section{argument}
 relativeTo
The path to make this path relative to.

@section{method}
 withTrailingSlash
Return this string with a trailing slash if that was not already the case.

@section{method}
 withoutTrailingSlash
Return this string without a trailing slash if that was not already the case.

@section{method}
 basename
Return the filename from a Unix path.

@racketblock[
"Imaginary/Directory/fish.rtf".basename;
::

]
@section{method}
 dirname
Return the directory name from a Unix path.

@racketblock[
"Imaginary/Directory/fish.rtf".dirname;
::

]
@section{method}
 splitext
Split off the extension from a filename or path and return both in an link::Classes/Array:: as [path or filename, extension].

@racketblock[
"fish.rtf".splitext;
"Imaginary/Directory/fish.rtf".splitext;
::

]
@section{subsection}
 YAML and JSON parsing

@section{method}
 parseYAML
Parse this string as YAML/JSON.
@section{returns}
 
A nested structure of link::Classes/Array::s (for sequences), link::Classes/Dictionary##Dictionaries:: (for maps) and link::Classes/String::s (for scalars).

@section{method}
 parseYAMLFile
Same as 
@racketblock[parseYAML:: but parse a file directly instead of a string. This is faster than reading a file into a string and then parse it.

]
@section{subsection}
 Document Support

@section{method}
 newTextWindow
Create a new link::Classes/Document:: with this.

@racketblock[
"Here is a new Document".newTextWindow;
::

]
@section{method}
 openDocument
Create a new link::Classes/Document:: from the path corresponding to this. The selection arguments will preselect the indicated range in the new window. Returns this.

@racketblock[
(
String.filenameSymbol.asString.openDocument(10, 20)
)
::

]
@section{method}
 findHelpFile
Returns the path for the helpfile named this, if it exists, else returns nil.

@racketblock[
"Document".findHelpFile;
"foobar".findHelpFile;
::

]
@section{method}
 help
Performs link::#-findHelpFile:: on this, and opens the file it if it exists, otherwise opens the main helpfile.

@racketblock[
"Document".help;
"foobar".help;
::

]
@section{subsection}
 Misc methods

@section{method}
 speak
Deprecated. See link::Classes/Speech:: for the full reason and possible replacements. Sends string to the macOS speech synthesizer.

@racketblock[
"hi i'm talking with the default voice now, i guess".speak;
::

]
@section{method}
 inspectorClass
Returns class link::Classes/StringInspector::.


@section{subsection}
 Drawing Support

The following methods must be called within an Window-drawFunc or a SCUserView-drawFunc function, and will only be visible once the window or the view is refreshed. Each call to Window-refresh SCUserView-refresh will 'overwrite' all previous drawing by executing the currently defined function.

See also: link::Classes/Window::, link::Classes/UserView::, link::Classes/Color::, and link::Classes/Pen::.

@section{note}
 
for cross-platform GUIs, use 
@racketblock[Pen.stringAtPoint, Pen.stringInRect, Pen.stringCenteredIn, Pen.stringLeftJustIn, Pen.stringRightJustIn:: instead.
::

]
@section{method}
 draw
Draws the String at the current 0@0 link::Classes/Point::. If not transformations of the graphics state have taken place this will be the upper left corner of the window. See also link::Classes/Pen::.

@racketblock[
(
w = Window.new.front;
w.view.background_(Color.white);
w.drawFunc = {
	"abababababa\n\n\n".scramble.draw
};
w.refresh
)
::

]
@section{method}
 drawAtPoint
Draws the String at the given link::Classes/Point:: using the link::Classes/Font:: and link::Classes/Color:: specified.

@racketblock[
(
w = Window.new.front;
w.view.background_(Color.white);
w.drawFunc = {
	"abababababa\n\n\n".scramble.drawAtPoint(
		100@100,
		Font("Courier", 30),
		Color.blue(0.3, 0.5))
};
w.refresh;
)
::

]
@section{method}
 drawInRect
Draws the String into the given link::Classes/Rect:: using the link::Classes/Font:: and link::Classes/Color:: specified.

@racketblock[
(
w = Window.new.front;
r = Rect(100, 100, 100, 100);
w.view.background_(Color.white);
w.drawFunc = {
	"abababababa\n\n\n".scramble.drawInRect(r, Font("Courier", 12), Color.blue(0.3, 0.5));
	Pen.strokeRect(r);
};
w.refresh;
)
::

]
@section{method}
 drawCenteredIn
Draws the String into the given Rect using the Font and Color specified.

@racketblock[
(
w = Window.new.front;
w.view.background_(Color.white);
r = Rect(100, 100, 100, 100);
w.drawFunc = {
	"abababababa\n\n\n".scramble.drawCenteredIn(
		r,
		Font("Courier", 12),
		Color.blue(0.3, 0.5)
	);
	Pen.strokeRect(r);
};
w.refresh;
)
::

]
@section{method}
 drawLeftJustIn
Draws the String into the given Rect using the Font and Color specified.

@racketblock[
(
w = Window.new.front;
w.view.background_(Color.white);
r = Rect(100, 100, 100, 100);
w.drawFunc = {
	"abababababa\n\n\n".scramble.drawLeftJustIn(
		r,
		Font("Courier", 12),
		Color.blue(0.3, 0.5)
	);
	Pen.strokeRect(r);
};
w.refresh;
)
::

]
@section{method}
 drawRightJustIn
Draws the String into the given link::Classes/Rect:: using the link::Classes/Font:: and link::Classes/Color:: specified.

@racketblock[
(
w = Window.new.front;
w.view.background_(Color.white);
r = Rect(100, 100, 100, 100);
w.drawFunc = {
	"abababababa\n\n\n".scramble.drawRightJustIn(
		r,
		Font("Courier", 12),
		Color.blue(0.3, 0.5)
	);
	Pen.strokeRect(r);
};
w.refresh;
)
::

]
@section{method}
 bounds
Tries to return a link::Classes/Rect:: with the size needed to fit this string if drawn with given font.
@section{argument}
  font
A link::Classes/Font::



