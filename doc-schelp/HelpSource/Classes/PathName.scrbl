#lang scribble/manual
@(require (for-label racket))

@title{PathName}
 file and directory path utilities@section{related}
  Classes/File, Classes/String
@section{categories}
  Files

@section{description}

PathName is a utility class for manipulating file names and paths. It expects a path to a file, and lets you access parts of that file path.

@section{ClassMethods}
 

@section{private}
 initClass

@section{method}
 new

@section{argument}
 path
a link::Classes/String:: which likely contains one or more / as typical for folder separation. ~ will be converted to your fully addressed home directory, as per link::Classes/String#-standardizePath::.

@racketblock[
PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
::

]
@section{method}
 tmp
Get or set the global temp directory as a link::Classes/String::. This is used by link::Classes/Buffer::, etc. By default this is "/tmp/" for Linux and macOS, and "/WINDOWS/TEMP/" for Windows.

@section{InstanceMethods}
 

@section{method}
 fileName
returns just the name of the file itself; i.e. everything after the last slash in the full path.

@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.fileName.postln;
)
::

]
@section{method}
  fileNameWithoutExtension
returns the name of the file itself without the file extension.

@section{method}
 extension
returns the file extension, i.e. everything after the last full-stop in the link::#-fileName::.

@section{method}
 pathOnly
returns the full path up to the file name itself; i.e. everything up to and including the last slash. This is handy e.g. for storing several files in the same folder.


@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.pathOnly.postln;
)
::

]
@section{method}
 isAbsolutePath, asAbsolutePath, isRelativePath, asRelativePath
you MUST have correctly initialized the scroot classvar for this to know what it is relative to !

@section{method}
 folderName
returns only the name of the folder that the file is in; i.e. everything in between the last but one and the last slash.

@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.folderName.postln;
)
::

]
@section{method}
 fullPath
returns the full path name that PathName contains.

@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.fullPath.postln;
)
::

]
@section{method}
 entries
returns a list of all the files+folders inside the folder represented by this path.

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.entries.postln;
)
::

]
@section{method}
 files
returns a list of all the files in the folder represented by this path.

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.files.postln;
)
::

]
@section{method}
 folders
returns a list of all the subfolders of the folder represented by this path.

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.folders.postln;
)
::

]
@section{method}
 isFile
returns a link::Classes/Boolean:: indicating whether or not the path represents a file (not a folder).

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.isFile.postln;
)
::

]
@section{method}
 isFolder
returns a link::Classes/Boolean:: indicating whether or not the path represents a folder (not a file).

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.isFolder.postln;
)
::

]
@section{method}
 filesDo
Iterates over all files found in the pathname, including ones in subfolders.

@racketblock[
(
var myPath;
myPath = PathName.new("./");
myPath.filesDo{|afile| afile.postln};
)
::

]
@section{method}
 allFolders
returns a list of all the folder names contained in the pathname itself.

@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.allFolders.postln;
)
::

]
@section{method}
 diskName
if path is an absolute path, returns the disk name; else a blank string.

@racketblock[
(
var myPath;
myPath = PathName.new("MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.diskName.postln;
)

( // note the / at the start
var myPath;
myPath = PathName.new("/MyDisk/SC 2.2.8 f/Sounds/FunkyChicken");
myPath.diskName.postln;
)
::

]
@section{method}
 +/+
Path concatenation operator - useful for avoiding doubling-up slashes unnecessarily.

@racketblock[
(PathName("/somewhere") +/+ PathName("over/the/rainbow")).postln;
(PathName("/somewhere") +/+ PathName("/over/the/rainbow")).postln;
::

]
@section{method}
 endNumber
returns a number at the end of PathName. Returns zero if there is no number.

@racketblock[
PathName("floating1").endNumber.postln;
PathName("floating").endNumber.postln;
::

]
@section{method}
 noEndNumbers
returns link::#-fullPath:: without any numbers at the end.

@racketblock[
PathName("floating1").noEndNumbers.postln;
PathName("floating").noEndNumbers.postln;
::

]
@section{method}
 nextName
generates a sensible next name for a file by incrementing a number at the end of PathName, or by adding one if there is none. This is useful for recording files with consecutive names, and e.g. to generate a new filename when you don't want to overwrite an existing file with the current name.

@racketblock[
PathName("floating34").nextName.postln;
PathName("floating").nextName.postln;
PathName("floating12_3A4X_56.7").nextName.postln;
::

]
@section{Examples}
 

Here is an example that uses many instance methods. Just pick any file to see all the parts of its path.


@racketblock[
(
GetFileDialog.new(
	{ arg ok, path;
	var myPathName;
	if (ok,
		{
			myPathName = PathName.new(path);

			"New PathName object:  ".postc;
			myPathName.postln;

			"fileName only:        ".postc;
			myPathName.fileName.postln;

			"path up to file only: ".postc;
			myPathName.pathOnly.postln;

			"folder Name:          ".postc;
			myPathName.folderName.postln;
		}
	)
	}
)
)
::

Choose a soundfile to put into the library, using its foldername and filename.

]

@racketblock[
(
GetFileDialog.new(
	{ arg ok, path;
	var myPathName, myFile;
	if (ok,
		{
			myPathName = PathName.new(path);

			// read your file from disk, e.g. a soundFile/

			myFile = SoundFile.new;
			if (myFile.openRead(path),
				{
					Library.put(
						[ myPathName.folderName.asSymbol, myPathName.fileName.asSymbol ],
						myFile);
					("Check Library.global" + myPathName.folderName + "please.").postln;
				},
				{ ("Could not read soundfile" + path ++ ".").postln; }
			);
			myFile.close;
		}
	)
	}
)
)
::

Save three tables in the same folder. Note: The file name chosen in the dialog is ignored! The files are always named table1, table2, table3.

]

@racketblock[
(
var table1, table2, table3;

table1 = Wavetable.sineFill(1024, [1,2,3]);
table2 = Signal.newClear.asWavetable;
table3 = Wavetable.sineFill(1024, Array.rand(64, 0.0, 1.0));

GetFileDialog.new(
	{ arg ok, path;
	var myPathName, myPathOnly;
	if (ok,
		{
			myPathName = PathName.new(path);
			myPathOnly = myPathName.pathOnly;
			("writing files tables1-3 to"+myPathOnly).postln;
			table1.write(myPathOnly ++ "table1");
			table2.write(myPathOnly ++ "table2");
			table3.write(myPathOnly ++ "table3");
		}
	)
	}
)
)
::
]


