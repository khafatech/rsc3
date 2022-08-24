#lang scribble/manual
@(require (for-label racket))

@title{FileDialog}
 Operating system interface for Open file, save file, select directory dialogs@section{categories}
  GUI>Accessories
@section{related}
  Classes/Dialog, Classes/File

@section{description}

This is the interface for your standard operating system modal file dialogs to open files, save files and select directories.

See also Dialog which makes it a bit simpler without the need to set fileMode and acceptMode manually.

At the moment selecting a directory is only possible with FileDialog.

@section{CLASSMETHODS}
 

@section{METHOD}
  new
Open a modal dialog

@section{ARGUMENT}
  okFunc
handler function for: user selected a file and clicked ok.
Argument to the function is either a single path, an array of paths, or multiple paths passed in as separate arguments.
This depends on what fileMode and stripResult were specified.

@section{ARGUMENT}
  cancelFunc
handler function for: user cancelled

@section{ARGUMENT}
  fileMode
Integer

Determines the type of dialog.

@section{list}
 
## 0			QFileDialog AnyFile		     The name of a file, whether it exists or not.
## 1			QFileDialog ExistingFile	   The name of a single existing file.
## 2			QFileDialog Directory		   The name of a directory. Both files and directories are displayed.
## 3			QFileDialog ExistingFiles	 The names of zero or more existing files.
::

0 or 3 implies that the user can type in a new file name.

@section{ARGUMENT}
  acceptMode
Integer
This determines what the accept button says: "Open" or "Save"

@section{list}
 
## 0			QFileDialog AcceptOpen
## 1			QFileDialog AcceptSave
::

@section{ARGUMENT}
  stripResult
If selecting multiple files (using fileMode 3) then you have the choice of having your okFunc called with a list or with each of the paths as separate items.

If you want to select only one item then use stripResult = true so that your okFunc will be passed the item instead of a list.

@section{list}
 
## true: okFunc(path1, path2, path3)
## false: okFunc(paths)
::

@section{returns}
  a FileDialog

@section{METHOD}
  qtClass
private


@section{EXAMPLES}
 


@racketblock[
FileDialog({ |path|

}, {

});
::
]


