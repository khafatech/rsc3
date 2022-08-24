#lang scribble/manual
@(require (for-label racket))

@title{Dialog}
 Shows various system dialogs@section{categories}
  GUI>Accessories

@section{description}

This class allows to show various system dialogs. link::#*openPanel:: will show a dialog for selecting a file to open, and link::#*savePanel:: will show a dialog for selecting or creating a file to save to.


@section{CLASSMETHODS}
 
@section{PRIVATE}
  key

@section{METHOD}
  openPanel
	Shows a dialog for selection of an existing file (or multiple files) to open. It does not do anything with the file, instead it just passes the chosen filenames to the given result handler.

	@section{ARGUMENT}
  okFunc
		An object to be evaluated when OK is pressed. As argument, either a single filename is passed as a String, or an Array of Strings for multiple selected items is passed, depending on the strong::multipleSelection:: argument. The paths will always be absolute paths.
	@section{ARGUMENT}
  cancelFunc
		An object to be evaluated when Cancel is pressed.
	@section{ARGUMENT}
  multipleSelection
		A Boolean indicating whether multiple files can be selected.
	@section{DISCUSSION}
 
	Example:

@racketblock[
(
Dialog.openPanel({ arg path;
	path.postln;
},{
	"cancelled".postln;
});
)
::

]
@section{METHOD}
  savePanel
	Shows a dialog for selecting or creating a file to save to. It does not do anything with the selected file, and does not create any file; instead it just passes the chosen filename to the given result handler.

	@section{ARGUMENT}
  okFunc
		An object to be evaluated when OK is pressed. The chosen filename (as an absolute path) is passed as a String as argument. If the file already exists, the user will be asked to confirm.
	@section{ARGUMENT}
  cancelFunc
		An object to be evaluated when Cancel is pressed.
	@section{DISCUSSION}
 
	Example:

@racketblock[
(
Dialog.savePanel({ arg path;
	path.postln;
},{
	"cancelled".postln;
});
)
::

]
@section{METHOD}
  getPaths
	@section{note}
 Deprecated. Use link::#*openPanel:: instead. ::

	Implements the same functionality as *openPanel, only the last argument is named differently and defaults to true.


