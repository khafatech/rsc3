#lang scribble/manual
@(require (for-label racket))

@title{QPenPrinter}
 QPen PDF export and printing of vector graphics@section{categories}
  GUI>Accessories
@section{related}
  Classes/Pen

@section{description}

QPenPrinter allows Pen to operate on a printer device. The graphics can be exported to PDF by using "print to file" as printer device.

@section{CLASSMETHODS}
 
@section{private}
  qtClass


@section{METHOD}
  new
Create a new QPenPrinter object.

@section{returns}
  an instance of QPenPrinter

@section{METHOD}
  print
Convenience function to show a print dialog and print.

@section{argument}
  printFunc
A link::Classes/Function:: to be evaluated when the user presses "Print", with the printer object as Pen painter target.
See strong::aPrintFunc:: in link::#-print:: below.

@section{argument}
  cancelFunc
An optional link::Classes/Function:: to be evaluated if the user presses "Cancel".


@section{INSTANCEMETHODS}
 
@section{private}
  init

@section{subsection}
  Printing

@section{METHOD}
  showDialog
Shows a Print Dialog to allow the user to configure the printer object. This is asynchronous and the method will return immediately.
When the user presses the "Print" button, strong::aOkFunc:: is called with this QPenPrinter object as argument.

@section{argument}
  aOkFunc
A link::Classes/Function:: to be evaluated when the user presses "Print".

@section{argument}
  aCancelFunc
An optional link::Classes/Function:: to be evaluated if the user presses "Cancel".


@section{METHOD}
  print
This method does the actual printing or PDF export. It evaluates strong::aPrintFunc:: with the printer object as Pen painter target. This QPenPrinter object is passed as the argument.

All the ordinary link::Classes/Pen:: commands can be used inside the function.

@section{argument}
  aPrintFunc
A link::Classes/Function:: to be evaluated to draw the graphics.

@section{discussion}
 
If this method is called without configuring the printer object first, it will print on the default printer with default settings.

This method is typically called from within the strong::aOkFunc:: of link::#-showDialog:: above. After showDialog has configured the printer once, this method can be called multiple times to reuse the last printer configuration.

The point at (0@0) will coincide with the origin of link::#-pageRect::, which is offset by the page margins. So you don't need to translate the Pen.


@section{METHOD}
  newPage
Starts a new page. Typically called within the strong::aPrintFunc:: of link::#-print::.


@section{subsection}
  Properties

@section{METHOD}
  paperRect
Get the paper bounds.

@section{returns}
  a link::Classes/Rect::


@section{METHOD}
  pageRect
Get the page bounds, which is the printable area and usually smaller than link::#-paperRect:: due to margins.

@section{returns}
  a link::Classes/Rect::

@section{discussion}
 
The strong::origin:: of the Rect is relative to the paper, and will be non-zero due to margins.


@section{METHOD}
  pageSize
Get the page size as a Size.

@section{returns}
  a link::Classes/Size::

@section{discussion}
 
This can be used to scale the graphics to fit the page if the bounds of the graphics is known:

@racketblock[
x = penPrinter.pageSize.width / bounds.width;
Pen.scale(x,x);
// ... draw stuff here ...
::

]
@section{subsection}
  Page range
The methods below returns the page range selected by the user. Page number starts at 1. When both methods returns 0 it means "print all pages".

@section{METHOD}
  fromPage
Get the start page.

@section{returns}
  an link::Classes/Integer::

@section{METHOD}
  toPage
Get the end page.

@section{returns}
  an link::Classes/Integer::



@section{EXAMPLES}
 

Simple usage:

@racketblock[
QPenPrinter.print {
    // first page
    Pen.moveTo(100@100);
    Pen.lineTo(300@300);
    Pen.stroke;

    // second page
    p.newPage;
    Pen.addRect(p.pageSize.asRect);
    Pen.stroke;
}
::

Keep the QPenPrinter object to save configuration state:
]

@racketblock[
p = QPenPrinter();
::
The code below can then be called multiple times:
]

@racketblock[
p.showDialog {
    p.print {
        // first page
        Pen.moveTo(100@100);
        Pen.lineTo(300@300);
        Pen.stroke;

        // second page
        p.newPage;
        Pen.addRect(p.pageSize.asRect);
        Pen.stroke;
    }
} {
    "Printing cancelled!".postln;
};
::

]


