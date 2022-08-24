#lang scribble/manual
@(require (for-label racket))

@title{GridLines}
 given a spec and the actual data's min and max values, calculates the ideal spacing and labelling of grid lines for plotting@section{categories}
  GUI>Accessories
@section{related}
  Reference/plot, Classes/Plotter, Classes/DrawGrid

@section{description}

GridLines is a strategy object that implements a general strategy for finding a suitable min max range for graphing and suitable intervals for grid lines and labelling.

The object that does the actual drawing on a view is DrawGrid.

A GridLines object uses a ControlSpec to define the minimum and maximum possible values.  Given a data set's actual minimum and maximum values, the GridLines object can choose a logical range for graphing that encompasses the data that will be plotted.  

Future development work will add subclasses of GridLines that can bind more tightly with the data they are representing.  For instance a FreqGridLines (not yet implemented) could apply stronger lines to octave divisions.  A DegreeGridLines could be used to draw pitch degree gridlines behind a frequency plot.

Spec has a .grid variable that points to its preferred GridLines object that should be used for graphing.


@racketblock[
\freq.asSpec.grid
::

This default implementation does not know anything about the data is displaying:

]

@racketblock[
DrawGrid.test( nil, \midinote.asSpec.grid );
::

A MidinoteGrid could be written that labels these correctly, shows octaves and individual notes depending on the current zoom.

Note that the GridLines does not know which axis it is to be used on and could also be used in polar plots or in 3D rendering.

]
@section{CLASSMETHODS}
 

@section{METHOD}
  new

@section{argument}
  spec
The ControlSpec that defines the mininum and maximum values, warn and step.

@section{returns}
  a GridLines


@section{INSTANCEMETHODS}
 

@section{METHOD}
  spec
get/set the spec

@section{returns}
  a ControlSpec

@section{METHOD}
  asGrid
return self.  nil.asGrid would return a BlankGridLines which is a subclass of GridLines.  So when plotting if you specify a grid of nil then you will get no lines at all.

@section{returns}
  self

@section{METHOD}
  niceNum
Based on: http://books.google.de/books?id=fvA7zLEFWZgC&pg=PA61&lpg=PA61
This rounds a value to a logical nice number.  It is mostly used to support internal calculation, though it may be useful for other applications.

@section{argument}
  val
The value.

@section{argument}
  round
Boolean. Rounding uses a specific algorithm.  This is not simple rounding to an integer value.

@section{returns}
  the nice number

@section{METHOD}
  ideals
for internal use

@section{argument}
  min
(describe argument here)

@section{argument}
  max
(describe argument here)

@section{argument}
  ntick
(describe argument here)

@section{returns}
  (returnvalue)

@section{METHOD}
  looseRange
Returns the logical minimum and maximum that will contain the data.

@section{argument}
  min
minimum value

@section{argument}
  max
maximum value.

@section{argument}
  ntick
the number of lines you would like (which usually varies by how much screen space you have and what you consider cluttered)

@section{returns}
  [ideal min, ideal max]

@section{METHOD}
  getParams
Specifically for use by DrawGrid. This returns a dictionary filled with:
'lines': an array of values where lines should be drawn
'labels': [value, formatted label] for each line 

@section{argument}
  valueMin
minimum value of the data to be plotted

@section{argument}
  valueMax
maximum value of the data to be plotted

@section{argument}
  pixelMin
If numTicks is nil: used to guess the ideal numTicks based on the graph size.

@section{argument}
  pixelMax
If numTicks is nil: used to guess the ideal numTicks based on the graph size.

@section{argument}
  numTicks
Explicit number of ticks you would like to see on the graph.

@section{returns}
  A dictionary

@section{METHOD}
  formatLabel
Round the value and append the spec's units

@section{argument}
  val
The value

@section{argument}
  numDecimalPlaces
Number of decimal places

@section{returns}
  a string




