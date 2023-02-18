#lang scribble/manual
@(require (for-label racket))

@title{QPalette}
 Set of colors used by the GUI@section{categories}
   GUI>Accessories


@section{description}


QPalette is a strong::set of colors:: that the GUI system uses to draw the views. The colors are organized into three strong::color groups:: (active, inactive and disabled) used according to the state of the views drawn, and each group containing one color assigned to each of the various strong::color roles:: (window, windowText, button, buttonText, etc.), used to draw distinct elements of the views. See link::Reference/palette_color_roles:: and link::Reference/palette_color_groups:: for details.

A palette can be assigned to the whole GUI using link::Classes/QtGUI#*palette::, or to a particular view using link::Classes/View#-palette::. Views will inherit a palette from their parent, and ultimately QtGUI, unless a palette is explicitly assigned to them. Moreover, when setting a palette on a view, it will be combined with the inherited one, overriding only those colors that have been explicitly set on the palette (see link::#-hasColor::). Hence, assigning a new and unmodified palette will reset all the colors to the inherited ones.

There are also two predefined palettes accessible using link::#*light:: and link::#*dark::. The light palette is assigned to QtGUI by default on startup. Should you wish to use a palette that matches the color scheme used natively on your platform, you can access such palette using link::#*system::.

If you wish to design your own palette, it is most convenient to use link::#*auto::, which will automatically derive a palette from only two colors, and then modify the details as you see fit.

Note that in Qt GUI most color-related methods of views (like link::Classes/Window#-background::, link::Classes/Slider#-knobColor::, etc.) actually modify the view's palette.



@section{CLASSMETHODS}
 

@section{METHOD}
  new
	Instantiates a new palette, equivalent to the global palette assigned to QtGUI. All colors are considered to not be set (see link::#-hasColor::).

@section{METHOD}
  auto
	Instantiates a new palette, with colors automatically derived from the given colors for 'button' and 'window' color roles. All colors are considered to be set (see link::#-hasColor::).

	@section{argument}
  buttonColor
		The Color assigned to the button role.
	@section{argument}
  windowColor
		The Color assigned to the window role.

@section{METHOD}
  light
	A predefined palette using light colors. All colors are considered to be set (see link::#-hasColor::).

@section{METHOD}
  dark
	A predefined palette using dark colors. All colors are considered to be set (see link::#-hasColor::).

@section{METHOD}
  system
	The native system palette. All colors are considered to be set (see link::#-hasColor::).


@section{INSTANCEMETHODS}
 


@section{PRIVATE}
  prInit
@section{PRIVATE}
  prAuto
@section{PRIVATE}
  prSystem
@section{PRIVATE}
  prColor
@section{PRIVATE}
  prSetColor
@section{PRIVATE}
  prHasColor

@section{METHOD}
  color
	Returns the color assigned to a color role within a color group.
	@section{argument}
  role
		A symbol among link::Reference/palette_color_roles::.
	@section{argument}
  group
		A symbol among link::Reference/palette_color_groups::, or nil, in which case the current color group is used.

@section{METHOD}
  setColor
	Assigns a color to a color role within a color group.

	@section{argument}
  color
		A Color.
	@section{argument}
  role
		A symbol among link::Reference/palette_color_roles::.
	@section{argument}
  group
		A symbol among link::Reference/palette_color_groups::, or nil, in which case the color will be assigned to all groups.

@section{METHOD}
  hasColor
	Whether the color belonging to a color role and group has been set on this QPalette instance.

	When setting a palette on a view, only colors for which this methods returns true will be changed, others will be inherited from the parent view (or QtGUI if this view has no parent).

	@section{argument}
  role
		A symbol among link::Reference/palette_color_roles::.
	@section{argument}
  group
		A symbol among link::Reference/palette_color_groups::.

@section{METHOD}
  window
	Convenience method to get or set the color for the 'window' role.

@section{METHOD}
  windowText
	Convenience method to get or set the color for the 'windowText' role.

@section{METHOD}
  button
	Convenience method to get or set the color for the 'button' role.

@section{METHOD}
  buttonText
	Convenience method to get or set the color for the 'buttonText' role.

@section{METHOD}
  base
	Convenience method to get or set the color for the 'base' role.

@section{METHOD}
  baseText
	Convenience method to get or set the color for the 'baseText' role.

@section{METHOD}
  highlight
	Convenience method to get or set the color for the 'highlight' role.

@section{METHOD}
  highlightText
	Convenience method to get or set the color for the 'highlightText' role.


