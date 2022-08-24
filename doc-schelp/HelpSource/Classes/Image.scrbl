#lang scribble/manual
@(require (for-label racket))

@title{Image}
 image component@section{categories}
  GUI>Views
@section{related}
  Classes/View

@section{description}


Image enables the drawing of images in the SuperCollider GUI.


@section{CLASSMETHODS}
 

@section{PRIVATE}
 initClass, prFromWindowRect, prFreeAll

@section{METHOD}
 new
Creates a new Image instance. "multiple" here stands for multiple arguments.

@section{ARGUMENT}
 multiple
Any of the following:
@section{list}
 
## link::Classes/Number:: to create an strong::empty:: image of size multiple as width and height

@racketblock[
i = Image.new(400);		// Create a 400x400 pixel Image.
i.dump;
i.free;

i = Image.new(400, 200);	// Create a 400x200 pixel Image.
i.dump;
i.free;
::
## link::Classes/Point:: to create an strong::empty:: image of size multiple.x as width and multiple.y as height
]

@racketblock[
i = Image.new(400@200);	// Create a 400x200 pixel Image.
i.dump;
i.free;
::
## link::Classes/String:: to create an image from a strong::local file::
]

@racketblock[
//	Path string
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png"); // add a path to your image
[i.width, i.height].postln;
i.plot;
i.free;
::
::

]
@section{ARGUMENT}
 height
If strong::multiple:: is a number, then this argument indicates the height of the new image.


@section{METHOD}
 color
Creates a new Image instance filled with the specified color.

@racketblock[
i = Image.color(400, 200, Color.blue(0.9,0.1));
i.plot(freeOnClose:true);
::

]
@section{ARGUMENT}
 ... args
Multiple arguments. the last argument should be a valid link::Classes/Color::

@section{METHOD}
 open
Creates a new Image instance from the local file at strong::path::.

@racketblock[
(
i = Image.open(SCDoc.helpSourceDir +/+ "images/Swamp.png");
i.plot(freeOnClose:true);
i.url.postln;
)
::

]
@section{METHOD}
 openURL
// NOT IMPLEMENTED YET
Creates a new Image instance from a valid image at the specified URL strong::path::.

@racketblock[
i = Image.openURL(SCDoc.helpSourceDir +/+ "images/Swamp.png");
i.url;
w = i.plot(freeOnClose:true);
::

]
@section{METHOD}
 fromImage
Creates a new Image instance from another Image.

@racketblock[
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
j = Image.fromImage(i);
i.dump;
j.dump;
[i, j].do(_.plot);
[i, j].do(_.free);
::

]
@section{METHOD}
 fromWindow
Creates a new Image from a portion of a Window. this can be used to capture either a window or a specific View.


@racketblock[
// WINDOW Example:
// First create a window and draw inside of it
(
	w = Window.new;
	w.front; // comment this to copy offscreen window
	w.view.background_(Color.white);
	w.drawHook = {
		Pen.translate(100, 100);
		10.do{
			// set the Color
			Pen.color = Color.blue(rrand(0.0, 1), rrand(0.0, 0.5));
			Pen.addWedge((100.rand)@(100.rand), rrand(10, 100), 2pi.rand, 2pi.rand);
			Pen.perform([\stroke, \fill].choose);
		}
	};
	w.refresh;
)

// then grab the window
(
	i = Image.fromWindow(w);
	w.close;
	i.plot(freeOnClose:true);
)

// VIEW Capture Example:
// First create a window and add some views inside of it
(
	w = Window.new.front;
	b = [10, 80].asSpec;
	c = NumberBox(w, Rect(20, 20, 60, 40));
	a = Slider(w, Rect(20, 80, 100, 40))
		.focusColor_(Color.red(alpha:0.2))
		.action_({
			c.value_(b.map(a.value).round(0.01))
	// round the float so it will fit in the NumberBox
			});
)

// then grab the window
(
	i = Image.fromWindow(w, a.bounds);
	w.close;
	i.plot(freeOnClose:true);
)
::

]
@section{ARGUMENT}
 window
the Window object.

@section{ARGUMENT}
 rect
optional. the constrained rect to capture inside the Window. By default, it is the window size.

@section{SUBSECTION}
 Class variables and attributes

@section{METHOD}
 formats
returns all the valid image formats as an link::Classes/Array::

@racketblock[
Image.formats;
::

]
@section{METHOD}
 compositingOperations
returns all the valid compositing operations you can use when drawing an Image as an link::Classes/Array::

@racketblock[
Image.compositingOperations;
::

]
@section{METHOD}
 interpolations
returns an link::Classes/Array:: of the different levels of interpolation you can specify when drawing an Image.

@racketblock[
Image.interpolations;
::

]
@section{METHOD}
 closeAllPlotWindows
close all the Image plot windows currently opened.

@section{INSTANCEMETHODS}
 

@section{PRIVATE}
 prLockFocus, prDrawAtPoint, prSync, prApplyFilters, prTileInRect, prUpdatePixelsInRect, prGetPixel, prInit, prSetInterpolation, prFree, prSetPixel, prLoadPixels, prSetBackground, prApplyKernel, prUpdatePixels, prWriteToFile, prUnlockFocus, prInitFromURL, prSetColor, prGetColor, prSetName, prGetInterpolation, prDrawInRect

@section{SUBSECTION}
 commons / general attributes

@section{METHOD}
 width
returns or set the width of the receiver

@section{METHOD}
 height
returns or set the height of the receiver

@section{METHOD}
 setSize
set the size of the receiver

@section{METHOD}
 bounds
returns the bounds of the receiver.

@section{METHOD}
 free
deallocate the receiver. this method is useful if you want to manage and reclaim yourself resources. otherwise you do not need to call this method since each object is automatically garbage collected.

@racketblock[
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
Image.all;
i.free;
Image.all;
::

]
@section{METHOD}
 scalesWhenResized
flag to tell or set if the receiver should update its bitmap representation to scale when a resize operation if performed

@racketblock[
(
	i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
	i.bounds.postln; // getting the dimensions
	w =i.plot;
)

// changing the size of an image
(
	i.scalesWhenResized_(true);
	i.setSize(400, 400 / (i.width / i.height));
	a =i.plot;
)
(
a.close; w.close; i.free;
)
::

]
@section{METHOD}
 url
returns or set the url of the receiver. Returning only if any where supplied at creation, otherwise returns nil. Setting may be used for different purpose but try to supply a valid one since it is used for archiving the image as an object.

@racketblock[
i = Image.new("http://www.google.com/intl/en_ALL/images/logo.gif");
i.url;
i.plot;
i.free;
::

]
@section{METHOD}
 interpolation
get or set the level of interpolation used when rendering the image - it has not effect when the Image is accelerated. see link::#*interpolations:: for a valid range of values.

@racketblock[
(
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
w = i.plot;
i.interpolation;			// get the image currrent interpolation mode
)

(
i.interpolation = 'none';		// experiment with interpolation modes
w.refresh;
)

(
i.interpolation = 'low';
w.refresh;
)

(
i.interpolation = 1;			// same as 'low'
w.refresh;
)

(
i.interpolation = 'high';
w.refresh;
)

(
i.interpolation = 'default';
w.refresh;
)

(
i.accelerated_(true);
i.interpolation = 'none'; // does not work on coreimage accelerated image
w.refresh;
)

i.free;
::

]
@section{SUBSECTION}
 saving and archiving

@section{METHOD}
 write
write the Image to a file.

@racketblock[
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
i.dump
i.write("~/Desktop/my_image.png".standardizePath);
i.free;

//	storeOn / asCompileString
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
i.url;
i.asCompileString;
i.writeArchive("~/Desktop/my_image.scd".standardizePath);

i.free;
i = nil;

Document.open("~/Desktop/my_image.scd".standardizePath);

i = Object.readArchive("~/Desktop/my_image.scd".standardizePath);
i.plot;
i.free;
::

]
@section{ARGUMENT}
 path
the location where to save it

@section{ARGUMENT}
 format
(optional) format to use. see Image.formats for supported formats. If nil, it will get the format depending on the path extension.

@section{ARGUMENT}
 quality
The quality factor must be in the range 0 to 100 or -1. Specify 0 to obtain small compressed files, 100 for large uncompressed files, and -1 (the default) to use the default settings.

@section{SUBSECTION}
 rendering

@section{METHOD}
 plot
plots the image in a Window.

@racketblock[
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
w = i.plot;
w.close;

w = i.plot(showInfo:false);
w.close;
i.free;

// other option - image will be automatically freed when closed
Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png").plot("Hello", freeOnClose:true);
::

]
@section{ARGUMENT}
 name
the title of the Window. may be nil.

@section{ARGUMENT}
 bounds
the bounds of the Window. may be nil.

@section{ARGUMENT}
 freeOnClose
flag to tell if the Window should free the Image when closed.

@section{ARGUMENT}
 background
additional background to apply to the Window. may be useful for artifacts due to alpha / compositing...

@section{ARGUMENT}
 showInfo
shows pixel coordinates while the mouse is over the image's plot window.

@section{METHOD}
 lockFocus
sets the receiver as the current graphic context. So you can use Pen to draw inside of it.

@section{METHOD}
 unlockFocus
restore the graphic context state. the receiver is not anymore the current graphic context.

@racketblock[
(
	j = Image.new(400,300);

	j.lockFocus;

		Pen.translate(100, 100);
		1000.do{
			// set the Color
			Pen.color = Color.green(rrand(0.0, 1), rrand(0.0, 0.5));
			Pen.addAnnularWedge(
				(100.rand)@(100.rand),
				rrand(10, 50),
				rrand(51, 100),
				2pi.rand,
				2pi.rand
			);
			Pen.perform([\stroke, \fill].choose);
		};

	j.unlockFocus;
)

j.plot;
j.write("~/Desktop/my_drawing.png"); // write the image
j.free;
::

]
@section{METHOD}
 draw
shortcut for drawing inside an image. equivalent to :
@section{list}
 
## receiver.lockFocus
## aFunction
## receiver.unlockFocus
::

@racketblock[
(
	j = Image.new(400,300);
	j.draw({ arg image;

		Pen.translate(100, 100);
		1000.do {
			// set the Color
			Pen.color = Color.green(rrand(0.0, 1), rrand(0.0, 0.5));
			Pen.addAnnularWedge(
				(100.rand)@(100.rand),
				rrand(10, 50),
				rrand(51, 100),
				2pi.rand,
				2pi.rand
			);
			Pen.perform([\stroke, \fill].choose);
		};
	}).plot(freeOnClose:true);
)

//	String drawing support on the image
//	drawStringAtPoint(string, point, font, color);
(
	j = Image.new(150,50);
	j.draw({ arg bounds;
		j.drawStringAtPoint("Hello, world!", 10@10, Font("Lucida Grande", 24), Color.black);
	});
)

j.plot;
j.write("~/Desktop/hello.png");
j.free;
::

]
@section{METHOD}
 drawStringAtPoint
renders *correctly* a String inside an Image :) 
@racketblock[// to fix to have a compliant interface::
]

@racketblock[
(
	var width, height, tgHeight, ratio, str, font, color, strb, targetWidth=400, shadowColor, run = true;
	shadowColor = Color.black;

	color = Color.gray(0.8);
	str = "I Love Pixels";
	font = Font("Monaco", 10);
	strb = str.bounds(font);
	width = strb.width;
	height = strb.height;
	ratio = height / width;
	i = Image(width@(height));
	i.draw({|bb|
		Pen.smoothing_(false);
		i.drawStringAtPoint(str, 0@0, font, color);
	});
	i.interpolation_(\none);
	tgHeight = targetWidth * ratio;
	w = Window.new("", Rect(400,400, 450, 150)).drawHook_({
		Pen.setShadow(2@2, 0.4, color:Color.red);
		i.drawInRect(Rect(5,5,targetWidth, tgHeight));
	});

	w.view.background_(Color.white);
	w.onClose_({run = false; i.free;});
	w.front;
)
::

]
@section{METHOD}
 drawAtPoint
render the image or a portion of it in the current graphic context.

@racketblock[
(
	var operation='sourceOver', fraction=1.0, i, w;

	i = Image.new(
	//	"http://supercollider.sourceforge.net/theme/sc01/icon.supercollider.gif"
	//	SCDoc.helpSourceDir +/+ "images/duck_alpha.png"
		SCDoc.helpSourceDir +/+ "images/Swamp.png"
	);

	w = Window.new("Image", Rect(120, 400, 360, 180)).front;

	Slider.new(w, Rect(10, 150, 150, 16))
		.value_(1.0)
		.action_({ arg sl;
			fraction = sl.value;
			w.refresh;
		});

	PopUpMenu.new(w, Rect(170, 150, 100, 16))
		.items_( Image.compositingOperations.collect({ arg i; i.asString }) )
		.value_(2)
		.action_({ arg pm;
			operation = Image.compositingOperations.at(pm.value);
			w.refresh;
		});

	w.onClose_({ i.free }); // free the image when the window is closed

	w.drawHook_({

		i.drawAtPoint(10@10, nil, operation, fraction);

	});
)
::

]
@section{ARGUMENT}
 point
the link::Classes/Point:: where to draw it

@section{ARGUMENT}
 fromRect
the portion of the Image to use

@section{ARGUMENT}
 operation
the compositing operation to use. 
@racketblock['sourceOver':: is the default.

]
@section{ARGUMENT}
 fraction
the opacity to use, ranging from 0.0 (fully transparent) to 1.0 (fully opaque)

@section{METHOD}
 drawInRect
render the image or a portion of it in a specified rectangle of the current graphic context. This may stretch the image depending on the destination rect.

@racketblock[
(
	i = Image.new(
		// "http://supercollider.sourceforge.net/theme/sc01/icon.supercollider.gif"
		SCDoc.helpSourceDir +/+ "images/icon.supercollider.png"
	);

	w = Window.new("Image", Rect(120, 400, 360, 180)).front;
	w.onClose_({ i.free }); // free the image when the window is closed
	w.drawHook_({
		i.drawInRect(Rect(10,10,50,50), Rect(10,10,50,50), 2, 1.0); // only a section
	});
)
::

]
@section{ARGUMENT}
 rect
the link::Classes/Rect:: where to draw it

@section{ARGUMENT}
 fromRect
the portion of the Image to use

@section{ARGUMENT}
 operation
the compositing operation to use. 
@racketblock['sourceOver':: is the default.

]
@section{ARGUMENT}
 fraction
the opacity to use, ranging from 0.0 (fully transparent) to 1.0 (fully opaque)

@section{METHOD}
 tileInRect
tile the image or a portion of it in a specified rectangle of the current graphic context. This may stretch the image depending on the destination rect.

@racketblock[
(
	i = Image.new(
		// "http://supercollider.sourceforge.net/theme/sc01/icon.supercollider.gif"
		SCDoc.helpSourceDir +/+ "images/icon.supercollider.png"
	);

	w = Window.new("Image", Rect(120, 400, 360, 180)).front;
	w.onClose_({ i.free }); // free the image when the window is closed
	w.drawHook_({
		i.tileInRect(w.view.bounds, nil, 2, 1.0); // all image contents
	});
)
::

]
@section{ARGUMENT}
 rect
the link::Classes/Rect:: where to draw it

@section{ARGUMENT}
 fromRect
the portion of the Image to use

@section{ARGUMENT}
 operation
the compositing operation to use. 
@racketblock['sourceOver':: is the default.
]
@section{NOTE}
 
Compositing operations are currently disabled for tileInRect
::

@section{ARGUMENT}
 opacity
the opacity to use, ranging from 0.0 (fully transparent) to 1.0 (fully opaque)

@section{SUBSECTION}
 Instance Methods / accessing and setting pixels

@section{METHOD}
 setPixel
fill a pixel located at x @ y.

@racketblock[
i = Image.color(60, 60, Color.blue(0.1,0.1));
w = i.plot;
i.setPixel([255,0,0,255].asRGBA, 0, 0); // setting red
w.refresh;
("pixel at 0 @ 0:"+i.getPixel(0,0).rgbaArray).postln;
i.free;
::

]
@section{ARGUMENT}
 rgbaInteger
an 32 bit link::Classes/Integer:: containing color information packed as 8bit RGBA

@section{METHOD}
 getPixel
retrieve the pixel value at x @ y as a RGBA integer

@racketblock[
// A simple example on how to manipulate pixels with Image
b = Int32Array[
	Integer.fromRGBA(255,0,0,255), // red
	Integer.fromRGBA(0,255,0,255), // green
	Integer.fromRGBA(0,0,255,255), // blue
	Integer.fromRGBA(255,0,255,255) // purple
];

b[0].red; // 255 see Integer.red
b[0].green; // 0 see Integer.green
b[0].blue; // 0 see Integer.blue
b[0].alpha; // 255 see Integer.alpha

a = Image.new(b.size@1).pixels_(b).interpolation_(\none);
a.plot;


// Set + Get
a.setPixel([255, 0, 255, 128].asRGBA /* create an Integer from 0-255 integer rgba value */, 0, 0).plot;
p = a.getPixel(0,0);

p.red; // 255
p.green; // 0
p.blue; // 255
p.alpha; // 128

// now another important example
a.setPixel([255, 0, 255, 0].asRGBA, 1, 0).plot; // clear color -> alpha is 0
p = a.getPixel(1,0);

p.red; // you expect 255 but you get 0 ??? Why = because Image uses premultiplied color component value internally
// meaning all Red, Green, and Blue component are premultiplied by the alpha
// if alpha is 0 you get 0 back for all components.

p.green; // 0
p.blue; // 0
p.alpha; // 0

p = a.getColor(1,0); // more explicit - but same here
::

]
@section{METHOD}
 setColor
fill the pixel located at x @ y with the specified strong::color::.

@section{METHOD}
 getColor
retrieve the pixel value at x @ y as a link::Classes/Color::.

@section{METHOD}
 pixels
retrieve or set all the pixels of the receiver.
@section{NOTE}
 
Careful: the returned Array is a link::Classes/Int32Array:: of size receiver.width * receiver.height containing all pixel values as 32bit Integer
::

@section{ARGUMENT}
 array
an link::Classes/Int32Array:: of size receiver.width * receiver.height containing all pixel values as 32bit Integer

@section{METHOD}
 loadPixels
load all the pixels of the receiver in an array. it is better and faster to call this function instead of link::#-pixels:: if you plan to retrieve frequently the pixel data (since it won't allocate a new array everytime !)

@racketblock[
// exec one line at a time
i = Image.new(
	// "http://supercollider.sourceforge.net/theme/sc01/icon.supercollider.gif"
	SCDoc.helpSourceDir +/+ "images/icon.supercollider.png"
);

// first grab the pixels
p = i.pixels;

// do some mods - here invert
i.invert;

// reload directly in my array - do not need to call i.pixels again
i.loadPixels(p);
i.free;
p;
::

]
@section{ARGUMENT}
 array
the array that will be filled. Should be an link::Classes/Int32Array:: of size receiver.width * receiver.height.

@section{ARGUMENT}
 region
the targeted rectangular region. (nil by default, meaning full size)

@section{ARGUMENT}
 start
the start index of the array.

@section{METHOD}
 setPixels
set the pixels in a specific portion of the receiver.

@racketblock[
(
	i = Image.new(20@20);
	i.pixels_(
		Int32Array.fill(i.width * i.height, {
			Integer.fromRGBA(255.rand,127.rand,255.rand,255)
		})
	);
	//i.interpolation_(\none); // uncomment to see the difference
	w = i.plot(freeOnClose:true);
	i.pixels.postln;
)

(
	i = Image.color(50@50, Color.white);
	i.setPixels(
		Int32Array.fill(20*20,{Integer.fromRGBA(255.rand, 127.rand, 255.rand, 255)}),
		Rect(10,10,20,20)
	);
	i.interpolation_(\none); // uncomment to see the difference
	w = i.plot(freeOnClose:true);
	i.pixels.postln;
)
::

]
@section{ARGUMENT}
 array
an link::Classes/Int32Array:: of size strong::rect::.width * strong::rect::.height containing all pixel values as 32bit Integer

@section{ARGUMENT}
 region
a rectangle defining the portion to update in the receiver. By default strong::rect:: is nil, meaning full image size.

@section{ARGUMENT}
 start
the array start index.

@section{SUBSECTION}
 Instance Methods / Attributes for ImageFilter support

see link::Classes/ImageFilter:: for more info

@section{METHOD}
 applyFilters
apply an array of link::Classes/ImageFilter:: to the image. this should be considered as an in place operation, meaning the Image is altered after it.

@racketblock[
// ******** Built In CoreImage Generators ********
// Generators are not filters, they actually create an image but do not need an input image
// you just have to create an image of a new size
// ** The Simple Random Filter **
(
f = ImageFilter.new(\CIRandomGenerator);
a = Image.new(500@500);
a.applyFilters(f);
w = a.plot(freeOnClose:true, background:Color.black);
a.bounds.postln;
)

// ** The StarShine example **
(
var width=500, height=500, centerVector;

centerVector = [ width*0.5, height*0.5 ];
a = Image.new(500@500);
f = ImageFilter.new(\CIStarShineGenerator);

f.center_(centerVector);
f.radius_(width*0.05);
f.color_(Color.blue);
f.crossWidth_(2.0);
f.crossAngle_(0.0);
f.crossOpacity_(-4.0);

a.applyFilters(f);
w = a.plot(freeOnClose:true, background:Color.gray); // change background to see
)


// ** Starshine + Pixellate + ZoomBlur **
(
var width=500, height=500, centerVector;

centerVector = [ width*0.5, height*0.5 ];
a = Image.new(500@500);

f = ImageFilter.new(\CIStarShineGenerator);
g = ImageFilter.new(\CIPixellate);
h = ImageFilter.new(\CIZoomBlur);

f.center_(centerVector);
f.radius_(width*0.05);
f.color_(Color.blue);
f.crossWidth_(2.0);
f.crossAngle_(0.0);
f.crossOpacity_(-4.0);
g.center_(centerVector);
h.center_(centerVector);
h.amount_(50);

a.applyFilters([f, g, h]);
w = a.plot(freeOnClose:true, background:Color.black);
)
::

]
@section{ARGUMENT}
 filters
a ImageFilter or an array of link::Classes/ImageFilter:: to be applied

@section{ARGUMENT}
 crop
the crop region to finally use. This may be required for extending bounds since some ImageFilter / CoreImageFilters require to set a wider region (to be applied correctly) or may create a huge image. Setting crop to nil sets no crop region. In case the current maximum size of a filtered Image is 4096 / 4096. Any larger size will be clipped. by default crop is constrained to the receiver bounds.

@section{ARGUMENT}
 region
@section{NOTE}
 
supported only in macOS 10.5 and later.
::
option to constrain the filter to a specific region IN the Image.

@section{METHOD}
 filteredWith
returns a new Image, copy of the receiver filtered with an array of ImageFilter. arguments are the same as link::#-applyFilters:: (except for strong::region::).
@section{NOTE}
 
Beware: you are responsible for freeing the newly created Image !!!
::

@section{METHOD}
 filters
filters is the instance variable that holds the array of ImageFilter attached to the receiver. This is a convenient for applying filters out place and changing the ImageFilter's attributes. see link::#-addFilter::, link::#-removeFilter::

see link::Classes/ImageFilter:: for an example on how to use the strong::filters:: array.

@section{METHOD}
 addFilter
you can also attach filters to the receiver for real-time changing operations. In this case the receiver will create a cache before each rendering to maintain his previous state, and allowing you to use filters without applying them in place. The cache is managed directly by the receiver. you can add several filters to the receiver, the first filter in the array is the first applied in the rendering chain.

see link::Classes/ImageFilter:: for an example on how to use strong::addFilter::.

@section{ARGUMENT}
 filter
a ImageFilter to apply before rendering of the image

@section{METHOD}
 removeFilter

see link::Classes/ImageFilter:: for an example on how to use strong::removeFilter::.

@section{ARGUMENT}
 filter
the ImageFilter to remove from the rendering chain.

@section{METHOD}
 flatten
if link::#-filters:: is not zero sized, this method will apply all those filters in place. if the image is accelerated this method force a bitmap representation of the receiver.

@section{METHOD}
 invert
invert the receiver

@racketblock[
(
i = Image.new();
i.invert;
i.plot(freeOnClose:true);
)
::

]
@section{METHOD}
 crop
crop the receiver

@racketblock[
(
i = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
i.crop( Rect(10,10,120,100) );
i.plot(freeOnClose:true);
)
::

]
@section{ARGUMENT}
 aRect
the cropping region

@section{SUBSECTION}
 Instance Methods / Attributes for ImageKernel support

see link::Classes/ImageKernel:: for examples and more info.

@section{METHOD}
 applyKernel
apply a Kernel in place. the receiver is modified after this call.

@section{ARGUMENT}
 kernel
a link::Classes/ImageKernel::

@section{EXAMPLES}
 

@section{SUBSECTION}
 Views addition

you can now use a Image as a valid view background. 16 drawing modes are defined to behave differently.

@section{definitionList}
 
## tileMode values: ||
@section{table}
 
## 1 || fixed to left, fixed to top
## 2 || horizontally tile, fixed to top
## 3 || fixed to right, fixed to top
## 4 || fixed to left, vertically tile
## 5 || horizontally tile, vertically tile
## 6 || fixed to right, vertically tile
## 7 || fixed to left, fixed to bottom
## 8 || horizontally tile, fixed to bottom
## 9 || fixed to right, fixed to bottom
## 10 || fit
## 11 || center, center (scale)
## 12 || center , fixed to top
## 13 || center , fixed to bottom
## 14 || fixed to left, center
## 15 || fixed to right, center
## 16 || center, center (no scale)
::

## View:backgroundImage_ ||

strong@section{image}
  - the Image to use

strong::tileMode:: - the mode to use. by default fixed to left, fixed to top

strong::alpha:: - opacity 0 < x < 1

strong::fromRect:: - the portion of the image to use. by default use the full image.
::

@racketblock[
(
	b = 1.0;
	a = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
	r = Rect(20,3,40,40);
	w = Window.new("Image background" /*, textured:false*/ );
	w.view.background_(Color.gray);
	w.view.backgroundImage_(a, 5, b, r);
	w.front;
)

// monte carlo :) exec every line to test
// r = nil; // uncomment for full image
w.view.backgroundImage_(a, 1, b, r);
w.view.backgroundImage_(a, 2, b, r);
w.view.backgroundImage_(a, 3, b, r);
w.view.backgroundImage_(a, 4, b, r);
w.view.backgroundImage_(a, 5, b, r);
w.view.backgroundImage_(a, 6, b, r);
w.view.backgroundImage_(a, 7, b, r);
w.view.backgroundImage_(a, 8, b, r);
w.view.backgroundImage_(a, 9, b, r);
w.view.backgroundImage_(a, 10, b, r);
w.view.backgroundImage_(a, 11, b, r); // find best ratio - move to see
w.view.backgroundImage_(a, 12, b, r);
w.view.backgroundImage_(a, 13, b, r);
w.view.backgroundImage_(a, 14, b, r);
w.view.backgroundImage_(a, 15, b, r);
w.view.backgroundImage_(a, 16, b, r);

// this is safe even if window is still open because Background object holds the Image
a.free;

w.close;

(
	a = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
	w = Window.new("Image background");
	l = Slider2D.new(w, Rect(10,10,200,200))
		.backgroundImage_( a, 5, 1, Rect(0,0,10,10) );
	w.front;
	a.free; // safe
)

(
	var bounds = Rect(10,10,150,18);
	a = Image.new(SCDoc.helpSourceDir +/+ "images/Swamp.png");
	w = Window.new("Image background");
	l = Slider.new(w, bounds)
		.backgroundImage_(a);
	w.front;
	a.free; // safe
)
::
]


