

## By the beginning of Calpoly Fall 2014 

### Fix bugs

- Adding filters to a track has a bug. (forgot what it is exactly). Has to do with "currently playing notes" stopping when adding a filter. It's because I'm using the replace bus scsynth feature.

- [ ] make starting scsynth reliable
	- [ ] linux.
		- script not reliable
	- [ ] OSX
		- script has hardcoded path & not reliable
	- [ ] Windows
		- nothing yet

### Features

- [ ] envelopes
  * learn how envgen works
  * use existing envelope types (trapizoid)
  * make preset envelopes?

- [ ] load samples
	[x] can load mono samples
	[ ] load stereo samples
		- what to do with 2 buffers?

- [ ] sequencing
	- [ ] test play-note-at
	- [ ] implement a metronome as in Overtone
	- [ ] see how pseq in SClang works.




## DAW-like model

Oregano will have these concepts/objects:


- track: like a "track" in renoise. can have notes and a list of filters

- instrument: instrument defenitions or presets, and actual instantiated instruments.
  - I should make some instrument presets, like sine, saw, square, triangle waves.

- filters: can be added to tracks. e.g. reverb, low pass filter.


## Functionality


### Instruments

- [Done] preset instruments

- custom instruments

- adding filters to tracks

- ability to change filter parameters

- playing a note from an instrument on a track.
  * [done] can play a note from an instrument on bus 0

- load sound files into buffers, and play them using notes.
  - create instruments 
  

