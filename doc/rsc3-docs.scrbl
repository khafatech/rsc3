#lang scribble/manual

@title{rsc3: SuperCollider client }
@author{Nik Gaffney, et al.}

@defmodule[rsc3]

The @racketmodname[rsc3] module is a port of the  @hyperlink["http://rd.slavepianos.org/?t=rsc3"]{rsc3} @hyperlink["https://supercollider.github.io/"]{SuperCollider} client to Racket. rsc3 is written by  @hyperlink["http://rd.slavepianos.org/"]{Rohan Drape} and initially ported to Racket by @hyperlink["https://khafateh.com/"]{Mustafa Khafateh} using the Thu Jun 27 2013 version.

Documentation, help files and tutorials are included, along with the utilities to generate scribble files from the upstream supercollideer documentation.

The current version of rd–rsc3 is available at @hyperlink["https://gitlab.com/rd--/rsc3"]{gitlab}

@section{Reference & resuscitation}

@table-of-contents[]

@section{Further}

Partially regenerated documentation (rsc.help.scm → rsc.scrbl)

@; ------------------------------------------------------------------------
@include-section["help/server-command/index.scrbl"]
@include-section["help/ugen/index.scrbl"]
@include-section["tutorials/index.scrbl"]


@index-section[]
