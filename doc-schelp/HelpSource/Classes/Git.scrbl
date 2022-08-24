#lang scribble/manual
@(require (for-label racket))

@title{Git}
 git interface@section{categories}
  Frontend
@section{related}
  Classes/Quarks, Classes/Quark

@section{description}

An interface to the git toolchain. For more information on git, see link::http://git.io::.


@section{CLASSMETHODS}
 



@section{METHOD}
  new
creates a new instance of 
@racketblock[Git::, pointing to an existing local git repository.
]
@section{argument}
  localPath
path to the git repository.


@section{METHOD}
  isGit
returns 
@racketblock[true::, if a local directory is a git repository.
]
@section{argument}
  localPath

@section{METHOD}
  checkForGit
returns 
@racketblock[true::, if the git toolchain is found on the system.


]
@section{INSTANCEMETHODS}
 

@section{subsection}
  info

@section{METHOD}
  remote, url
@section{returns}
  url of the first remote that it finds.

@section{METHOD}
  remoteLatest
@section{returns}
  hash of latest commit on the remote

@section{METHOD}
  localPath
@section{returns}
  path to local repository

@section{METHOD}
  tag
@section{returns}
  currently checked out tag

@section{METHOD}
  tags
@section{returns}
  avaliable tags

@section{METHOD}
  sha
@section{returns}
  hash of the currently checked out version

@section{METHOD}
  shaForTag
@section{argument}
  tag
one of the tags returned by link::#-tags::
@section{returns}
  hash of the given tag


@section{METHOD}
  isDirty
@section{returns}
  
@racketblock[true:: if there are local changes



]
@section{subsection}
  perform actions on remote

@section{METHOD}
  fetch
perform a fetch from remote


@section{METHOD}
  checkout
perform a checkout from remote with argument 
@racketblock[refspec::
]
@section{argument}
  refspec

@section{METHOD}
  pull
perform a pull from remote

@section{METHOD}
  clone
perform a clone from url into link::#-localPath::
@section{argument}
  url
the url of the remotes


@section{PRIVATE}
  git, refspec



@section{EXAMPLES}
 


@racketblock[
// create a Git that points to a Quark directory
g = Git(Quarks.all.choose.localPath);

// alternatively, provide a pathname to a local git repository:
g = Git("/path/to/local/repo");

// get all available tags
g.tags;

// return local path
g.localPath;

// return url
g.url;
]


