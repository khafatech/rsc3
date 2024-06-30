#lang setup/infotab

(define collection 'multi)

(define deps
  (list "base"
        "gui-lib"
        "r6rs-lib"
        "srfi-lib"
        "srfi-lite-lib"
        "parsack" ;; for parsing schelp docs
        ))

(define build-deps
  (list "racket-doc"
        "scribble-lib"
        "rackunit-lib"))

(define scribblings
  '(("doc/rsc3-docs.scrbl" (multi-page))))
