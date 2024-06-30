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
  (list "rackunit-lib"))

(define scribblings
  (list "doc/rsc3-docs.scrbl" ()))
