#! /usr/bin/env racket
#lang racket

;; Parse .schelp.scm files and convert to .scrbl
;;
;; Copyright (C) 2022 FoAM
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/


;; Author(s)
;;  - nik gaffney <nik@fo.am>


;; Commentary
;;
;;  (schelp -> scm ) -> racket (scribble)
;;  This is a q&d parser to convert .help.scm files as seen in rsc3
;;  to something usable as part of the racket documentation system
;;  which uses the scribble format as described at
;;                   https://docs.racket-lang.org/scribble/index.html
;;
;;  partially complete. partially automated.


(require parsack)

;; cli options
(define filename (make-parameter ""))
(define verbose? (make-parameter #f))
;; help is relative (or not at all)
(define help-path (make-parameter
                    (path->string (current-directory))))

(define getopt
  (command-line
   #:program "scm2scrbl"
   #:once-each
   (("-v" "--verbose")   "various verbose messages" (verbose? #t))
   (("-p" "--help-path") folder "path to the directory containing help files" (help-path folder))
   #:args (input-file)
   (filename input-file)
   (if (file-exists? (filename))
       (printf "Converting .help.scm to scribble: ~a\n" (filename))
       (raise-user-error 'scm2scrbl "File '~a' does not exist." (filename)))))

;; echoing verbosity
(define-syntax verbose
  (syntax-rules ()
    ((verbose str ...) (when (verbose?) (printf str ...)))))


;; parsack parser for base case, assuming well formatted input...
;;  - fragile and error prone
;;  - assume first line is title
;;  - all comments are considered 'text'
;;  - any code is wrapped in a racketblock
;;  - multiline examples
;;  - minimal xref info


;; title as first comment line
(define $title
  (<?> (parser-compose
        (manyTill (char #\;) $space) ;; assume a space separation
        (x <- (manyTill $anyChar $eol))
        (return (format-title x)))
       "title"))

;; s-expressions (not strictly, but...)
(define $sexp
  (<?> (parser-seq
        (char #\() (many (<or> $sexp (and $anyChar (noneOf "()")))) (char #\)))
       "sexp"))

;; text as any scheme comment line
(define $text-terminator (<?> $eol "text terminator"))

(define $text-line
  (<?> (parser-compose
        (parser-seq (many1 (char #\;)) $space) ;; assume a space separation of comments
        (x <- (manyUntil $anyChar $text-terminator))
        (return (format-text x)))
       "text"))

;; block comment 
(define $text-block
  (<?> (parser-compose
        (string "#|") 
        (x <- (many (and $anyChar (<!> (try (char #\|))))))
        (string "|#") 
        (return (format-text x)))
       "text"))

;; blank line
(define $blank (<?> (<any> $newline $space $eol) "blank line"))

;; code block formatting
(define $code
  (<?> (parser-seq $sexp #:combine-with format-racketblock) "code"))

;; a help file usually starts with a one line function followed by a new line
;; then either text blocks (as comments) and/or example code.

(define $help-file
  (parser-seq
   $title
   (many (<any> $code $text-line $text-block $blank))
   $eof))

;; parser -> string -> string
(define (parse->string p s)
  (string-append (parse-result p s)))

;; scribbling & formatting

;; any header or footer info required for the scribble file
(define  scribble-preamble
  "#lang scribble/manual\n@(require (for-label racket))")

(define  scribble-postamble "")

;; wrap codeblock
(define (format-racketblock a . z)
   (verbose "format racket block: ~a~n" (cons a z))
    (format "~n@racketblock[~n~a~n]" (format-result (cons a z))))

;; wrap title
(define (format-title s)
  (verbose "format title: ~a~n" s)
  (format "~n@title{~a}~n~n" (string-trim (list->string s))))

;; generic text
(define (format-text s)
  (verbose "format text: ~a~n" s)
  (format "~a~n" (list->string s)))

(define (format-result l)
  (verbose "formatting result: ~s~n" l)
  (foldl (lambda (e acc)
           (cond
             ((string? e) (string-append acc e))
             ((char? e) (string-append acc (~a e )))
             (else (verbose "~nunformatted?: ~n" e))))
         ""
         (flatten l)))

;; parse a .help.scm file into formatted string
(define (parse-help-file f)
  (let* ((path (build-path (help-path) f))
         (body (port->string
                (open-input-file path))))
    (verbose "parsing file: ~a~n" path)
    (format-result
     (parse-result $help-file body))))


;; read a help file, write a scribble file
(define (read-write-scribble f)
  (let ((body (parse-help-file f))
        (path (string-replace
               (string-append (help-path) f) ".scm" ".scrbl")))
    (verbose "writing file: ~a~n" path)
    (with-output-to-file
        #:exists 'replace
        path
        (lambda ()
	  (printf "~a~n" scribble-preamble)
          (printf "~a~n" body)
          (printf "~a~n" scribble-postamble)))))

;; testing
(define sc0 ";; (demand-env-gen rate levels times shapes curves gate reset levelScale levelOffset timeScale doneAction)

;; levels - a demand ugen or any other ugen

;; times  - a demand ugen or any other ugen if one of these ends,
;;          the doneAction is evaluated

;; shapes - a demand ugen or any other ugen, the number given is
;;          the shape number according to Env

;; curves - a demand ugen or any other ugen, if shape is 5, this
;;          is the curve factor some curves/shapes don't work if
;;          the duration is too short. have to see how to improve
;;          this. also some depend on the levels obviously, like
;;          exponential cannot cross zero.

;; gate   - if gate is x >= 1, the ugen runs, if gate is 0 > x > 1,
;;          the ugen is released at the next level (doneAction), if
;;          gate is x < 0, the ugen is sampled end held

;; reset  - if reset crosses from nonpositive to positive, the ugen
;;          is reset at the next level, if it is > 1, it is reset
;;          immediately.

;; Frequency envelope with random times.

(let* ((l (dseq dinf (make-mce (list 204 400 201 502 300 200))))
       (t (drand dinf (make-mce (list 1.01 0.2 0.1 2.0))))
       (y (mouse-y kr 0.01 3 1 0.1))
       (f (demand-env-gen ar l (mul t y) 7 0 1 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar (mul f (mce2 1 1.01)) 0) 0.1))))

;; Frequency modulation

(let* ((x (mouse-x kr -0.01 -4 0 0.1))
       (y (mouse-y kr 1 3000 1 0.1))
       (l (lambda () (dseq dinf (clone 32 (exp-rand 200 1000)))))
       (t (mul sample-dur y))
       (f (demand-env-gen ar (mce2 (l) (l)) t 5 x 1 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))

;;  gate. Mouse x on right side of screen toggles gate.

(let* ((x (mouse-x kr 0 1 0 0.1))
       (l (u:round (dwhite dinf 300 1000) 100))
       (f (demand-env-gen kr l 0.1 5 0.3 (gt x 0.5) 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar (mul f (mce2 1 1.21)) 0) 0.1))))
")

(define ss0 "(let* ((x (mouse-x kr -0.01 -4 0 0.1))
       (y (mouse-y kr 1 3000 1 0.1))
       (l (lambda () (dseq dinf (clone 32 (exp-rand 200 1000)))))
       (t (mul sample-dur y))
       (f (demand-env-gen ar (mce2 (l) (l)) t 5 x 1 1 1 0 1 do-nothing)))
  (audition (out 0 (mul (sin-osc ar f 0) 0.1))))
")

(define bc0 ";; (pv-jensen-andersen buffer propsc prophfe prophfc propsf threshold waittime)

#|

fft feature detector for onset detection based on work described in
Jensen,K. & Andersen, T. H. (2003). Real-time Beat Estimation Using
Feature Extraction. in Proceedings of the Computer Music Modeling and
Retrieval Symposium, Lecture Notes in Computer Science. springer
Verlag.

First order derivatives of the features are taken. Threshold may
need to be set low to pick up on changes.

buffer    - fft buffer to read from.
propsc    - Proportion of spectral centroid feature.
prophfe   - Proportion of high frequency energy feature.
prophfc   - Proportion of high frequency content feature.
propsf    - Proportion of spectral flux feature.
threshold - Threshold level for allowing a detection
waittime  - If triggered, minimum wait until a further frame can
            cause another spot (useful to stop multiple detects on
            heavy signals)

Default values in sclang are: propsc=0.25, prophfe=0.25,
prophfc=0.25, propsf=0.25, threshold=1.0, waittime=0.04.

|#

(with-sc3
 (lambda (fd)
   (async fd (b-alloc 0 2048 1))))

(let* ((source (sound-in 0))
       (detect (pv-jensen-andersen (fft* 0 source)
				  0.25 0.25 0.25 0.25
				  (mouse-x kr 0.01 1.0 1 0.1)
				  0.04)))
  (audition
   (out 0 (mul (sin-osc ar (mce2 440 445) 0)
	       (decay (mul 0.1 detect) 0.1)))))
")

;(format-result (parse-result $code ss0))

;(format-result (parse-result $help-file sc0))

;; cli edition
(read-write-scribble (filename))
