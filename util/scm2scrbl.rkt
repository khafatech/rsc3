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
  (parser-compose
   (manyTill (char #\;) $space) ;; assume a space separation
   (x <- (many1 (and $anyChar (noneOf ";"))))
   (return (format-title x))))

;; text as any scheme comment line
(define $text
  (parser-compose
   (manyTill (char #\;) $space) ;; assume a space separation
   (x <- (many1 (and $anyChar (noneOf ";"))))
   (return (format-text x))))

;; s-expressions (not strictly, but...)
(define $sexp
  (parser-seq
   (char #\() (many1 (<or> (noneOf "()") $eol $sexp)) (char #\))))

;; code block formatting
(define $code
  (parser-seq $sexp #:combine-with format-racketblock))

;; a help file usually starts with a one line function followed by a new line
;; then either text blocks (as comments) and/or example code.

(define $help-file
  (parser-seq
   $title
   (many (<or> $code $text $eol))
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
(define format-racketblock
  (lambda (a . z)
    (format "~n@racketblock[~n~a~n]" (format-result (cons a z)))))

;; wrap title
(define (format-title s)
  (format "~n@title{~a}~n~n" (string-trim (list->string s))))

;; generic text
(define (format-text s)
  (format "~a" (list->string s)))

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
  (let* ((path (build-path help-path f))
         (body (port->string
                (open-input-file path))))
    (printf "parsing file: ~a~n" path)
    (format-result
     (parse-result $help-file body))))


;; read a help file, write a scribble file
(define (read-write-scribble f)
  (let ((body (parse-help-file f))
        (path (string-replace
               (string-append help-path f) ".scm" ".scrbl")))
    (printf "writing file: ~a~n" path)
    (with-output-to-file
        #:exists 'replace
        path
        (lambda ()
	  (printf "~a~n" scribble-preamble)
          (printf "~a~n" body)
          (printf "~a~n" scribble-postamble)))))


;; cli edition
(read-write-scribble (filename))
