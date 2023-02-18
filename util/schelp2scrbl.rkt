#! /usr/bin/env racket
#lang racket

;; Parse .schelp files and convert to .scrbl
;;
;; Copyright (C) 2022 FoAM
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/


;; Author(s)
;;  - nik gaffney <nik@fo.am>


;; Commentary
;;
;;  -> schelp -> racket (scribble)
;;  This is a q&d parser to convert .schelp files to scribble with the
;;  renaming seen in rsc3 (as closely as possible) adn should be usable
;;  as part of the racket documentation system which uses the scribble
;;  format as described at
;;                    https://docs.racket-lang.org/scribble/index.html
;;
;;  partially complete. partially automated.


(require parsack)

;; cli options
(define filename (make-parameter ""))
(define verbose? (make-parameter #f))
(define loglevel (make-parameter 0))
;; help is relative (or not at all)
(define help-path (make-parameter
                   (path->string (current-directory))))
(define output-path (make-parameter
                     (path->string (current-directory))))

(define getopt
  (when (not (vector-empty?
              (current-command-line-arguments))) ;; i.e. cli or not?
    (command-line
     #:program "schelp2scrbl"
     #:once-each
     (("-v" "--verbose")   "various verbose messages" (verbose? #t))
     (("-l" "--loglevel")  level "level of verbosity (1->inf)" (loglevel level)) 
     (("-p" "--help-path") folder "path to the directory containing help files" (help-path folder))
     (("-o" "--output")    folder "path to write converted help files" (output-path folder))

     #:args (input-file)
     (filename input-file)
     (if (file-exists? (filename))
         (printf "Converting .schelp to scribble: ~a\n" (filename))
         (raise-user-error 'scm2scrbl "File '~a' does not exist." (filename))))))

;; echoing verbosity
(define (verbose str #:level (n 0) . fmt )
  (when (and (verbose?) (<= n (loglevel)))
    (if (empty? fmt)
        (printf str)
        (apply printf str fmt))))

;;;;;;; ; ;; ;;;;  ;;;;;    ;;  ;; ;
;;
;;  parsack parsers for SuperCollider help files
;;  - fragile and error prone
;;  - tags as seen in SCDoc/SCDoc.[ly]
;;  - uses block:: to determine formatting
;;  - name remapping (sc3->rsc3) may need another pass?
;;  - minimal xref info
;;  - no validation or recovery (vaguely informative $err)
;;
;;  the grammar is described in the yacc and lex files at
;;   - https://github.com/supercollider/supercollider/blob/develop/SCDoc/SCDoc.y
;;   - https://github.com/supercollider/supercollider/blob/develop/SCDoc/SCDoc.l
;;
;;;; ;; ;;   ;   


;; block labels don't need to be explicitly closed (but sometimes are)
(define $block-tag
  (try (oneOfStringsAnyCase
        "anchor::"
        "argument::"
        "categories::"
        "class::"
        "classmethods::"
        "classtree::"
        "code::"
        "copymethod::"
        "definitionlist::"
        "description::"
        "discussion::"
        "examples::"
        "footnote::"
        "image::"
        "instancemethods::"
        "keyword::"
        "list::"
        "method::"
        "note::"
        "numberedlist::"
        "private::"
        "redirect::"
        "related::"
        "returns::"
        "section::"
        "subsection::"
        "summary::"
        "table::"
        "title::"
        "tree::"
        "warning::")))

;; inline stuff like 'emphasis' etc. (need to be explicitly closed)
(define $inline-tag
  (try (oneOfStringsAnyCase
        "emphasis::"
        "link::"
        "soft::"
        "strong::"
        "teletype::")))

;;  most of these 'sections' can use generic formatting (parsed via $section)
;;  remove any specifics from this list when adding a specialised parser...

(define $section-name
  (try (oneOfStringsAnyCase
        "anchor::"
        "argument::"
        "categories::"
        "classmethods::"
        "classtree::"
        "copymethod::"
        "definitionlist::"
        "discussion::"
        "examples::"
        "footnote::"
        "image::"
        "instancemethods::"
        "keyword::"
        "list::"
        "method::"
        "note::"
        "numberedlist::"
        "private::"
        "redirect::"
        "related::"
        "returns::"
        "section::"
        "subsection::"
        "table::"
        "tree::"
        "warning::")))

(define $endtag (try (string "::")))

;; a line ends when it ends (and ignores inline tags)
(define $line-terminator
  (<?> (<or> $eol $newline)
       "line terminator"))

;; a block ends when it's closed with "::" or
;; another another block starts, or it just ends...
(define $block-terminator
  (<?> (<or> (lookAhead $block-tag)
             ;; $endtag ;; optional, but only after parsing inline tags
             $eof)
       "block terminator"))

;; a line of zero or more characters
(define $line
  (manyUntil $anyChar $line-terminator))

;; inline styling
(define $markup
  (parser-compose
   (t <- $inline-tag)
   (x <- (many $anyChar))
   $endtag
   (return (inline-markup t x))))

(define $line-inline
  (<or> $markup
        $line))

;; just unfomratted "for now" (dispatch on t to format x)
(define  (inline-markup t x)
  (verbose "markup: ~a~n" t #:level 1)
  (format x))

;; a block is multiple lines (etc...)
(define $block
  (parser-compose
   (x <- (manyUntil $anyChar $block-terminator))
   (return x)))

;; specifics
(define $title
  (parser-compose
   (oneOfStringsAnyCase "class::" "title::")
   (x <- $line)
   (return (format-title x))))

(define $summary
  (parser-compose
   (oneOfStringsAnyCase "summary::")
   (x <- $line)
   (return (format-summary x))))

(define $description
  (parser-compose
   (oneOfStringsAnyCase "description::")
   (x <- $block)
   (return (format-description x))))

(define $code
  (parser-compose
   (oneOfStringsAnyCase  "code::")
   (x <- $block)
   (return (format-racketblock x))))

;; generics
(define $section
  (parser-compose
   (s <- $section-name) ;; catch undefined?
   (x <- $block)
   (return (format-block s x))))

;; a help file usually starts with a title or class name followed by a new line
;; then usually (zero or more) sections of text

(define $schelp-file
  (parser-seq
   (many $title)
   (many (<or> $summary
               $description
               $section
               $code
               $newline
               ))
   $eof))

;; parse a string with given parser
(define (parse->string p s)
  (verbose "parse->string~n parser: ~a~n string: ~a~n" #:level 1)
  (string-append (parse-result p s)))

;;;;;;; ; ;; ;;;;;;;;;    ;;  ;; ;
;;
;;   scribbling & formatting
;;
;;;;; ;   ; ;  ;;;    ;

;; any header or footer info required for the scribble file
(define scribble-preamble
  "#lang scribble/manual\n@(require (for-label racket))")

(define scribble-postamble "")


(define (format-summary s)
  (verbose "format-summary: ~a~n" s  #:level 0)
  (format "~a" (format-result s)))

(define (format-description s)
  (verbose "format-description: ~a~n" s #:level 0)
  (format "@section{description}~n~a" (format-result s)))

;; wrap codeblock
(define (format-racketblock a . z)
  (verbose "format-racketblock: ~a~n" (cons a z) #:level 0)
  (format "~n@racketblock[~a]~n" (format-result (cons a z))))

;; wrap title
(define (format-title s)
  (verbose "format-title: ~a~n" s #:level 0)
  (format "~n@title{~a}~n" (string-trim (format-result s))))

;; generic text
(define (format-block s x)
  (verbose "format-block: ~a :: ~a~n" s x #:level 0)
  (format "~a ~a"  (format-section-name s) (format-result x)))

(define (format-result l)
  (verbose "format-result: ~s~n" l  #:level 2)
  (string-trim 
   (foldl (lambda (e acc)
            (cond
              ((string? e) (string-append acc e))
              ((char? e) (string-append acc (~a e )))
              (else (verbose "~nunformatted?: ~a~n" e))))
          ""
          (flatten l))
   "::" ));; remove any stray $endtag (fix w. $inline)

(define (format-section-name s)
  (format "@section{~a}~n" (string-trim (format-result s) "::"))) ;; drop tag suffix


;; parse an .schelp file into a formatted string
(define (parse-help-file f)
  (if (non-empty-string? f)
      (let* ((path (build-path (help-path) f))
             (body (port->string
                    (open-input-file path))))
        (printf "parsing file: ~a~n" path)
        (format-result
         (parse-result $schelp-file body)))
      (printf "No help file.")))


;; read a help file, write a scribble file
(define (read-write-scribble f)
  (let ((body (parse-help-file f))
        (path (string-replace
               (string-append (output-path) f) ".schelp" ".scrbl")))
    (printf "writing file: ~a~n" path)
    (with-output-to-file
        #:exists 'replace
      path
      (lambda ()
        (printf "~a~n" scribble-preamble)
        (printf "~a~n" body)
        (printf "~a~n" scribble-postamble)))))


;;;;;;; ; ;; ;;;;;;;;;;;;; ;
;;
;; cli edition
;;

(read-write-scribble (filename))
