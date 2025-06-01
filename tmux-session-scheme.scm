#!/usr/bin/env gsi-script
;; -*- Scheme -*-

;;;; Configuration =================================================

(define version '0.0.1)

(define MIN-DEPTH '0)
(define MAX-DEPTH '1)

;;;; Errors ========================================================

(define (missing-deps dependency)
	(display (string-append "Error: " dependency " is required!"))
	(newline)
	(exit))

(define (print-usage)
	(display "tmux-session-scheme")
	(newline)
	(display "Usage:")
	(newline)
	(display "    tmux-session-scheme <directory-path> [--depth <num>]")
	(newline)
	(display "Example:")
	(newline)
	(display "    tmux-session-scheme ~/Documents ~/work --depth 2")
	(newline)
	(display "Arguments:")
	(newline)
	(display "    --depth <num>: the number of directory levels to traverse. Default: 1.")
	(newline)
	(exit))


;;;; Functionality =================================================

; Credit "https://cookbook.scheme.org/split-string/"
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))

(define (check-commands cmd-list)
	(if (not (null? cmd-list))
			(if (not (equal? (car (shell-command (car cmd-list) #t)) 0))
					(missing-deps (car (string-split char-whitespace? (car cmd-list))))
					(check-commands (cdr cmd-list)))))


(define (find-config args)
	(list path: "find"
				arguments: (append (car args) (list "-mindepth" (number->string MIN-DEPTH) "-maxdepth" (cadr args) "-type" "d"))))

(define (get-selection processed-args)
	(with-output-to-process
		(list path: "fzf" stdout-redirection: #f)
		(lambda ()
			(define find (open-input-process (find-config processed-args)))
			(let loop ((line (read-line find)))
				(if (not (eof-object? line))
						(begin
							(display line)
							(newline)
							(loop (read-line find)))))
			(close-port find))))


;;; Returns true if the path string points to a valid path, and it is a directory
(define (directory? path-str)
	(if (file-exists? path-str)
			(equal? (file-type path-str) 'directory)
			#f))

;;; process args return list of sub-lists
;;; the first is a list of valid directory with trailing slash removed
;;; the second is (optional) depth level number
;;; exits is finds help or version flags, or if paths are not valid directories or
;;; depth value is not a valid number between 1-5
(define (process-args arguments)
	(let loop ((args arguments)
						 (paths '())
						 (depth (number->string MAX-DEPTH)))
		(cond ((null? args) ; if there are no args left then return the options list
					 (list paths depth))
					 ; if flag -h or --help print usage
					((or (member "-h" args) (member "--help" args))
					 (print-usage))
					; if flag -h or --help print usage
					((or (member "-v" args) (member "--version" args))
					 (display version)
					 (newline)
					 (exit))
					; if depth flag get the next arg into the depth element and continue
					((equal? (car args) "--depth")
					 ; TODO: find out how to check if NaN... using `nan?` doesn't work as I expect
					 (if (or (null? (cdr args))
									 (< (string->number (cadr args)) 1)
									 (> (string->number (cadr args)) 5))
							 (print-usage)
							 (loop (cdr (cdr args))
										 paths
										 (cadr args))))
					; if arg is valid directory path continue loop adding it to paths list
					((directory? (car args))
					 (loop (cdr args)
								 (cons (path-strip-trailing-directory-separator (car args)) paths)
								 depth))
					; catch all non-conformant args
					(else (print-usage)))))


;;;; Main ==========================================================

;; TODO:
;; - [x] check fzf and tmux are installed
;; - [x] extract filepaths and any flagged opts from args
;; - [x] parse filepaths and conform to string list for find command
;; - [x] pipe from find->fzf and return the selection
;; - [ ] get the last el of selected path
;; - [ ] check if the tmux process exists, create if not,
;;       and attach to port (?) or save ref (?)
;; - [ ] check if a tmux session exists which matches the selected path
;; - [ ] create a new session if not, or attach if true
;; - [ ] bonus: spin up new session with two tabs: nvim, zsh
;; - [ ] bonus: allow tmux config passed in to define how to spin up new session
(define (run arguments)
	(check-commands '("fzf --version" "tmux -V"))
	(if (null? arguments)
		(print-usage))
	(display (get-selection (process-args arguments))))

(run (cdr (command-line)))
