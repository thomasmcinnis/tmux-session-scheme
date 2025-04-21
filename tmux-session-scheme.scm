#!/usr/bin/env gsi-script
;; -*- Scheme -*-

;;;; Configuration =================================================

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
	(display "    tmux-session-scheme <file-path> [--depth <num>]")
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


(define find-args
  (list (string-append (getenv "HOME") "/dev") "-mindepth" "1" "-maxdepth" "1" "-type" "d"))

(define find-config (list path: "find" arguments: find-args))

(define (get-selection)
	(with-output-to-process
		(list path: "fzf" stdout-redirection: #f)
		(lambda ()
			(define find (open-input-process find-config))
			(let loop ((line (read-line find)))
				(if (not (eof-object? line))
						(begin
							(display line)
							(newline)
							(loop (read-line find)))))
			(close-port find))))


;;;; Main ==========================================================

;; TODO:
;; - [x] check fzf and tmux are installed
;; - [ ] extract filepaths and any flagged opts from args
;; - [ ] parse filepaths and conform to string list for find command
;; - [x] pipe from find->fzf and return the selection
;; - [ ] get the last el of selected path
;; - [ ] check if the tmux process exists, create if not,
;;       and attach to port (?) or save ref (?)
;; - [ ] check if a tmux session exists which matches the selected path
;; - [ ] create a new session if not, or attach if true
;; - [ ] bonus: spin up new session with two tabs: nvim, zsh
;; - [ ] bonus: allow tmux config passed in to define how to spin up new session
(define (main)
	(check-commands '("fzf --version" "tmux -V"))
	(get-selection))

