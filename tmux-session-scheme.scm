#!/usr/bin/env gsi-script


;;;; Configuration =================================================

;;;; Errors ========================================================

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
	(newline))

;;;; Functionality =================================================

(define find-args
  (list (string-append (getenv "HOME") "/dev") "-mindepth" "1" "-maxdepth" "1" "-type" "d"))

(define find-config (list path: "find" arguments: find-args))

(define (selection)
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
;; - handle args (list of paths from users $HOME) or missing paths
;; - find if the tmux process exists and open a port to it
;; - check if a tmux session exists which matches the name of the directory
;; - create a new session if not, or attach if true
(define (run args)
; check for find and fzf
	; define the options from the args with a procedure that extracts the
	(print-usage))

(run (cdr (command-line)))
