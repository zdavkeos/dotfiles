;;; funcs.el --- Handy elisp functions
;; -*- mode: emacs-lisp-mode; -*-


;;; Commentary:
;; Handy elisp functions.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dos2unix and unix2dos converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unix2dos ()
  "Set encoding to dos line endings."
  (interactive)
  (message "Converting line endings to dos(CRLF). **Save buffer to commit")
  (setq buffer-file-coding-system 'undecided-dos)
  (set-buffer-modified-p t))

(defun dos2unix ()
  "Replace CR's with nothing and set encoding to unix."
  (interactive)
  (message "Converting line endings to unix (LF). **Save buffer to commit")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\x0d" nil t)
      (replace-match "")))
  (setq buffer-file-coding-system 'undecided-unix)
  (set-buffer-modified-p t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert template when certain files are created.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar template-files-dir "~/Templates/")
(defvar ext-template-map '(("py"   . "Py File.py")
						   ("c"    . "C File.c")
						   ("html" . "HTML File.html")
						   ("java" . "Java File.java")))

(defun insert-template()
  "Ask user to insert a file template when a new blank buffer is created
whose extense matches one of the available templates."
  (let* ((ext (cadr (split-string (buffer-name) "\\.")))
         (template (cdr (assoc ext ext-template-map))))
    (when (and template (string= (buffer-string) ""))
      (if (yes-or-no-p (concat "Insert template for " ext " file? "))
          (insert-file-contents (concat template-files-dir template))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rm-wsp - extraneous whitespace remover
;; -> deprecated - use whitespace-cleanup instead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun rm-wsp ()
;  "Remove whitespace from blank lines, and ends of lines."
;  (interactive)
;  (save-excursion
;    (goto-char (point-min))
;    (while (re-search-forward "[[:space:]]+$" nil t)
;      (replace-match "\n")))
;  (message "Extraneous whitespace removed"))
(defalias 'rm-wsp 'whitespace-cleanup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date, Time, and signature functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun signature ()
  "Spit out my name and email."
  (interactive)
  (insert (concat "~" user-full-name " - " user-mail-address)))

(defun datestamp ()
  "Print out the current date mm-dd-yyyy."
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))

(defun datestamp-fancy ()
  "Print out the current date: month day, year."
  (interactive)
  (insert (format-time-string "%b %d, %Y")))

(defun timestamp ()
  "Print out the current date mm-dd-yyyy : hh:mm:ss."
  (interactive)
  (insert (format-time-string "%m-%d-%Y : %T")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; C-w, and M-w now kill whole line if no region is selected
;; copied shamelessly from emacs-fu.blogspot.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive (if mark-active (list (region-beginning) (region-end))
   (message"Copied line")
   (list (line-beginning-position)
     (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
       (line-beginning-position 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Amended comment-region (python)
;;  if no region is selected, comment line instead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice comment-region (before comment-line-if-no-region activate compile)
  "Comment region, or line if no region."
  (interactive (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position) (line-beginning-position 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-region* - Diff two regions
;;
;;  To compare two regions, select the first region and run
;; `diff-region'.  The region is copied to a temporary buffer.  Next,
;; navigate to the next region in question (even in another buffer or
;; file).  Mark the region and run `diff-region-now', the diff of the
;; two regions will be displayed by `ediff'.
;;
;;  You can re-select the first region at any time by re-calling
;; `diff-region' again.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-region (start end)
  "Select a region to compare.
Should be followed by a call to  `diff-region-now'.
Argument START Start of first region.
Argument END End of first region."
  (interactive "r")
  (let ((buf (get-buffer-create "*Diff-regionA*")))
	(with-current-buffer buf (erase-buffer))
	(append-to-buffer buf start end))
  (message "Select second region to compare and run `diff-region-now'"))

(defun diff-region-now (start end)
  "Compares current region with region selected by `diff-region'.
Argument START Start of second region.
Argument END End of second region."
  (interactive "r")
  (let ((bufa (get-buffer-create "*Diff-regionA*"))
		(bufb (get-buffer-create "*Diff-regionB*")))
	(with-current-buffer bufb (erase-buffer))
	(append-to-buffer bufb start end)
	(ediff-buffers bufa bufb)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let* ((pos-end (line-beginning-position 2))
         (line    (buffer-substring (line-beginning-position) pos-end)))
    (save-excursion
      (goto-char pos-end)
      (unless (bolp) (newline))
      (insert line))
    (forward-line)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unescape xml buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-unescape-buffer (start end)
  "Un-escape a buffer of escaped XML.
Argument START Start of region to un-escape.
Argument END End of escaped region."
  (interactive "r")
;  (save-excursion
  (goto-char start)
  (while (search-forward-regexp "&\\(lt\\|gt\\|#xD\\|amp\\|quot\\|apos\\);" end t)
	(let* ((m1 (match-string 1))
		   (rep (assoc m1 xml-escapes)))
	  (when rep
		(replace-match (cdr rep) nil t)))))

(defvar xml-escapes
  '(("#xD"  . "") ; technically newline...
    ("gt"   . ">")
    ("lt"   . "<")
    ("quot" . "\"")
    ("apos" . "'")
    ("amp"  . "&")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fix-patch-paths ()
  "Fix paths in patch files made on windows (fix slashes) (cvs diff)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\+\\+\\+ |Index: \\|\\-\\-\\- \\|RCS file:\\).*$")
      (let* ((str (match-string 0))
			 (rep (replace-regexp-in-string "/" "\\" str t t)))
		(replace-match rep t t)))
    (message "Done.")))


;;;;;;;;;;;;;;;;;;;;
;; backward-kill-line - from the current cursor position,
;;   kill everthing back to the start of the line, but not
;;   then newline itself
;;;;;;;;;;;;;;;;;;;;
(defun backward-kill-line ()
  "Kill from current point to the beginning of the current line."
  (interactive)
  (while (not (bolp))
	(delete-backward-char 1)))
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; interactive function skeleton
;(defun my-foo-func ()
;  "Foo func!!"
;  (interactive)
;  (save-excursion
;   (goto-char (point-min))
;   ; move around and do cool stuff...
;   ; ...
;   (message "other stuff...")))
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; align repeatedly on regexp
;;;;;;;;;;;;;;;;;;;;
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression.
Argument START Beginning of region.
Argument END End of region.
Argument REGEXP Regex to align on."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;;;;;;;;;;;;;;;;;;;;
;; print a new week in TODO
;;;;;;;;;;;;;;;;;;;;
(defun new-week ()
  "Print blank week schedule."
  (interactive)
  (save-excursion
    (insert
     (format-time-string
"* Week %m/%d/%y
  * Mon
    -
  * Tue
    -
  * Wed
    -
  * Thu
    -
  * Fri
    -
"))))


(defun reinterpret-hex (start end)
  "Show ASCII hex values in plain ASCII in another buffer.
Looks through entire buffer if no region is selected.
Argument START .
Argument END End of region."
  (interactive "r")
  (let ((buf (get-buffer-create "*reinterpret-hex*")))
	(with-current-buffer buf (erase-buffer))
	(save-excursion
	  (goto-char start)
	  (while (search-forward-regexp "\\([0-9a-fA-F]\\{2\\}\\)" end t)
		(let ((m1 (match-string 1)))
		  (with-current-buffer buf
			(insert (format "%s: %c\n" m1 (string-to-number m1 16)))))))
	(display-buffer buf)))

(defun extract-wrs (start end sep)
  "Search for wr numbers in a region.
Results are are displayed in the *WRS* buffer.
Argument START Start of region.
Argument END End of region.
Argumen SEP Output seperator"
  (interactive "r\nsSeperator: ")
  (let ((buf (get-buffer-create "*WRS*")))
	(with-current-buffer buf (erase-buffer))
	(save-excursion
	  (goto-char start)
;	  (while (search-forward-regexp "[0-9]\\{4,5\\}" end t) ;tends to pick up years...
	  (while (search-forward-regexp "[0-9]\\{5\\}" end t)
		(let ((str (match-string 0)))
		  (with-current-buffer buf
			(insert str sep)))))
	(display-buffer buf)))


; (defun keep-matches-only (regexp)
;   "Given a regex, delete everything in buffer except matches"
;   (interactive "sRegexp: ")
;   (save-excursion
; 	(goto-char (point-min))
; 	(while (not (eobp))
; 	  (let ((cur (point)))
; 		(search-forward-regexp regexp)
; 		(kill-region cur (match-beginning 0))))))


; based on sample from documentation for `sort-subr'
(defun sort-lines (reverse beg end)
  "Sort lines in region by length"
  (interactive "P\nr")
  (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (let ((inhibit-field-text-motion t))
		(sort-subr reverse 'forward-line 'end-of-line 
				   (lambda ()
					 (save-excursion
					   (beginning-of-line)
					   (let ((strt (point)))
						 (end-of-line)
						 (- (point) strt)))))))))

; insert c decl string
(defun c-ins-fname ()
  "insert current function name at point"
  (interactive)
  (insert (c-defun-name)))

; insert c decl string comment
(defun c-ins-fname-decl ()
  "insert current function name at point"
  (interactive)
  (insert (format "static const char name[] = \"%s\";" (c-defun-name))))

; run calc on region
(defun calc-region (beg end)
  (interactive "r")
  (when (region-active-p)
	(calc-embedded nil)
	(calc-embedded nil)
	(message "Calculated")))

(defun keepers (beg end re)
  (interactive "r\ne")
  (let ((buf (get-buffer-create "*KEEPERS*")))
	(save-excursion
	  (goto-char start)
	  (while (search-forward-regexp re end t)
		(let ((str (match-string 0)))
		  (with-current-buffer buf
			(insert str "\n")))))
	(display-buffer buf)))

(provide 'funcs)

;;; funcs.el ends here

