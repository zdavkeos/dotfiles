;;; funcs.el --- Handy elisp functions
;; -*- mode: emacs-lisp-mode; -*-

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dos2unix and unix2dos converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unix2dos ()
  "Set encoding to dos line endings."
  (interactive)
  (message "Converting line endings to dos(CRLF). **Save buffer to commit")
  (setq buffer-file-coding-system 'undecided-dos)
  (not-modified t)
)

(defun dos2unix ()
  "Replace CR's with nothing and set encoding to unix."
  (interactive)
  (message "Converting line endings to unix (LF). **Save buffer to commit")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\x0d" nil t)
        (replace-match "")))
  (setq buffer-file-coding-system 'undecided-unix)
  (not-modified t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert template when certain files are created.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq template-files-location "~/Templates/")
(setq ext-template-map '(("py"   . "Py File.py")
			 ("c"    . "C File.c")
			 ("html" . "HTML File.html")
			 ("java" . "Java File.java")) )

(defun insert-template()
  "ask user to insert template certain file creations"
  (let (ext template)
	(setq ext (car (cdr (split-string (buffer-name) "\\."))))
	(setq template (cdr (assoc ext ext-template-map)))
	  (if template
	    (progn
              (if (string= (buffer-string) "")
	      (progn
              (if (yes-or-no-p (concat "Insert template for " ext " file? "))
	          (insert-file (concat template-files-location template)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rm-wsp - extraneous whitespace remover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rm-wsp ()
  "Remove whitespace from blank lines, and ends of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\s-+$" nil t)
        (replace-match "")
	)

    (goto-char (point-min))
    (while (re-search-forward "^[:space:]+$" nil t)
        (replace-match "")
	)
  )
  (message "Extraneous whitespace removed")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date, Time, and signature functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun signature ()
  "Spit out my name and email."
  (interactive)
  (insert (concat "~" user-full-name " - " user-mail-address)))
(global-set-key [f1]  'signature)

(defun datestamp ()
  "Print out the current date mm-dd-yyyy."
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))
(global-set-key [f2]  'datestamp)

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
  (interactive)
  (if mark-active (list (region-beginning) (region-end))
	(list (line-beginning-position) (line-beginning-position 2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change python mode's comment-dwim to
;;  comment out line if no region is selected
;;  rather than add an end-of-line comment.
;;
;;  this could be a defadvice, but sometimes i like the
;;  default behavior...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-dwim-line-if-no-region ()
  (interactive)
  (if mark-active (list (region-beginning) (region-end))
                  (list (line-beginning-position) (line-beginning-position 2)))
  (lambda () (comment-dwim)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-region* - Diff two regions
;;
;;  To compare two regions, select the first
;; region and run `diff-region`.  The region is
;; now copied to a seperate diff-ing buffer.
;; Next, navigate to the next region in question
;; (even in another file).  Mark the region and
;; run `diff-region-now`, the diff of the two
;; regions will be displayed by ediff.
;;
;;  You can re-select the first region at any time
;; by re-calling `diff-region`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-region ()
  "Select a region to compare."
  (interactive)
  (when (use-region-p)  ; there is a region
		(let (buf)
		  (setq buf (get-buffer-create "*Diff-regionA*"))
		  (save-current-buffer
			(set-buffer buf)
			(erase-buffer))
		  (append-to-buffer buf (region-beginning) (region-end)))
  )
  (message "Now select other region to compare and run `diff-region-now`")
)

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region'."
  (interactive)
  (when (use-region-p)
		(let (bufa bufb)
		  (setq bufa (get-buffer-create "*Diff-regionA*"))
		  (setq bufb (get-buffer-create "*Diff-regionB*"))
		  (save-current-buffer
			(set-buffer bufb)
			(erase-buffer))
		  (append-to-buffer bufb (region-beginning) (region-end))
		  (ediff-buffers bufa bufb))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplcate line down
;;
;; Copy the current line and paste a copy on a new line below the
;; current one
;;
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
	(next-line)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unescape xml buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-unescape-buffer ()
  "Un-escape a buffer of escaped XML."
  (interactive)
;  (save-excursion
  (let (m1 rep)
	(while (search-forward-regexp "&\\(lt\\|gt\\|#xD\\|amp\\|quot\\|apos\\);" nil t)
	  (progn
		(setq m1 (match-string 1))
		(message m1)
		(setq rep (assoc m1 xml-escapes))
		(when rep
		  (replace-match (cdr rep) nil t))))))

(defvar xml-escapes
  '(("#xD" . "") ; technically newline...
	("gt" . ">")
	("lt" . "<")
	("quot" . "\"")
	("apos" . "'")
	("amp" . "&")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from stackoverflow, make #if 0...#else
;;  a c-mode comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face))))) nil)

(defun fix-patch-paths ()
  "Fix paths in patch files made on windows (fix slashes)."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\(\\+\\+\\+\\|Index: \\|\\-\\-\\-\\|RCS file:\\).*$")
	  (let ( (str (match-string)) )
			(setq str (replace-regexp-in-string "/" "\\" (match-string 0) t t))
			(replace-match str t t)))
	(message "Done.")))


;;;;;;;;;;;;;;;;;;;;
;; backward-kill-line - from the current cursor position,
;;   kill everthing back to the start of the line, but not
;;   then newline itself
;;;;;;;;;;;;;;;;;;;;
(defun backward-kill-line ()
  "Kill from current point to the beginning of the current line."
  (interactive)
  (while (not (bolp)) (delete-backward-char 1)))
;  (delete-region () (point)))
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; interactive function skeleton
;(defun my-foo-func ()
;  "Foo func!!"
;  (interactive)
;  (save-excursion
;	(goto-char (point-min))
;	; move around and do cool stuff...
;	; ...
;	(message "other stuff...")))
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
	- Submit Time Card
"))))


(defun reinterpret-hex ()
  "Show ASCII hex values in plain ASCII in another buffer."
  (interactive)
  (setq buf (get-buffer-create "*reinterpret-hex*"))
  (save-current-buffer ; clear the temp buffer
	(set-buffer buf)
	(erase-buffer))
  (save-excursion
	(let ((end (if mark-active (region-end) (point-max))))
	  (goto-char (if mark-active (region-beginning) (point-min)))
	  (while (search-forward-regexp "\\([0-9a-fA-F]\\{2\\}\\)" end t)
		(let ((m1 (match-string 1)))
		  (save-current-buffer
			(set-buffer buf)
			(insert (format "%s: %c\n" m1 (string-to-number m1 16)))))))
	(display-buffer buf)))

(provide 'funcs)

;;; funcs.el ends here
