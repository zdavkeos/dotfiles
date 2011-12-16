;; -*- mode: emacs-lisp-mode; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Collection of handy functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dos2unix and unix2dos converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unix2dos ()
  "set encoding to dos line endings"
  (interactive)
  (message "Converting line endings to dos(CRLF). **Save buffer to commit")
  (setq buffer-file-coding-system 'undecided-dos)
  (not-modified t)
)

(defun dos2unix ()
  "replace CR's with nothing and set encoding to unix"
  (interactive)
  (message "Converting line endings to unix (LF). **Save buffer to commit")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\x0d" nil t)
        (replace-match "")
	)
  )
  (setq buffer-file-coding-system 'undecided-unix)
  (not-modified t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  prefix-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-region (start end)
  "Prefix region with supplied string"
  (interactive "r")
  (setq str (read-from-minibuffer "Enter prefix string: "))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (goto-char (line-beginning-position))
      (insert str)
      (forward-line 1)
      )
    )
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
	          (insert-file (concat template-files-location template)))
	      ))
	   ))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rm-wsp - extraneous whitespace remover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rm-wsp ()
  "remove whitespace from blank lines, and ends of lines"
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
  "Spit out my name and email"
  (interactive)
  (insert (concat "~" user-full-name " - " user-mail-address)))
(global-set-key [f1]  'signature)

(defun datestamp ()
  "Print out the current date mm-dd-yyyy"
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))
(global-set-key [f2]  'datestamp)

(defun datestamp-fancy ()
  "Print out the current date: month day, year"
  (interactive)
  (insert (format-time-string "%b %d, %Y")))

(defun timestamp ()
  "Print out the current date mm-dd-yyyy : hh:mm:ss"
  (interactive)
  (insert (format-time-string "%m-%d-%Y : %T")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; C-w, and M-w now kill whole line if no region is selected
;; copied shamelessly from emacs-fu.blogspot.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
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
;; Amended comment-region
;;  if no region is selected, comment line instead
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice comment-region (before comment-line-if-no-region activate compile)
  "comment region, or line if no region"
  (interactive (if mark-active (list (region-beginning) (region-end))
		               (list (line-beginning-position) (line-beginning-position 2))))
)

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
  (lambda () (comment-dwim))
)


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
  "Select a region to compare"
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
  "Compare current region with region already selected by `diff-region`"
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
;; If a region is selected, copy it and place it
;; on a new line below the current one.  If no
;; region is selected, copy the current line and
;; paste a copy on a new line below the current
;; one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duplicate-line ()
  " duplicate the current line "
  (interactive)
  (save-excursion
	(if mark-active
		(kill-ring-save (region-beginning) (region-end))
	    (kill-ring-save (line-beginning-position) (line-beginning-position 2))
    )
	(goto-char (line-beginning-position 2)) ; goto the start of the next line
	(yank)
   )
  (next-line)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
