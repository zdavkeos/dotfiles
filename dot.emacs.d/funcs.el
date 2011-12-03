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
;; Copied from:
; http://www.johndcook.com/blog/2010/08/05/emacs-command-to-add-html-tags/
;; surround a word or region with a tag (html)
(defun tag-word-or-region (tag)
    "Surround current word or region with a given tag."
    (interactive "sEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" tag ">"))
        (if (and transient-mark-mode mark-active)
            (progn
                (goto-char (region-end))
                (insert end-tag)
                (goto-char (region-beginning))
                (insert start-tag))
            (progn
                (setq bds (bounds-of-thing-at-point 'symbol))
                (goto-char (cdr bds))
                (insert end-tag)
                 (goto-char (car bds))
                 (insert start-tag)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
