;; .emacs file
;; Zach Davis
;;  Created: 06-16-2010


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Most of the stuff in this file is taken from:
;;   -Steve Yegge's various blogs
;;   -emacs-fu.blogspot.com blog
;;   -emacswiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; name and email -
(setq user-full-name "Zach Davis")
(if at-work
    (setq user-mail-address "Zach.Davis@osii.com")
    (setq user-mail-address "zdavkeos@gmail.com"))

;; local elisp files path
(setq load-path (append (list nil "~/.emacs.d") load-path))

;; load my set of handy functions
(load "~/.emacs.d/funcs.el")

;; shortcut to open .emacs (emacs-fu)
(global-set-key (kbd "C-c E")
  (lambda()(interactive)(find-file "~/.emacs.d/emacs.el")))

(global-set-key [f1] 'signature)
(global-set-key [f2] 'datestamp)

;; shortcut to TODO file
(global-set-key [f3]
  (lambda ()
	(interactive)
	(find-file (if at-work
				   "u:/notes/TODO.org"
				   "~/Dropbox/notes/TODO.org"))))

;; set default tab width
(setq-default tab-width 4)
;; use spaces instead of tabs
(if at-work
    (setq indent-line-function 'insert-tab) ; *hard tabs*
    (setq-default indent-tabs-mode nil) ; *soft tabs*
)
;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
;              64 68 72 76 80 84 88 92 96 100 104 108 112
;              116 120))

;; highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 1)

;; disable the toolbar
(tool-bar-mode -1)

;; put backup files in a specific directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

;; disable autosaves
(setq auto-save-default nil)

;; enable delete selection mode
(delete-selection-mode 1)

;; disable copy-on-select
(setq mouse-drag-copy-region nil)

;; smoother scrolling - still needed in v24?
;(setq scroll-conservatively 8)
;(setq scroll-margin 1)

;; CUA mode settings
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; set so comments over multiple lines, line up
(setq comment-style 'multi-line)

;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; show column number - slow?
(column-number-mode 1)

;; change how diff's are displayed
(setq diff-switches "") ; unified diff
(add-hook 'diff-mode-hook
		  (lambda ()
			(local-set-key [M-down] 'diff-hunk-next)
			(local-set-key [M-up] 'diff-hunk-prev)))

;; remember the last line you were on when you quit
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; Automatically reload files after they've been modified 
(global-auto-revert-mode 1)

;; collection of c-mode hooks
(add-hook 'c-mode-common-hook
  (lambda()
    ;(local-set-key  (kbd "C-c o") 'ff-find-other-file) ;find implementation in .c from .h
	(imenu-add-menubar-index) ; add an index
    (local-set-key (kbd "C-c C-v") 'compile) ; make C^c C^v 'compile' in c-mode
    (local-set-key (kbd "C-c C-f") 'mark-defun) ; make C^c C^f mark current function
    ; add highlights to TODO, etc. in c/c++/java files
    (font-lock-add-keywords nil
                '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))
				'((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))))
    (flyspell-prog-mode) ; spell checking of comments
	;; c style indentation stuff
	(setq c-default-style "linux")
	(setq c-basic-offset 4)
	(add-to-list 'c-offsets-alist '(substatement-open . 0))
	(add-to-list 'c-offsets-alist '(access-label . /))
	(if at-work
		(add-to-list 'c-offsets-alist '(case-label . 0) t)
	  (add-to-list 'c-offsets-alist '(case-label . 4) t))))


;; couple of c++-mode hooks
(add-hook 'c++-mode-hook
  (lambda()
    ;(set (make-local-variable 'compile-command) "g++ -Wall -g -o ")
    (local-set-key  (kbd "C-c C-v") 'compile) ; make C^c C^v 'compile' in c-mode
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
    (flyspell-prog-mode))) ; spell checking of comments

;; couple of java-mode hooks
(add-hook 'java-mode-hook
  (lambda()
    (set (make-local-variable 'compile-command) "javac ")))

;; python related stuff
(add-to-list 'auto-mode-alist '("\\.ipy\\'" . python))
(add-hook 'python-mode-hook
  (lambda ()
	(setq python-indent-offset 4)
	(setq tab-width 4)
    (local-set-key (kbd "C-c C-c") 'comment-region)))

;; org mode hooks
(add-hook 'org-mode-hook
		  (lambda ()
			(local-set-key (kbd "M-n") 'outline-next-visible-heading)
			(local-set-key (kbd "M-p") 'outline-prev-visible-heading)
			;(setq org-indent-mode t)
			(setq org-hide-leading-stars nil)))


;; disable the startup screen
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

;; make page up/down just move the screen a few lines
(global-set-key [prior] '(lambda () (interactive)(cua-scroll-down 3)))
(global-set-key [next]  '(lambda () (interactive)(cua-scroll-up 3)))

;; make C-] => M-x
(global-set-key (kbd "C-]") 'execute-extended-command)

;; record macro
(global-set-key [f10]  'start-kbd-macro)
(global-set-key [f11]  'end-kbd-macro)
(global-set-key [f12]  'call-last-kbd-macro)

;; make alt-F4 close emacs
(global-set-key [M-f4] 'save-buffers-kill-terminal)

;; disable C-x f, gets me every time
(global-unset-key "\C-xf")

;; kill to beginning of line
(global-set-key (kbd "\C-c\C-k") '(lambda () (kill-line -1)))

;; change C-x C-o from delete-blank lines
;; to fixup-whitespace
(global-set-key (kbd "\C-x\C-o") 'fixup-whitespace)

;; change ESC key to be like C-g (stackoverflow)
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; change format of title bar
(setq frame-title-format '(buffer-file-name "     %f" ("   %b")))

;; change M^s to regex search
(global-set-key (kbd "M-s") 'isearch-forward-regexp)

;; add rectangle command string-insert-rectangle
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; change C-z to undo
(global-set-key (kbd "C-z") 'undo)

;; duplicate line M-RET
(global-set-key (kbd "M-RET") 'duplicate-line)

;; make alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; make line wrapping shortcut
(defalias 'wrapping 'toggle-truncate-lines)

;; make shortcut for whitespace-mode
(global-set-key [f5] 'whitespace-mode)

;; some handy tags-related stuff
;; - Navigate forward) and backward( in tag search
;;   M-* to quit search and return to start
(global-set-key (kbd "M-)") '(lambda() (interactive)(find-tag nil t)))
(global-set-key (kbd "M-(") '(lambda() (interactive)(find-tag nil '-)))

;; goto start of file, end of file
(global-set-key (kbd "C-<") (lambda() (interactive)(goto-char (point-min))))
(global-set-key (kbd "C->") (lambda() (interactive)(goto-char (point-max))))

;; parenthesis matching
(global-set-key (kbd "C-}") 'forward-sexp)
(global-set-key (kbd "C-{") 'backward-sexp)

;; backward-kill-sexp can be pretty handy
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; kill from cursor to beginning of line
(global-set-key (kbd "M-<backspace>") 'backward-kill-line)

;; add support for common extensions
(add-to-list 'auto-mode-alist '("README"        . text-mode))
(add-to-list 'auto-mode-alist '("\\.cu$"        . c-mode))
(add-to-list 'auto-mode-alist '("\\.glsl$"      . c-mode))
(add-to-list 'auto-mode-alist '("\\.stp$"       . c-mode))
(add-to-list 'auto-mode-alist '("makefile.win$" . makefile-mode))

;; Ps Print settings
;; letter paper, 10 pt, no header
(setq ps-paper-type 'letter
	  ps-font-size 10.0
	  ps-print-header nil
	  ps-landscape-mode nil
	  ps-number-of-columns 1
	  ps-spool-duplex t)

;; fancy things up a bit
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Set the window size (if running in a window)
(if (window-system)
	(progn
	  (add-to-list 'default-frame-alist (cons 'width 110))
	  (add-to-list 'default-frame-alist (cons 'height 45))))


;; Helper for compilation. Close the compilation window if
;; there were no errors. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;;;;;;;;;;;;;;;;;;;
;; ido-mode (interactively do things)
;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-enable-regexp t)
(setq ido-enable-last-directory-history nil)
(setq ido-record-commands nil)
(setq ido-max-work-directory-list 0)
(setq ido-max-work-file-list 0)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; nxml mode cutomizations
;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
  (lambda ()
	(setq nxml-child-indent 4)
    (setq nxml-sexp-element-flag t)))

;;;;;;;;;;;;;;;;;;;;
;; asm mode hooks
;;;;;;;;;;;;;;;;;;;;
(add-hook 'asm-mode-hook
  (lambda ()
    (setq comment-column 25)))

(require 'which-func)
(set-face-attribute 'which-func nil :foreground "light cyan")
(which-function-mode t)

;;;;;;;;;;;;;;;;;;;;;;; Stuff that requires dependencies ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; javascript major mode using Steve Yegge's js2.el
;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2" "JavaScript Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$"   . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;(add-to-list 'auto-mode-alist '("\\.geojson$" . js2-mode)) ; also get geojson files


;;;;;;;;;;;;;;;;;;;;
;; highlighting for dos batch files
;;;;;;;;;;;;;;;;;;;;
(autoload 'bat-mode "dosbat" "" t nil)
(add-to-list 'auto-mode-alist '("\\.bat$" . bat-mode))

;;;;;;;;;;;;;;;;;;;;
;; highlighting for osi files
;;;;;;;;;;;;;;;;;;;;
(if at-work
	(progn
	  (require 'osikey)
	  (autoload 'rel-mode  "relmode" "" t nil)
	  (autoload 'skm-mode  "skmmode" "" t nil)
	  (autoload 'rc-mode   "rcmode"  "" t nil)
	  (autoload 'dat-mode  "datmode"  "" t nil)
	  (autoload 'prod-mode "prodmode"  "" t nil)
	  (autoload 'flt-mode  "fltmode" "" t nil)
	  (autoload 'compile-req-mode  "compile-req-mode"  "" t nil)
	  (add-to-list 'auto-mode-alist '("\\.rel$" . rel-mode))
	  (add-to-list 'auto-mode-alist '("\\.skm$" . skm-mode))
	  (add-to-list 'auto-mode-alist '("\\.rc$"  . rc-mode))
	  (add-to-list 'auto-mode-alist '("\\.DAT$" . dat-mode))
	  (add-to-list 'auto-mode-alist '("\\.atd$" . nxml-mode))
	  (add-to-list 'auto-mode-alist '("\\.atr$" . nxml-mode))
	  (add-to-list 'auto-mode-alist '("\\.olf$" . nxml-mode))
	  (add-to-list 'auto-mode-alist '("products.*\\.txt.*$" . prod-mode))
	  (add-to-list 'auto-mode-alist '("compile_request\\.txt$" . compile-req-mode))
	  (add-to-list 'auto-mode-alist '("\\.flt$" . flt-mode))
	  (add-hook 'imenu-after-jump-hook (lambda () (recenter 'top)))))

;;;;;;;;;;;;;;;;;;;;
;; change default font
;;;;;;;;;;;;;;;;;;;;
(if (eq system-type 'windows-nt)
	(set-face-attribute 'default nil :font "Consolas 10")
  (set-face-attribute 'default nil :font "Inconsolata 12"))

;(set-face-attribute 'default nil :font "Droid Sans Mono 10")
;(set-face-attribute 'default nil :font "DejaVu Sans Mono 10")
;(set-face-attribute 'default nil :font "Source Code Pro 10")
;(set-face-attribute 'default nil :font "Verdana 10")
;(set-face-attribute 'default nil :font "Anonymous Pro 12")

;;;;;;;;;;;;;;;;;;;;
;; tex/latex stuff
;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook
    (lambda()
      (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
      (auto-fill-mode 1)
      (setq TeX-command-default "XeLaTeX")
	  (setq TeX-auto-save t)
	  (setq TeX-parse-self t)
	  (setq TeX-save-query nil)
	  (setq TeX-PDF-mode t) ; auctex, set pdf as the default
      (flyspell-mode t)))

;; dependent stuff dependent on OS
(if (eq system-type 'windows-nt)
    (progn
      ;; set aspell path
      (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
      ;; set pdf output to preview in Adobe Reader
      '(TeX-output-view-style (quote (
                ("^pdf$" "." "C:/Program Files (x86)/Adobe/Reader 10.0/Reader/AcroRd32.exe"))))
	  ;; fix path to find important utility programs
	  (setenv "PATH"
			  (concat
			   ;; Change this with your path to MSYS bin directory
			   "C:\\cygwin\\bin;"
			   (getenv "PATH"))))
    (progn
     ;; setup spell-checker
     (setq ispell-program-name "aspell" ispell-extra-args '("--sug-mode=ultra"))))

;;;;;;;;;;;;;;;;;;;;
;; color-theme package config - should update for v24
;;;;;;;;;;;;;;;;;;;;
(require 'color-theme-zenburn)
(color-theme-zenburn)

;;;;;;;;;;;;;;;;;;;;
;; Doxymacs package
;;;;;;;;;;;;;;;;;;;;
;(require 'doxymacs)
;(setq doxymacs-use-external-xml-parser nil)
;(add-hook 'font-lock-mode-hook
;          '(lambda ()
;             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;                 (doxymacs-font-lock))))

;;;;;;;;;;;;;;;;;;;;
;; multi-term package
;;;;;;;;;;;;;;;;;;;;
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(if (eq system-type 'windows-nt)
	(setq multi-term-program "C:/cygwin/bin/bash.exe")
  (setq multi-term-program "/bin/zsh"))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; ascii table
;;;;;;;;;;;;;;;;;;;;
(autoload 'ascii-table "ascii-table" "Show ASCII table." t)

;;;;;;;;;;;;;;;;;;;;
;; Deft mode
;;;;;;;;;;;;;;;;;;;;
;(require 'deft)
(autoload 'deft "deft.el" "Deft Mode" t)
(setq deft-extension "org")
(if at-work
    (setq deft-directory "U:/notes/")
    (setq deft-directory "~/Dropbox/notes/"))
(setq deft-text-mode 'org-mode)
(setq deft-strip-title-regexp "#\\+TITLE: ")
(setq deft-strip-summary-regexp "#\\+DESCRIPTION: ")
(setq deft-auto-save-interval 15.0)
(global-set-key [f4] 'deft)

;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" .  markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" .  markdown-mode))


;;;;;;;;;;;;;;;;;;;;
;; C# mode
;;;;;;;;;;;;;;;;;;;;
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;;;;;;;;;;;;;;;;;;;;
;; Haskell mode
;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook (lambda () 
							   (local-set-key (kbd "C-c C-c") 'comment-region)))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(setq haskell-program-name "C:/hp/bin/ghci.exe")
(setq haskell-check-command "C:/Users/zdavis/AppData/Roaming/cabal/bin/hlint.exe")

;; M-x typing-of-emacs
(require 'typing)

;; htmlize package
(require 'htmlize)

;; json file editing
(require 'json-mode)
(add-hook 'json-mode-hook (lambda () (wrapping)))
(autoload 'json-mode "json-mode" "Major mode for editing JSON files")

;; turtle file edting
(autoload 'turtle-mode "turtlemode" "" t nil)

;; chicken scheme
;(autoload 'chicken "chicken" "Chicken Scheme mode." t)
(require 'chicken)
(setq scheme-program-name "C:/cygwin/usr/local/bin/csi -:c")
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

;; This snippet enables lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
