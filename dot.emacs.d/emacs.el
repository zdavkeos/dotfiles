;; .emacs file
;; Zach Davis
;;  06-16-2010


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Most of the stuff in this file is taken from:
;;   -Steve Yegge's various blogs
;;   -emacs-fu.blogspot.com blog
;;   -emacswiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; name and email -
(setq user-full-name "Zach Davis")
(setq user-mail-address "zdavkeos@gmail.com")

;; local elisp files path
(setq load-path (append (list nil "~/.emacs.d") load-path))

;; load my set of handy functions
(load "~/.emacs.d/funcs.el")

;; disable the toolbar
(tool-bar-mode -1)

;; shortcut to open .emacs (emacs-fu)
(global-set-key (kbd "C-c E") ;; .emacs
  (lambda()(interactive)(find-file "~/.emacs.d/emacs.el")))

;; set default tab width
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil) ; *hard tabs* uncomment to use soft tabs
(setq indent-line-function 'insert-tab)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
		      64 68 72 76 80 84 88 92 96 100 104 108 112
		      116 120))

;; highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 1)

;; put backup files in a specific directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

;; disable autosaves
(setq auto-save-default nil)

;; enable delete selection mode
(delete-selection-mode 1)

;; disable copy-on-select
(setq mouse-drag-copy-region nil)

;; smoother scrolling
(setq scroll-conservatively 8)
(setq scroll-margin 1)

;; CUA mode settings
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; set so comments over multiple lines, line up
;(setq comment-style 'indent)
;(setq comment-style 'aligned)
(setq comment-style 'multi-line)

;; Make all yes-or-no questions as y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; show column number
(column-number-mode 1)

;; change how diff's are displayed/navigated
(setq diff-switches "") ;unified diff
(add-hook 'diff-mode-hook
    (lambda ()
        (local-set-key [M-down] 'diff-hunk-next)
        (local-set-key [M-up] 'diff-hunk-prev)
    )
)

;; make page up/down just move the screen a few lines
(global-set-key [prior] '(lambda () (interactive)(cua-scroll-down 3)))
(global-set-key [next]  '(lambda () (interactive)(cua-scroll-up 3)))

;; toggle whitespace-mode with F5
(global-set-key (kbd "<f5>") 'whitespace-mode)

;; make C-[ => M-x
(global-set-key (kbd "C-]") 'execute-extended-command)

;; remember the last line you were on when you quit
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Automatically reload files after they've been modified 
(global-auto-revert-mode 1)

;; c style and indentation
(setq c-default-style "linux")
(setq c-basic-offset 4)
(setq c-offsets-alist '((substatement-open . 0)
                        (case-label . 4)
			(access-label . /)))

;; collection of c-mode hooks
(add-hook 'c-mode-common-hook
  (lambda()
    (which-function-mode t) ; add current function name to status bar
    (local-set-key (kbd "C-c C-v") 'compile) ; make C^c C^v 'compile' in c-mode
    ; add highlights to TODO, etc. in c/c++/java files
    (font-lock-add-keywords nil
			    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
    ;change default compile-command
    ;(set (make-local-variable 'compile-command) "gcc -Wall -g -o")
    (flyspell-prog-mode) ; spell checking of comments
    ))

;; couple of c++-mode hooks
(add-hook 'c++-mode-hook
  (lambda()
    (set (make-local-variable 'compile-command) "g++ -Wall -g -o ")
    (local-set-key  (kbd "C-c C-v") 'compile) ; make C^c C^v 'compile' in c-mode
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
    (flyspell-prog-mode) ; spell checking of comments
	(which-function-mode t)
    ))

;; python related stuff
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.ipy\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-c") 'comment-region)
	(which-function-mode t)
  )
)
(autoload 'python-mode "python-mode" "Python Mode." t)

;; org mode hooks
(add-hook 'org-mode-hook
    (lambda ()
        (local-set-key (kbd "M-n") 'outline-next-visible-heading)
        (local-set-key (kbd "M-p") 'outline-prev-visible-heading)
        ;(setq org-indent-mode t)
        (setq org-hide-leading-stars nil)
    )
)

;; record macro
(global-set-key [f10]  'start-kbd-macro)
(global-set-key [f11]  'end-kbd-macro)
(global-set-key [f12]  'call-last-kbd-macro)

;; make alt-F4 close emacs
(global-set-key [M-f4] 'save-buffers-kill-terminal)

;; disable C-x f, gets me every time
(global-unset-key "\C-xf")

;; chang ESC key to be like C-g (stackoverflow)
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; change format of title bar
(setq frame-title-format '(buffer-file-name "   %f" ("   %b")))

;; change M^s to regex search
(global-set-key "\M-s" 'isearch-forward-regexp)

;; add rectangle command string-insert-rectangle
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; change C-z to undo
(global-set-key (kbd "C-z") 'undo)

;; duplicate line M-RET
(global-set-key (kbd "M-RET") 'duplicate-line)

;; make alias for query-replace-regex
(defalias 'qrr 'query-replace-regexp)

;; make shortcut for whitespace-mode
(global-set-key [f5] 'whitespace-mode)

;;add support for common file extensions
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.glsl$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.stp$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bat$" . bat-mode) auto-mode-alist))

;; fancy things up a bit
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(modify-frame-parameters nil '((wait-for-wm . nil)))


;; add 'insert template?' on file-open hook
(add-hook 'find-file-hook 'insert-template)


;; Helper for compilation. Close the compilation window if
;; there were no errors. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;; OS dependant stuff
(if (eq system-type 'windows-nt)
    (progn
      '(inferior-lisp-program nil)
    )
    (progn
      '(inferior-lisp-program "clisp")
    )
)


;; ido-mode (interactively do things)
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

;; some handy tags-related stuff
;; - Navigate forward) and backward( in tag search
;;   M-* to quit search and return to start
(global-set-key (kbd "M-)") '(lambda() (interactive)(find-tag nil t)))
(global-set-key (kbd "M-(") '(lambda() (interactive)(find-tag nil '-)))

;; goto start of file, end of file
(global-set-key (kbd "C-<") (lambda() (interactive)(goto-char (point-min))))
(global-set-key (kbd "C->") (lambda() (interactive)(goto-char (point-max))))

;; backward-kill-sexp can be pretty handy
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; nxml mode cutomizations
(add-hook 'nxml-mode-hook
  (lambda ()
	(setq nxml-child-indent 4)
    (setq nxml-sexp-element-flag t)
))

;;;;;;;;;;;;;;;;;;;;;;; Stuff that requires dependencies ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable line numbers: linum.el
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html
(require 'linum)
(global-set-key [f6] 'linum-mode)

;;javascript major mode using Steve Yegge's js2.el
(autoload 'js2-mode "js2" "JavaScript Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; highlighting for dos batch files
(autoload 'bat-mode "dosbat" "" t nil)

;; change default font
(set-face-attribute 'default nil :font "Inconsolata 12")
;(set-face-attribute 'default nil :font "Droid Sans Mono 10")
;(set-face-attribute 'default nil :font "DejaVu Sans Mono 10")
;(set-face-attribute 'default nil :font "Consolas 10")
;(set-face-attribute 'default nil :font "Anonymous Pro 12")


;; tex/latex stuff
;(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t) ; auctex, set pdf as the default
;;add XeLaTeX
(add-hook 'LaTeX-mode-hook
    (lambda()
      (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
      (auto-fill-mode 1)
      (setq TeX-command-default "XeLaTeX")
      (flyspell-mode t)
    )
)

;; dependent stuff dependent on OS
(if (eq system-type 'windows-nt)
    (progn
      ;; set aspell path
      (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
      ;; set pdf output to preview in evince, html in firefox
      '(TeX-output-view-style (quote (
				 ("^pdf$" "." "C:/Program Files (x86)/Adobe/Reader 10.0/Reader/AcroRd32.exe")
				 ("^html?$" "." "firefox %o")
				 )))
      )
    (progn
     ;; setup spell-checker
     (setq ispell-program-name "aspell" ispell-extra-args '("--sug-mode=ultra"))
     ;; set pdf output to preview in evince, html in firefox
     '(TeX-output-view-style (quote (
				 ("^pdf$" "." "evince %o")
				 ("^html?$" "." "firefox %o")
				 )))
     )
)

;; color-theme package config
(require 'color-theme-zenburn)
(color-theme-zenburn)


;; Doxymacs package
(require 'doxymacs)
(setq doxymacs-use-external-xml-parser t)
;(autoload 'doxymacs-mode "doxymacs" "Deal with doxygen." t)
(add-hook 'font-lock-mode-hook
          '(lambda ()
             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                 (doxymacs-font-lock))))


;; Markdown syntax highlighting
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; C# stuff
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; multi-term package
;;;;;;;;;;;;;;;;;;;;
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

;(setq multi-term-program "/bin/bash")   ;; use bash
(setq multi-term-program "/bin/zsh")   ;; use zsh
;(setq multi-term-program "C:/cygwin/bin/bash.exe --login -i")   ;; use cygwin

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one
;;;;;;;;;;;;;;;;;;;;

;; ascii table
;;;;;;;;;;;;;;;;;;;;
(autoload 'ascii-table "ascii-table" "Show ASCII table." t)

;;;;;;;;;;;;;;;;;;;;
;; Deft mode:
;;  http://jblevins.org/projects/deft
;;;;;;;;;;;;;;;;;;;;
(setq deft-extension "org")
(setq deft-directory "~/Dropbox/notes")
(setq deft-text-mode 'org-mode)
(setq deft-auto-save-interval 15.0)
(global-set-key [f4] 'deft)
(require 'deft)
