;; proxy for loading emacs.el

(defconst at-work t)

(setq package-enable-at-startup nil) (package-initialize)

(load "~/.emacs.d/lisp/emacs.el")
