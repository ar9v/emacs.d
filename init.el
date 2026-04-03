;; Gets us stuff from
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Advanced-Mark-Commands.html
;;
;; E.g. `* .` (`dired-mark-extension`)
(require 'package)
(require 'dired-x)

;; Useful for debugging
;; (setq debug-on-error t)

(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)

(defconst NOTES-DIR "~/Documents/notes/"
  "The directory where my notes live.")

(defconst SKELETON-SIGIL "\"_\""
  "A sigil inserted by skeletons, which makes jumping around interest points easy.")

(defun argv/goto-note ()
  "Open a file in ~/Documents/notes."
  (interactive)

  (argv/find-file-in NOTES-DIR))

(defun argv/goto-init ()
  "Open init.el."
  (interactive)

  (find-file user-init-file))

(defun argv/load-project-setup ()
  "Loads a `project-setup.el' file in the project.

This is intended to be used with `C-x p p'. If the project has a `project-setup.el' file,
it runs it."

  (interactive)

  (let* ((project-dir (project-root (project-current)))
         (setup-filename (concat project-dir "project-setup.el")))
    (if (file-exists-p setup-filename)
        (load-file setup-filename)
      (message "No project-setup.el found in %s. Nothing to do." project-dir))))

(defun argv/next-skeleton-sigil ()
  "search-forward to the next SKELETON-SIGIL, or wrap around."
  (interactive)

  (or (search-forward SKELETON-SIGIL nil t)
      (progn (goto-char (point-min)) (search-forward SKELETON-SIGIL))))

(defun argv/previous-skeleton-sigil ()
  "search-backward to the next SKELETON-SIGIL, or wrap around."
  (interactive)

  (or (search-backward SKELETON-SIGIL nil t)
      (progn (goto-char (point-max)) (search-backward SKELETON-SIGIL))))

(defun argv/find-file-in (dir)
  "Jump to a file in directory `DIR'."
  (let* ((file (completing-read
                "File: "
                (directory-files dir nil))))

    (find-file (concat dir file))))

(use-package emacs
  :bind
  (("C-x C-S-f" . recentf-open)
   ("C-x C-b"   . ibuffer)
   :map prog-mode-map
   ("C-$" . argv/next-skeleton-sigil)
   ("M-$" . argv/previous-skeleton-sigil))

  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  :config
  (setq custom-file (make-temp-file "emacs-custom-"))

  ;; Aesthetics
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (load-theme 'doric-obsidian :no-confirm)))

  (set-scroll-bar-mode nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (set-face-attribute
   'default
   nil
   :height 140
   :family "Iosevka Nerd Font")

  (set-face-attribute
   'fixed-pitch
   nil
   :height 120
   :family "Iosevka Mono")

  (setq use-short-answers t)
  (setq-default indent-tabs-mode nil)

  (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

  ;; Keybindings
  ;; TODO: consider windmove(?)
  (keymap-global-set "M-o" 'other-window)
  (keymap-global-set "M-i" 'imenu)

  (setq isearch-allow-motion t)

  ;;;; Hippie-expand
  (keymap-global-set "M-/" 'hippie-expand)

  ;; Backups
  (setq make-backup-files nil)
  (auto-save-mode -1))

(use-package doric-themes
  :ensure t)

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode 1))

(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package executable
  :defer t
  :config
  (setq executable-prefix-env t))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package magit)

(use-package conf-mode
  :defer t
  :bind (("C-," . backward-page)
         ("C-." . forward-page)))

(use-package rainbow-mode
  :defer t
  :hook (conf-mode . rainbow-mode)
  :config
  ;; Just turning `rainbow-mode' on gets you hex colors, but not
  ;; rgb(a) colors. Adding the conf modes to this list gets us
  ;; all `rainbow-mode' colorizing.
  (dolist (mode '(conf-mode conf-space-mode conf-unix-mode conf-colon-mode))
    (push mode rainbow-html-colors-major-mode-list)))

(use-package json
  :defer t
  :custom
  (js-indent-level 2))

(use-package yaml-mode)

(use-package markdown-mode
  :defer t)

(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;; Ruby
(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package yari
  :defer t)

(use-package ruby-compilation
  :after (ruby-mode)
  :bind (:map ruby-mode-map
              ("C-x t" . nil)
              ("C-c m c" . ruby-compilation-this-test)
              ("C-c m C" . ruby-compilation-this-buffer)))

(use-package bundler)
(use-package rspec-mode)
(use-package minitest-mode)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package paredit
  :hook (racket-mode emacs-lisp-mode))

(use-package scribble
  :load-path "site-lisp/scribble")

(use-package racket-mode
  :init (require 'racket-xp)
  :hook (racket-mode . racket-xp-mode)
  :defer t)

(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

(use-package corfu
  :init (global-corfu-mode))

(use-package project
  :defer t
  :config
  (add-to-list 'project-switch-commands
               '(argv/load-project-setup "Load setup.el" "l")
               t))

(use-package yasnippet
  :defer t
  :init (yas-global-mode 1))

(use-package org
  :defer t
  :config
  (plist-put org-format-latex-options :scale 1.5))

;; TODO: make this not suck
(defun dupe-sexp ()
  (interactive)

  (mark-sexp)
  (copy-region-as-kill (region-beginning) (region-end))
  (forward-sexp)
  (newline-and-indent)
  (yank)
  (backward-sexp))
