;; Gets us stuff from
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Advanced-Mark-Commands.html
;;
;; E.g. `* .` (`dired-mark-extension`)
(require 'dired-x)


(defconst NOTES-DIR "~/Documents/notes/"
  "The directory where my notes live.")

(defun argv/goto-note ()
  "Open a file in ~/Documents/notes."
  (interactive)

  (argv/find-file-in NOTES-DIR))

(defun argv/goto-init ()
  "Open init.el."
  (interactive)

  (find-file user-init-file))


(defun argv/find-file-in (dir)
  "Jump to a file in directory `DIR'."
  (let* ((file (completing-read
                "File: "
                (directory-files dir nil))))

    (find-file (concat dir file))))

(use-package emacs
  :bind
  (("C-x C-S-f" . recentf-open))
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
  ;; Custom interface config
  (setq custom-file (locate-user-emacs-file "custom.el"))

  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Aesthetics
  (setq use-short-answers t)

  (set-scroll-bar-mode nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (setq-default indent-tabs-mode nil)

  (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

  ;;;; Modus themes
  (require-theme 'modus-themes)

  (setq modus-themes-vivendi-color-overrides
        '((bg-main . "#111111")))

  (load-theme 'modus-vivendi)

  ;; Keybindings
  ;; TODO: consider windmove(?)
  (keymap-global-set "M-o" 'other-window)
  (keymap-global-set "M-i" 'imenu)

  (global-display-line-numbers-mode 1)
  (column-number-mode 1)

  (setq isearch-allow-motion t)

  ;;;; Hippie-expand
  (keymap-global-set "M-/" 'hippie-expand))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package magit
  :ensure t)

(use-package conf-mode
  :bind (("C-," . backward-page)
         ("C-." . forward-page)))

(use-package rainbow-mode
  :ensure t
  :hook (conf-mode . rainbow-mode)
  :config
  ;; Just turning `rainbow-mode' on gets you hex colors, but not
  ;; rgb(a) colors. Adding the conf modes to this list gets us
  ;; all `rainbow-mode' colorizing.
  (dolist (mode '(conf-mode conf-space-mode conf-unix-mode conf-colon-mode))
    (push mode rainbow-html-colors-major-mode-list)))

(use-package json
  :custom
  (js-indent-level 2))

(use-package markdown-mode
  :ensure t)

(use-package inf-ruby
  :ensure t)

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package scribble
  :load-path "site-lisp/scribble")
