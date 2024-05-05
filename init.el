(use-package emacs
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

  ;; Modus themes
  (require-theme 'modus-themes)

  (setq modus-themes-vivendi-color-overrides
	'((bg-main . "#111111")))

  (load-theme 'modus-vivendi))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package magit
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (conf-unix-mode . rainbow-mode))

(use-package json
  :custom
  (js-indent-level 2))
