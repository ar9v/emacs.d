(setq custom-file (locate-user-emacs-file "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))


(setq use-short-answers t)

(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(load-theme 'modus-vivendi)

(use-package magit
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (conf-unix-mode . rainbow-mode))
