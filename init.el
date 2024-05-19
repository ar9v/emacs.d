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
