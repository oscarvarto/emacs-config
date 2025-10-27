;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq make-backup-files nil) ; stop creating ~ files

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Projectile settings
  (setq projectile-require-project-root nil)
  (setq projectile-globally-ignored-directories
        (list (getenv "HOME")))
  ; Set garbage collection threshold higher (e.g., 100MB) for potentially smoother performance
  (setq gc-cons-threshold (* 100 1024 1024))
  ; Disable projectile caching (can help if stale or slow)
  (setq projectile-enable-caching nil)
  ; Explicitly use default completion for projectile
  (setq projectile-completion-system 'default))

;; Automatic tree-sitter grammar installation and mode activation
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (global-treesit-auto-mode))

;; Use built-in tree-sitter modes (Emacs 29+)
;; Remap existing modes to their tree-sitter equivalents
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (java-mode . java-ts-mode)
        (rust-mode . rust-ts-mode)
        (css-mode . css-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        (clojure-mode . clojure-ts-mode)))

(setq-default buffer-file-coding-system 'utf-8-unix) ; Default file encoding

;; Enable line numbers only for specific modes
(global-display-line-numbers-mode 0)  ;; Turn off global line numbers

;; Define a hook to enable line numbers for desired modes

(defun enable-line-numbers-for-desired-modes ()
  "Enable line numbers for programming and text editing modes."
  (display-line-numbers-mode 1))

;; Add the hook to programming modes
(add-hook 'prog-mode-hook 'enable-line-numbers-for-desired-modes)

;; Add the hook to specific text modes
(add-hook 'org-mode-hook 'enable-line-numbers-for-desired-modes)
(add-hook 'markdown-mode-hook 'enable-line-numbers-for-desired-modes)
(add-hook 'text-mode-hook 'enable-line-numbers-for-desired-modes)
(add-hook 'conf-mode-hook 'enable-line-numbers-for-desired-modes)

;; Explicitly disable line numbers for specific modes where they're not wanted
(dolist (mode '(treemacs-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(menu-bar-mode t)
(tool-bar-mode -1) ; Disable the graphical toolbar
(winner-mode 1) ; Enable window configuration history (undo/redo window changes)

(use-package doom-modeline
  :ensure t
  :demand t
  :init (doom-modeline-mode 1))

;; Magit configuration (dependencies are loaded in my-elpaca.el)
(use-package magit
  :ensure t
  :demand t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eglot-booster
	:ensure (:host github :repo "jdtsmith/eglot-booster" :depth 1)
	:after eglot
	:config (eglot-booster-mode))

(provide 'my-defaults)
