;; -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package catppuccin-theme
  :ensure t
  :demand t
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'latte)
  (catppuccin-reload))

(provide 'my-theme)
