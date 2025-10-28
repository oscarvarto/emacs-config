;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Set shell to use Nix-managed zsh from ~/.nix-profile
;; This ensures subprocesses have access to git and other Nix-installed tools
(let ((nix-zsh (expand-file-name "~/.nix-profile/bin/zsh")))
  (when (file-executable-p nix-zsh)
    (setq shell-file-name nix-zsh)
    (setq explicit-shell-file-name nix-zsh)))

;; Ensure PATH includes Nix profile bins
(let ((nix-profile-bin (expand-file-name "~/.nix-profile/bin")))
  (when (file-directory-p nix-profile-bin)
    ;; Add to exec-path so Emacs can find executables
    (add-to-list 'exec-path nix-profile-bin)
    ;; Also update PATH environment variable for subprocesses
    (setenv "PATH" (concat nix-profile-bin ":" (getenv "PATH")))))

(provide 'my-shell)
