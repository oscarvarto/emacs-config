;;; my-rust.el -*- lexical-binding: t; no-byte-compile: t; -*-

(dolist (pkg '(direnv envrc))
  (eval `(use-package ,pkg
           :ensure t
           :demand t)))

(require 'cl-lib)
(require 'subr-x)  ;; string-trim, string-empty-p

;; Rustic customization (these don't affect mode setup)
(custom-set-faces
 '(rustic-compilation-column ((t (:inherit compilation-column-number))))
 '(rustic-compilation-line ((t (:foreground "fuchsia")))))

;; Ensure tree-sitter grammar for Rust is installed
;; This prevents the "language grammar for rust is unavailable" warning
(with-eval-after-load 'treesit-auto
  ;; Check if rust grammar is available, if not, install it
  (unless (treesit-language-available-p 'rust)
    (message "Installing tree-sitter grammar for Rust...")
    (when (fboundp 'treesit-auto-install-all)
      (treesit-auto-install-all)))

  ;; Only load rust-ts-mode after treesit-auto has had a chance to install grammars
  (let ((orig-derived (symbol-function 'derived-mode-add-parents)))
    (unwind-protect
        (progn
          (fset 'derived-mode-add-parents
                (lambda (mode extra)
                  (when (and (eq mode 'rust-ts-mode)
                             (memq 'rust-mode extra))
                    (setq extra (cl-remove 'rust-mode extra)))
                  (funcall orig-derived mode extra)))
          (require 'rust-ts-mode nil t))
      (fset 'derived-mode-add-parents orig-derived))))

;; Ensure rustic is the primary mode for .rs files
(setq auto-mode-alist
      (cons '("\\.rs\\'" . rustic-mode)
            (cl-remove-if (lambda (entry)
                            (memq (cdr entry) '(rust-ts-mode rust-ts-mode-maybe)))
                          auto-mode-alist)))

;; Avoid remapping rust-mode to rust-ts-mode; rustic handles tree-sitter itself.
(when (boundp 'treesit-major-mode-remap-alist)
  (setq treesit-major-mode-remap-alist
        (assq-delete-all 'rust-mode treesit-major-mode-remap-alist)))

(setq rust-mode-treesitter-derive t)
(setq rustic-rustfmt-args "+nightly")
(setq rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))

(defun my/direnv-project-root ()
  "Return the nearest directory containing a .envrc file, or nil."
  (when-let ((root (locate-dominating-file default-directory ".envrc")))
    (expand-file-name root)))

(defun my/find-executable-in-env (program &optional prefer)
  "Return absolute path to PROGRAM found via PATH in `process-environment'.
When PREFER is non-nil, it should be a predicate receiving a candidate path
and returning non-nil for the preferred match."
  (let* ((path (getenv "PATH"))
         (dirs (and path (split-string path path-separator t)))
         (candidates '()))
    (dolist (dir dirs)
      (let ((candidate (and dir (expand-file-name program dir))))
        (when (and candidate (file-executable-p candidate))
          (push candidate candidates))))
    (setq candidates (nreverse candidates))
    (cond
     ((and prefer candidates)
      (or (cl-find-if prefer candidates)
          (car candidates)))
     (candidates
      (car candidates))
     (t nil))))

;; Detect if we're in a Nix environment
(defun my/in-nix-environment-p ()
  "Check if we're running in a Nix environment."
  (or (getenv "IN_NIX_SHELL")
      (getenv "NIX_PATH")
      ;; Check if rustc is from nix store
      (let ((rustc-path (my/find-executable-in-env
                         "rustc"
                         (lambda (path)
                           (string-match-p "/nix/store/" path)))))
        (and rustc-path (string-match-p "/nix/store/" rustc-path)))))

;; Safe toolchain detection that works in daemon mode and supports Nix
(defun my/get-rust-toolchain ()
  "Get rust toolchain safely, handling both Nix and rustup environments."
  (condition-case err
      (let* ((default-directory (expand-file-name "~/"))
             (process-environment (append '("PATH=/usr/local/bin:/opt/homebrew/bin:$PATH") process-environment))
             (toolchain-output (shell-command-to-string "rustup default 2>/dev/null | cut -d'-' -f1")))
        (if (and toolchain-output (not (string-empty-p (string-trim toolchain-output))))
            (string-trim toolchain-output)
          (or (getenv "RUST_TOOLCHAIN") "nightly")))
    (error
     (message "Warning: Could not detect rust toolchain, using fallback: %s" err)
     (or (getenv "RUST_TOOLCHAIN") "nightly"))))

;; Set rust-analyzer command based on environment
(defun my/setup-rust-analyzer ()
  "Configure rust-analyzer command based on the active environment.
Sets the command globally for rustic-analyzer-command."
  (let* ((ra-path (my/find-executable-in-env
                   "rust-analyzer"
                   (lambda (path)
                     (string-match-p "/nix/store/" path))))
         (nix-env (my/in-nix-environment-p))
         (envrc-root (my/direnv-project-root))
         (direnv-available (and envrc-root (executable-find "direnv")))
         command)
    (cond
     (direnv-available
      (let ((exec-root (directory-file-name envrc-root)))
        (setq command `("direnv" "exec" ,exec-root "rust-analyzer"))
        (message "Using direnv exec for rust-analyzer (env: %s)" exec-root)))
     (ra-path
      (setq command (list ra-path))
      (message "Using rust-analyzer from %s%s"
               ra-path
               (if (and nix-env (string-match-p "/nix/store/" ra-path))
                   " (Nix)"
                 "")))
     (t
      (let ((toolchain (my/get-rust-toolchain)))
        (setq command `("rustup" "run" ,toolchain "rust-analyzer"))
        (message "rust-analyzer not found in PATH; falling back to rustup toolchain %s" toolchain))))
    (when command
      (setq rustic-analyzer-command command))))

(my/setup-rust-analyzer)

;; Interactive command to verify Rust environment
(defun my/rust-show-environment ()
  "Display information about the current Rust environment."
  (interactive)
  (let* ((in-nix (my/in-nix-environment-p))
         (envrc-root (my/direnv-project-root))
         (direnv-available (and envrc-root (executable-find "direnv")))
         (rustc-path (my/find-executable-in-env
                      "rustc"
                      (lambda (path)
                        (string-match-p "/nix/store/" path))))
         (ra-path (my/find-executable-in-env
                   "rust-analyzer"
                   (lambda (path)
                     (string-match-p "/nix/store/" path))))
         (rustc-version (when rustc-path
                          (string-trim (shell-command-to-string "rustc --version"))))
         (ra-version (when ra-path
                      (string-trim (shell-command-to-string "rust-analyzer --version"))))
         (analyzer-cmd (bound-and-true-p rustic-analyzer-command)))
    (with-current-buffer (get-buffer-create "*Rust Environment*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== Rust Environment Information ===\n\n"))
        (insert (format "Nix environment detected: %s\n\n" (if in-nix "YES" "NO")))
        (insert (format ".envrc root: %s\n" (or envrc-root "NOT FOUND")))
        (insert (format "direnv exec available: %s\n\n" (if direnv-available "YES" "NO")))
        (insert (format "rustc path: %s\n" (or rustc-path "NOT FOUND")))
        (insert (format "rustc version: %s\n\n" (or rustc-version "N/A")))
        (insert (format "rust-analyzer path: %s\n" (or ra-path "NOT FOUND")))
        (insert (format "rust-analyzer version: %s\n\n" (or ra-version "N/A")))
        (insert (format "rustic-analyzer-command: %S\n" analyzer-cmd))
        (insert (format "rustic-lsp-client: %S\n\n" (bound-and-true-p rustic-lsp-client)))
        (insert (format "Environment variables:\n"))
        (insert (format "  IN_NIX_SHELL: %s\n" (or (getenv "IN_NIX_SHELL") "not set")))
        (insert (format "  NIX_PATH: %s\n" (or (getenv "NIX_PATH") "not set")))
        (insert (format "  PATH: %s\n" (getenv "PATH"))))
      (special-mode)
      (display-buffer (current-buffer)))))

;; Configure rustic with eglot
(use-package rustic
  :ensure t
  :demand t
  :custom
  ;; Use eglot instead of lsp-mode
  (rustic-lsp-client 'eglot)
  ;; rust-analyzer command will be set by my/setup-rust-analyzer
  (rustic-format-on-save nil)
  (rustic-cargo-use-last-stored-arguments t)
  ;; Enable detached file support if needed (eglot-only feature)
  (rustic-enable-detached-file-support nil)
  :config
  ;; Load rustic-babel for org-babel support
  ;; This provides the ob-rust feature needed for org-mode code blocks
  (require 'rustic-babel nil t)

  ;; Auto-save hook
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))

  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

  ;; Setup rust-analyzer by advising rustic-setup-lsp
  ;; This ensures we set rustic-analyzer-command RIGHT before eglot starts
  (defun my/rustic-setup-analyzer-advice (&rest _)
    "Advice to set rustic-analyzer-command before eglot setup."
    (my/setup-rust-analyzer))

  (advice-add 'rustic-setup-lsp :before #'my/rustic-setup-analyzer-advice))

;; Eglot configuration for Rust
(use-package emacs
  :ensure nil
  :hook (rustic-mode . eglot-ensure)
  :config
  ;; Disable flymake if you prefer another linter (e.g., flycheck)
  ;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  ;; Optional: Configure eglot-rust-analyzer initialization options
  ;; These are rustic's defaults, but you can customize them here
  (with-eval-after-load 'rustic-lsp
    ;; Ensure eglot-rust-analyzer class is defined by rustic
    (when (class-p 'eglot-rust-analyzer)
      ;; You can add custom initialization options here if needed
      ;; Example: Configure clippy on save
      (setq-default eglot-workspace-configuration
                    '(:rust-analyzer
                      (:checkOnSave (:command "clippy")
                       :cargo (:features "all")
                       :procMacro (:enable t)))))))

;; Direnv/envrc integration: refresh rust-analyzer when environment changes
(defun my/rustic--direnv-updated (&rest _)
  "Refresh rust-analyzer command after direnv updates."
  (my/setup-rust-analyzer)
  ;; Restart eglot if it's running in the current buffer
  (when (and (derived-mode-p 'rustic-mode)
             (eglot-managed-p))
    (call-interactively #'eglot-reconnect)))

(with-eval-after-load 'direnv
  (when (boundp 'direnv-after-update-environment-hook)
    (add-hook 'direnv-after-update-environment-hook #'my/rustic--direnv-updated)))

(with-eval-after-load 'envrc
  (when (boundp 'envrc-after-update-hook)
    (add-hook 'envrc-after-update-hook #'my/rustic--direnv-updated)))

(provide 'my-rust)
