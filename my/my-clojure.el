;;; my-clojure.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Clojure development with cider and lsp-mode

(require 'use-package)

;; Ensure required packages are available
(dolist (pkg '(clojure-mode parseedn cider))
  (eval `(use-package ,pkg
           :ensure t)))

;; Basic Clojure mode configuration
(with-eval-after-load 'clojure-mode
  (setq clojure-align-forms-automatically t
        clojure-indent-style 'always-align))

;; CIDER configuration
(with-eval-after-load 'cider
  (require 'projectile)
  (require 'parseedn)
  (require 'cider-overlays)
  (require 'cider-repl)

  (setq cider-overlays-use-font-lock t)
  (setq cider-repl-use-content-types t)
  (setq cider-ns-code-reload-tool 'clj-reload)
  (setq cider-show-error-buffer t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp)
  (setq cider-preferred-build-tool 'clojure-cli)
  (setq cider-default-cljs-repl 'shadow)

  (defun get-project-root ()
    "Get the root directory of the current project."
    (if (projectile-project-p)
      (projectile-project-root)
      default-directory))

  (defun extract-ns-default (deps-file)
    "Extract the ns-default from the :run-x alias in deps.edn."
    (if (file-exists-p deps-file)
      (with-temp-buffer
        (insert-file-contents deps-file)
        (goto-char (point-min))
        (let* ((deps-data (parseedn-read-str (buffer-string)))
               (aliases (gethash :aliases deps-data))
               (run-x (gethash :run-x aliases)))
          (gethash :ns-default run-x)))
      nil))

  (defun set-cider-repl-init-code ()
    (let ((ns (extract-ns-default (concat (get-project-root) "deps.edn"))))
      (if ns
          (format "(require '[%s :as mm]
                            '[clj-reload.core :as reload])
(reload/init
  {:dirs [\"src\" \"dev\" \"test\"]})
" ns))))

  ;; Hook into CIDER's jack-in hook to set the init code dynamically
  (add-to-list 'cider-repl-init-code (set-cider-repl-init-code) t)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (yas-minor-mode-on)
              (yas-define-snippets 'cider-repl-mode
                                   '(("doc" "(clojure.repl/doc $0)" nil nil nil nil nil)
                                     ("r" "(reload/reload)$0" nil nil nil nil nil)
                                     )))))

;; LSP configuration for Clojure
(with-eval-after-load 'lsp-mode
  ;; Configure clojure-lsp
  (setq lsp-clojure-custom-server-command '("bash" "-c" "clojure-lsp"))
  (setq lsp-completion-enable t)

  ;; Optionally enable semantic tokens for better syntax highlighting
  (setq lsp-semantic-tokens-enable t))

;; Enable lsp-mode for Clojure buffers (alongside cider)
(add-hook 'clojure-mode-hook #'lsp-deferred)
(add-hook 'clojurec-mode-hook #'lsp-deferred)
(add-hook 'clojurescript-mode-hook #'lsp-deferred)

(provide 'my-clojure)
