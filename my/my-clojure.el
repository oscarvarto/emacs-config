;; -*- lexical-binding: t; no-byte-compile: t; -*-

(dolist (pkg '(clojure-mode parseedn cider))
  (eval `(use-package ,pkg
           :ensure t)))

(with-eval-after-load 'cider
  (require 'projectile)
  (require 'parseedn)
  (require 'cider-overlays)
  (require 'cider-repl)
 
  (setq cider-overlays-use-font-lock t)
  (setq cider-repl-use-content-types t)

  (setq cider-ns-code-reload-tool 'clj-reload)

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

(provide 'my-clojure)
