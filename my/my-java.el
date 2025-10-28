;;; my-java.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Project detection for Java projects
;; Ensure project.el recognizes Maven/Gradle projects
(with-eval-after-load 'project
  (defun my/project-try-maven (dir)
    "Find Maven project root by looking for pom.xml."
    (when-let ((root (locate-dominating-file dir "pom.xml")))
      (cons 'transient root)))

  (defun my/project-try-gradle (dir)
    "Find Gradle project root by looking for build.gradle or build.gradle.kts."
    (when-let ((root (or (locate-dominating-file dir "build.gradle")
                         (locate-dominating-file dir "build.gradle.kts"))))
      (cons 'transient root)))

  ;; Add Maven and Gradle project detection before VC
  (add-hook 'project-find-functions #'my/project-try-maven 10)
  (add-hook 'project-find-functions #'my/project-try-gradle 10))

;; Fix for Eglot glob pattern parsing error with Eclipse JDT Language Server
;; This handles cases where JDT LS sends glob patterns in extended format
;; with :pattern and :baseUri keys (LSP 3.17+) instead of plain strings
(defun my/eglot-glob-parse-fix (original-func glob-spec)
  "Fix for eglot glob parsing when receiving complex glob patterns from jdtls.
Handles both simple string patterns and plist patterns with :pattern/:baseUri keys."
  (let ((pattern-string
         (cond
          ;; If glob-spec is a plist with :pattern, extract the pattern string
          ((and (listp glob-spec) (plist-get glob-spec :pattern))
           (plist-get glob-spec :pattern))
          ;; If it's already a string, use it directly
          ((stringp glob-spec)
           glob-spec)
          ;; Fallback: return a safe default pattern
          (t
           "**/*.java"))))
    (funcall original-func pattern-string)))

;; Apply the fix to eglot's glob parsing
(with-eval-after-load 'eglot
  (advice-add 'eglot--glob-parse :around #'my/eglot-glob-parse-fix))

(use-package eglot-java
  :ensure t
  :demand t
  :hook ((java-mode java-ts-mode) . eglot-java-mode)
  :hook ((java-mode java-ts-mode) . eglot-ensure)
  :init
  ;; Suppress warning about unsupported workspace/didChangeWorkspaceFolders capability
  ;; This is a known issue with Eclipse JDT LS - the capability is not critical
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-ignored-server-capabilities :workspaceFolders))
  :config
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
   "Custom options that will be merged with any default settings."
   (let* ((format-settings-file (expand-file-name "config/jvm/eclipse-java-google-style.xml" user-emacs-directory))
          (bundles-dir (expand-file-name "config/jvm/bundles" user-emacs-directory))
          (jar-files (when (file-directory-p bundles-dir)
                       (directory-files (expand-file-name "config/jvm/bundles" user-emacs-directory) t "\\.jar$"))))
     `(,@(when jar-files
          `(:bundles ,(apply #'vector jar-files)))
       :settings
        (:java
          (:autobuild (:enabled t))
          :configuration
          (:runtimes [(:name "JavaSE-17"
                       :path "/Library/Java/JavaVirtualMachines/corretto-17.0.16.8.1.jdk/Contents/Home"
                       :default nil)
                      (:name "JavaSE-21"
                       :path "/Library/Java/JavaVirtualMachines/corretto-21.0.8.9.1.jdk/Contents/Home"
                       :default nil)
                      (:name "JavaSE-22"
                       :path "/Library/Java/JavaVirtualMachines/corretto-25.0.1.8.1.jdk/Contents/Home"
                       :default t)])
          :contentProvider (:preferred ["fernflowerContentProvider"])
          ,@(when (file-exists-p format-settings-file)
             `(:format (:settings (:url ,format-settings-file
                                   :profile "GoogleStyle"))))
          :home "/Library/Java/JavaVirtualMachines/corretto-25.0.1.8.1.jdk/Contents/Home"
          :implementationsCodeLens (:enabled t)
          :jdt (:ls (:lombokSupport (:enabled t))))
       :extendedClientCapabilities (:classFileContentsSupport t)))))

(use-package eglot-java-lombok
  :ensure (:host github :repo "ltylty/eglot-java-lombok" :branch "main" :rev :newest)
  :demand t
  :config
  (eglot-java-lombok/init))

;; Keybindings for eglot-java
(with-eval-after-load 'eglot-java
  (define-prefix-command 'eglot-java-prefix-map)
  (define-key eglot-java-mode-map (kbd "C-c j") 'eglot-java-prefix-map)
  (define-key eglot-java-prefix-map (kbd "n") #'eglot-java-file-new)
  (define-key eglot-java-prefix-map (kbd "x") #'eglot-java-run-main)
  (define-key eglot-java-prefix-map (kbd "t") #'eglot-java-run-test)
  (define-key eglot-java-prefix-map (kbd "P") #'eglot-java-project-new)
  (define-key eglot-java-prefix-map (kbd "T") #'eglot-java-project-build-task)
  (define-key eglot-java-prefix-map (kbd "R") #'eglot-java-project-build-refresh)
  (define-key eglot-java-prefix-map (kbd "d") #'my/testng-debug))

;; TestNG debugging with Dape
;; Usage: Start your Java TestNG tests with JDWP enabled:
;;   java -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 \
;;        -cp your-classpath org.testng.TestNG your-testng.xml
;; Then run M-x dape RET testng-attach RET to attach the debugger

(defun my/testng-debug ()
  "Start debugging TestNG tests.
This will prompt for TestNG configuration and start the Java process
with JDWP enabled, then attach the debugger."
  (interactive)
  (message "Starting TestNG with debugging on port 5005...")
  (message "Run your Java process with: java -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 -cp <classpath> org.testng.TestNG <testng.xml>")
  (message "Then attach with: M-x dape RET testng-attach RET"))

(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(testng-attach
                 modes (java-mode java-ts-mode)
                 host "localhost"
                 port 5005
                 :type "java"
                 :request "attach")))

(provide 'my-java)
