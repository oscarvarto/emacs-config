;;; my-java.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Java development with lsp-mode and lsp-java

(require 'use-package)

;; Configure lsp-java (already installed via elpaca in my-elpaca.el)
(use-package lsp-java
  :ensure nil
  :after lsp-mode)

;; Project detection for Java projects
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

;; JDK configuration
(defvar my/jdk-paths
  `((:name "JavaSE-17"
     :path ,(string-trim (shell-command-to-string "mise where java@corretto-17"))
     :version "17"
     :default nil)
    (:name "JavaSE-21"
     :path ,(string-trim (shell-command-to-string "mise where java@corretto-21"))
     :version "21"
     :default nil)
    (:name "JavaSE-25"
     :path ,(string-trim (shell-command-to-string "mise where java@corretto-25"))
     :version "25"
     :default t))
  "List of JDK installations available for use by LSP servers.")

(defun my/jdk-get-default-path ()
  "Get the path of the default JDK installation."
  (let ((default-jdk (seq-find (lambda (jdk) (plist-get jdk :default)) my/jdk-paths)))
    (when default-jdk
      (plist-get default-jdk :path))))

(defun my/jdk-get-lsp-java-runtimes ()
  "Get JDK runtime configuration for lsp-java."
  (let ((runtimes (mapcar (lambda (jdk)
                           `(:name ,(plist-get jdk :name)
                             :path ,(plist-get jdk :path)
                             :default ,(plist-get jdk :default)))
                         my/jdk-paths)))
    (vconcat runtimes)))

;; LSP Java configuration
(with-eval-after-load 'lsp-java
  ;; Set JAVA_HOME for lsp-java
  (setenv "JAVA_HOME" (my/jdk-get-default-path))

  ;; Download Lombok jar if needed
  (defvar my/lombok-jar-path
    (expand-file-name "config/jvm/bundles/lombok.jar" user-emacs-directory)
    "Path to Lombok jar file.")

  (unless (file-exists-p my/lombok-jar-path)
    (let ((lombok-dir (file-name-directory my/lombok-jar-path)))
      (unless (file-directory-p lombok-dir)
        (make-directory lombok-dir t))
      (message "Downloading Lombok jar...")
      (url-copy-file "https://projectlombok.org/downloads/lombok.jar"
                     my/lombok-jar-path)))

  ;; Configure lsp-java VM arguments
  ;; Optimized for Java 21+ (using Corretto 25 with G1GC and modern JVM features)
  (setq lsp-java-vmargs
        (list "-XX:+UseG1GC"                    ; G1GC is default and better for modern Java
              "-XX:+UseStringDeduplication"     ; Java 8+: Reduce memory for duplicate strings
              "-XX:GCTimeRatio=4"
              "-XX:AdaptiveSizePolicyWeight=90"
              "-Dsun.zip.disableMemoryMapping=true"
              "-Xmx16G"
              "-Xms512m"                        ; Higher min heap for better startup performance
              "--add-modules=ALL-SYSTEM"        ; Modern Java: enable all system modules
              (concat "-javaagent:" (expand-file-name my/lombok-jar-path))))

  ;; Configure lsp-java settings
  (setq lsp-java-java-path (expand-file-name "bin/java" (my/jdk-get-default-path)))
  (setq lsp-java-configuration-runtimes (my/jdk-get-lsp-java-runtimes))

  ;; Additional lsp-java settings
  (setq lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-references-code-lens-enabled t
        lsp-java-signature-help-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-format-enabled nil
        lsp-java-save-actions-organize-imports nil
        lsp-java-content-provider-preferred "fernflower"
        lsp-java-autobuild-enabled t
        lsp-java-max-concurrent-builds 16
        lsp-java-completion-enabled t
        lsp-java-completion-overwrite t
        lsp-java-completion-guess-method-arguments t
        lsp-java-folding-range-enabled nil
        lsp-java-progress-reports-enabled t
        lsp-java-code-generation-hash-code-equals-use-java7objects t
        lsp-java-code-generation-hash-code-equals-use-instanceof t
        lsp-java-code-generation-use-blocks nil
        lsp-java-code-generation-generate-comments t
        lsp-java-code-generation-to-string-skip-null-values t
        lsp-java-code-generation-to-string-list-array-contents t
        lsp-java-code-generation-to-string-limit-elements 0
        lsp-java-inhibit-message nil))

;; Format settings (optional - Google Style)
(defvar my/java-format-settings-file
  (expand-file-name "config/jvm/eclipse-java-google-style.xml" user-emacs-directory)
  "Path to Eclipse formatter settings file.")

(when (file-exists-p my/java-format-settings-file)
  (with-eval-after-load 'lsp-java
    (setq lsp-java-format-settings-url my/java-format-settings-file
          lsp-java-format-settings-profile "GoogleStyle")))

;; Enable lsp-mode for Java buffers
;; These hooks will autoload lsp-mode when a Java buffer is opened
(defun my/java-lsp-setup ()
  "Setup lsp-mode for Java buffers, ensuring lsp-java is loaded."
  (require 'lsp-java)
  (lsp-deferred))

(add-hook 'java-mode-hook #'my/java-lsp-setup)
(add-hook 'java-ts-mode-hook #'my/java-lsp-setup)

;; DAP (Debug Adapter Protocol) configuration for Java
(with-eval-after-load 'dap-mode
  (require 'dap-java nil t))

(provide 'my-java)
