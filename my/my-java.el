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

(defgroup my/jdtls nil
  "Custom configuration for running Eclipse JDT LS under Eglot."
  :group 'tools
  :prefix "my/jdtls-")

(defcustom my/jdtls-install-root
  (expand-file-name "git-repos/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository"
                    (or (getenv "HOME") default-directory))
  "Root directory of the Eclipse JDT LS installation.
The directory should contain the `plugins' and `config_*' subdirectories
produced by building the `eclipse.jdt.ls' project."
  :type 'directory)

(defcustom my/jdtls-java-home "/Library/Java/JavaVirtualMachines/corretto-25.0.1.8.1.jdk/Contents/Home"
  "Preferred JDK installation used both to launch JDT LS and as the default runtime."
  :type 'directory)

(defcustom my/jdtls-workspace-base
  (expand-file-name "cache/jdtls-workspaces" user-emacs-directory)
  "Base directory where per-project JDT LS workspaces are stored."
  :type 'directory)

(defcustom my/jdtls-runtimes
  '((:name "JavaSE-17"
     :path "/Library/Java/JavaVirtualMachines/corretto-17.0.16.8.1.jdk/Contents/Home"
     :default nil)
    (:name "JavaSE-21"
     :path "/Library/Java/JavaVirtualMachines/corretto-21.0.8.9.1.jdk/Contents/Home"
     :default nil)
    (:name "JavaSE-25"
     :path "/Library/Java/JavaVirtualMachines/corretto-25.0.1.8.1.jdk/Contents/Home"
     :default t))
  "Runtime definitions passed to `java.configuration.runtimes'."
  :type '(repeat (plist :tag "Runtime"
                        :options
                        ((:name string)
                         (:path directory)
                         (:default boolean)))))

(defcustom my/jdtls-lombok-jar
  (expand-file-name "config/jvm/bundles/lombok.jar" user-emacs-directory)
  "Optional Lombok agent passed to the language server.
Set to nil to skip adding a `-javaagent' argument entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Lombok jar")))

(defcustom my/jdtls-extra-bundles nil
  "List of additional Eclipse JDT LS bundles to load.
Each entry should be an absolute path to an OSGi bundle jar
exporting the appropriate `Bundle-SymbolicName'.  Leave this nil
unless you explicitly need to install an extension."
  :type '(repeat file))

(defun my/jdtls--ensure-install-root ()
  "Return a usable JDT LS installation directory or signal an error."
  (let ((root my/jdtls-install-root))
    (unless (and root (file-directory-p root))
      (error "JDT LS install root not found: %s. Build eclipse.jdt.ls or customize `my/jdtls-install-root'"
             root))
    root))

(defun my/jdtls--launcher-jar (root)
  "Locate the Equinox launcher jar under ROOT."
  (let* ((plugins (expand-file-name "plugins" root))
         (candidates (when (file-directory-p plugins)
                       (directory-files plugins t "^org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))))
    (or (car (last (sort candidates #'string<)))
        (error "Cannot find org.eclipse.equinox.launcher_*.jar in %s" plugins))))

(defun my/jdtls--config-dir (root)
  "Return the platform configuration directory under ROOT."
  (let* ((subdir (pcase system-type
                   ('darwin "config_mac")
                   ((or 'gnu/linux 'gnu) "config_linux")
                   ('windows-nt "config_win")
                   (_ (error "Unsupported system-type %s for JDT LS" system-type))))
         (path (expand-file-name subdir root)))
    (unless (file-directory-p path)
      (error "Missing JDT LS configuration directory: %s" path))
    path))

(defun my/jdtls--workspace-dir (project)
  "Return the workspace directory for PROJECT."
  (make-directory my/jdtls-workspace-base t)
  (let* ((root (if project
                   (file-name-as-directory (expand-file-name (project-root project)))
                 (file-name-as-directory (expand-file-name default-directory))))
         (hash (md5 root))
         (workspace (expand-file-name hash my/jdtls-workspace-base)))
    (make-directory workspace t)
    workspace))

(defun my/jdtls--java-command ()
  "Return the Java executable used to launch JDT LS."
  (let* ((preferred (and my/jdtls-java-home
                         (expand-file-name "bin/java" my/jdtls-java-home))))
    (cond
     ((and preferred (file-executable-p preferred)) preferred)
     ((executable-find "java"))
     (t (error "Cannot locate a suitable Java executable. Customize `my/jdtls-java-home' or ensure `java' is on PATH")))))

(defun my/jdtls--runtime-vector ()
  "Convert `my/jdtls-runtimes' into a vector, filtering missing paths."
  (let (valid)
    (dolist (rt my/jdtls-runtimes)
      (let ((path (plist-get rt :path)))
        (if (and path (file-directory-p path))
            (push rt valid)
          (message "Skipping JDT runtime %s (missing path %s)" (plist-get rt :name) path))))
    (when valid
      (apply #'vector (nreverse valid)))))

(defun my/jdtls--format-settings ()
  "Return a plist with format settings if the configuration file exists."
  (let ((format-file (expand-file-name "config/jvm/eclipse-java-google-style.xml"
                                       user-emacs-directory)))
    (when (file-exists-p format-file)
      (list :format
            (list :settings
                  (list :url format-file
                        :profile "GoogleStyle"))))))

(defun my/jdtls--java-settings ()
  "Assemble the `java' settings section for JDT LS."
  (let* ((base (list :autobuild (list :enabled t)
                     :contentProvider (list :preferred (vector "fernflowerContentProvider"))
                     :home my/jdtls-java-home
                     :implementationsCodeLens (list :enabled t)
                     :jdt (list :ls (list :lombokSupport (list :enabled t)))))
         (runtime-vector (my/jdtls--runtime-vector)))
    (append base
            (when runtime-vector
              (list :configuration (list :runtimes runtime-vector)))
            (my/jdtls--format-settings))))

(defun my/jdtls--bundles-vector ()
  "Build the vector of extra bundles declared in `my/jdtls-extra-bundles'."
  (let (valid)
    (dolist (bundle my/jdtls-extra-bundles)
      (let ((expanded (expand-file-name bundle)))
        (if (file-exists-p expanded)
            (push expanded valid)
          (message "Skipping missing JDT LS bundle %s" bundle))))
    (when valid
      (apply #'vector (nreverse valid)))))

(defun my/jdtls--lombok-agent ()
  "Return a list with the Lombok `-javaagent' argument if available."
  (let ((jar (cond
              ((and my/jdtls-lombok-jar (file-exists-p my/jdtls-lombok-jar))
               my/jdtls-lombok-jar)
              (t
               (car (directory-files (expand-file-name "config/jvm/bundles"
                                                       user-emacs-directory)
                                     t "lombok.*\\.jar$" t))))))
    (when jar
      (list (concat "-javaagent:" (expand-file-name jar))))))

(defun my/jdtls--initialization-options ()
  "Compute initialization options for JDT LS."
  (let ((bundles (my/jdtls--bundles-vector))
        (java-settings (my/jdtls--java-settings)))
    `(,@(when bundles
          `(:bundles ,bundles))
      :settings (:java ,java-settings)
      :extendedClientCapabilities (:classFileContentsSupport t))))

(defun my/jdtls--contact (_interactive project)
  "Return the contact specification for `eglot-server-programs'."
  (let* ((root (my/jdtls--ensure-install-root))
         (launcher (my/jdtls--launcher-jar root))
         (config (my/jdtls--config-dir root))
         (workspace (my/jdtls--workspace-dir project))
         (java (my/jdtls--java-command))
         (agent-args (my/jdtls--lombok-agent))
         (cmd (append (list java
                            "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                            "-Dosgi.bundles.defaultStartLevel=4"
                            "-Declipse.product=org.eclipse.jdt.ls.core.product"
                            "-Dlog.protocol=false"
                            "-Dlog.level=WARN"
                            "-Xms1G"
                            "-XX:+UseStringDeduplication")
                      agent-args
                      (list "-jar" launcher
                            "-configuration" config
                            "-data" workspace))))
    (append cmd
            (list :initializationOptions (my/jdtls--initialization-options)))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-ignored-server-capabilities :workspaceFolders)
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) . my/jdtls--contact))
  (let* ((current (copy-sequence (default-value 'eglot-workspace-configuration)))
         (updated (plist-put current :java (my/jdtls--java-settings))))
    (setq-default eglot-workspace-configuration updated)))

(dolist (hook '(java-mode-hook java-ts-mode-hook))
  (add-hook hook #'eglot-ensure))


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
