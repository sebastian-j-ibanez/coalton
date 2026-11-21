;;;; loader.lisp -- Runtime TOML theme loading for Mine.

(defpackage #:mine/theme/loader
  (:use #:cl)
  (:export #:load-and-install-theme
           #:validate-theme-file
           #:theme-search-directories
           #:resolve-theme-path
           #:reload-current-theme
           #:current-theme-designator))

(in-package #:mine/theme/loader)

(define-condition theme-error (error)
  ((path :initarg :path :reader theme-error-path)
   (detail :initarg :detail :reader theme-error-detail))
  (:report (lambda (condition stream)
             (format stream "~@[~A: ~]~A"
                     (theme-error-path condition)
                     (theme-error-detail condition)))))

(defstruct raw-theme
  (name "" :type string)
  (inherits "default" :type string)
  (palette (make-hash-table :test #'equal))
  (colors (make-hash-table :test #'equal)))

(defparameter +maximum-theme-bytes+ (* 1024 1024))
(defparameter +maximum-inheritance-depth+ 16)

(defvar *active-theme-designator* "default")

(defun %table-value (root key path)
  (multiple-value-bind (value presentp) (gethash key root)
    (cond
      ((not presentp) nil)
      ((hash-table-p value) value)
      (t
       (error 'theme-error :path path
              :detail (format nil "~A must be a TOML table" key))))))

(defun %copy-string-table (source destination path prefix)
  (when source
    (maphash
     (lambda (key value)
       (unless (and (stringp key) (stringp value))
         (error 'theme-error :path path
                :detail (format nil "~A entries must map string keys to string values"
                                prefix)))
       (setf (gethash (string-downcase key) destination) value))
     source)))

(defun %parse-theme-file (path)
  (when (> (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
             (file-length stream))
           +maximum-theme-bytes+)
    (error 'theme-error :path path :detail "theme file exceeds 1 MiB"))
  (let ((document (cl-toml:parse-file path)))
    (unless (hash-table-p document)
      (error 'theme-error :path path :detail "theme document must be a TOML table"))
    (maphash
     (lambda (key value)
       (declare (ignore value))
       (unless (and (stringp key)
                    (member key '("version" "inherits" "palette" "ui" "editor"
                                  "repl" "diagnostic" "scrollbar" "syntax")
                            :test #'string=))
         (error 'theme-error :path path
                :detail (format nil "unknown top-level key ~S" key))))
     document)
    (multiple-value-bind (version presentp) (gethash "version" document)
      (unless presentp
        (error 'theme-error :path path :detail "missing required version = 1"))
      (unless (and (integerp version) (= version 1))
        (error 'theme-error :path path :detail "only theme version 1 is supported")))
    (let ((raw (make-raw-theme :name (pathname-name path))))
      (multiple-value-bind (inherits presentp) (gethash "inherits" document)
        (when presentp
          (unless (stringp inherits)
            (error 'theme-error :path path :detail "inherits must be a string"))
          (setf (raw-theme-inherits raw) inherits)))
      (%copy-string-table (%table-value document "palette" path)
                          (raw-theme-palette raw) path "palette")
      (dolist (section '("ui" "editor" "repl" "diagnostic" "scrollbar" "syntax"))
        (let ((table (%table-value document section path)))
          (when table
            (maphash
             (lambda (key value)
               (unless (and (stringp key) (stringp value))
                 (error 'theme-error :path path
                        :detail (format nil "[~A] entries must map string keys to string values"
                                        section)))
               (setf (gethash (format nil "~A.~A" section (string-downcase key))
                              (raw-theme-colors raw))
                     value))
             table))))
      raw)))

(defun theme-search-directories ()
  (remove nil
          (list
           (let ((override (uiop:getenv "MINE_CONFIG_HOME")))
             (when override (merge-pathnames "themes/" (uiop:ensure-directory-pathname override))))
           #+win32
           (let ((appdata (uiop:getenv "APPDATA")))
             (when appdata (merge-pathnames "mine/themes/" (uiop:ensure-directory-pathname appdata))))
           #+darwin
           (merge-pathnames "Library/Application Support/mine/themes/"
                            (user-homedir-pathname))
           #-(or win32 darwin)
           (let ((xdg (uiop:getenv "XDG_CONFIG_HOME")))
             (merge-pathnames "mine/themes/"
                              (uiop:ensure-directory-pathname
                               (or xdg (merge-pathnames ".config/" (user-homedir-pathname)))))))
          :test #'equal))

(defun %valid-theme-name-p (name)
  (and (plusp (length name))
       (every (lambda (char)
                (or (and (char>= char #\a) (char<= char #\z))
                    (and (char>= char #\0) (char<= char #\9))
                    (member char '(#\- #\_))))
              name)))

(defun resolve-theme-path (name &optional relative-directory)
  (let ((explicit (or (find #\/ name) (find #\\ name)
                      (uiop:string-suffix-p ".toml" name))))
    (cond
      (explicit
       (let ((path (merge-pathnames name (or relative-directory *default-pathname-defaults*))))
         (and (probe-file path) (truename path))))
      ((not (%valid-theme-name-p name))
       (error 'theme-error :path nil
              :detail (format nil "invalid theme name ~S" name)))
      (t
       (loop for directory in (if relative-directory
                                  (cons relative-directory (theme-search-directories))
                                  (theme-search-directories))
             for path = (merge-pathnames (format nil "~A.toml" name) directory)
             when (probe-file path) return (truename path))))))

(defun %rgb (red green blue)
  (mine/term/color:ColorRGB red green blue))

(defun %default-colors ()
  (let ((table (make-hash-table :test #'equal)))
    (flet ((put (key color) (setf (gethash key table) color)))
      (put "ui.background" (%rgb 0 0 0))
      (put "ui.text" (%rgb 238 238 236))
      (put "ui.text-dim" (%rgb 186 189 182))
      (put "ui.text-dimmer" (%rgb 136 138 133))
      (put "ui.panel" (%rgb 46 52 52))
      (put "ui.selection" (%rgb 72 61 139))
      (put "ui.field" (%rgb 32 36 36))
      (put "ui.status-bar" (%rgb 85 87 83))
      (put "ui.accent" (%rgb 114 159 207))
      (put "ui.accent-bg" (%rgb 52 101 164))
      (put "ui.border" (%rgb 85 87 83))
      (put "ui.brand" (%rgb 135 206 250))
      (put "ui.error-dialog" (%rgb 140 0 0))
      (put "editor.current-line" (%rgb 37 43 43))
      (put "editor.gutter" (%rgb 85 87 83))
      (put "editor.search-match" (%rgb 161 105 18))
      (put "editor.search-current" (%rgb 214 138 22))
      (put "editor.highlight" (%rgb 114 159 207))
      (put "editor.matching-parenthesis" (%rgb 52 101 164))
      (put "editor.quick-result" (%rgb 54 60 60))
      (put "repl.background" (%rgb 37 43 43))
      (put "diagnostic.error" (%rgb 239 41 41))
      (put "diagnostic.warning" (%rgb 252 175 62))
      (put "diagnostic.style-warning" (%rgb 214 138 22))
      (put "diagnostic.info" (%rgb 252 233 79))
      (put "diagnostic.error-background" (%rgb 140 0 0))
      (put "diagnostic.warning-background" (%rgb 161 105 18))
      (put "diagnostic.style-warning-background" (%rgb 54 60 60))
      (put "diagnostic.info-background" (%rgb 52 101 164))
      (put "scrollbar.track-fg" (%rgb 60 66 66))
      (put "scrollbar.track-bg" (%rgb 46 52 52))
      (put "scrollbar.thumb" (%rgb 186 189 182))
      (put "syntax.string" (%rgb 173 127 168))
      (put "syntax.number" (%rgb 138 226 52))
      (put "syntax.quote" (%rgb 239 41 41))
      (put "syntax.type-variable" (%rgb 138 226 52))
      (put "syntax.constructor" (%rgb 237 212 0))
      (put "syntax.accessor" (%rgb 173 127 168))
      (put "syntax.dynamic" (%rgb 255 99 71)))
    table))

(defun %copy-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %merge-raw (parent child)
  (let ((merged (make-raw-theme :name (raw-theme-name child)
                                :inherits (raw-theme-inherits child)
                                :palette (%copy-table (raw-theme-palette parent))
                                :colors (%copy-table (raw-theme-colors parent)))))
    (maphash (lambda (key value) (setf (gethash key (raw-theme-palette merged)) value))
             (raw-theme-palette child))
    (maphash (lambda (key value) (setf (gethash key (raw-theme-colors merged)) value))
             (raw-theme-colors child))
    merged))

(defun %load-raw-theme (name &optional relative-directory (chain nil) (depth 0))
  (when (> depth +maximum-inheritance-depth+)
    (error 'theme-error :path nil :detail "theme inheritance is too deep"))
  (when (member name chain :test #'string=)
    (error 'theme-error :path nil
           :detail (format nil "theme inheritance cycle: ~{~A~^ -> ~}" (reverse (cons name chain)))))
  (if (string= name "default")
      (make-raw-theme :name "default")
      (let ((path (resolve-theme-path name relative-directory)))
        (unless path
          (error 'theme-error :path nil :detail (format nil "theme ~S was not found" name)))
        (let* ((child (%parse-theme-file path))
               (parent-name (raw-theme-inherits child))
               (parent (%load-raw-theme parent-name
                                        (uiop:pathname-directory-pathname path)
                                        (cons name chain) (1+ depth))))
          (%merge-raw parent child)))))

(defun %parse-color-literal (text path)
  (cond
    ((string= text "default") mine/term/color:ColorDefault)
    ((and (= (length text) 7) (char= (char text 0) #\#))
     (handler-case
         (%rgb (parse-integer text :start 1 :end 3 :radix 16)
               (parse-integer text :start 3 :end 5 :radix 16)
               (parse-integer text :start 5 :end 7 :radix 16))
       (error () (error 'theme-error :path path :detail (format nil "invalid color ~S" text)))))
    ((uiop:string-prefix-p "ansi:" text)
     (let ((value (parse-integer text :start 5 :junk-allowed nil)))
       (unless (<= 0 value 15)
         (error 'theme-error :path path :detail (format nil "ANSI color out of range: ~A" value)))
       (mine/term/color:Color16 value)))
    ((uiop:string-prefix-p "index:" text)
     (let ((value (parse-integer text :start 6 :junk-allowed nil)))
       (unless (<= 0 value 255)
         (error 'theme-error :path path :detail (format nil "indexed color out of range: ~A" value)))
       (mine/term/color:Color256 value)))
    (t nil)))

(defun %resolve-color (expression palette path &optional chain)
  (or (%parse-color-literal expression path)
      (let ((next (gethash expression palette)))
        (unless next
          (error 'theme-error :path path :detail (format nil "unknown color or palette entry ~S" expression)))
        (when (member expression chain :test #'string=)
          (error 'theme-error :path path
                 :detail (format nil "palette cycle: ~{~A~^ -> ~}"
                                 (reverse (cons expression chain)))))
        (%resolve-color next palette path (cons expression chain)))))

(defun %resolved-colors (raw)
  (let ((colors (%default-colors)))
    (maphash (lambda (key expression)
               (unless (gethash key colors)
                 (error 'theme-error :path nil :detail (format nil "unknown theme color key ~S" key)))
               (setf (gethash key colors)
                     (%resolve-color expression (raw-theme-palette raw) nil)))
             (raw-theme-colors raw))
    colors))

(defun %theme-from-raw (raw)
  (let* ((colors (%resolved-colors raw))
         (get (lambda (key) (gethash key colors)))
         (background (funcall get "ui.background"))
         (text (funcall get "ui.text")))
    (mine/term/color:Theme
     (raw-theme-name raw)
     (mine/term/color:Style text background mine/term/color:AttrNone)
     (funcall get "ui.selection")
     (funcall get "editor.search-match")
     (funcall get "editor.search-current")
     (funcall get "editor.highlight")
     (funcall get "editor.matching-parenthesis")
     (funcall get "ui.panel")
     (funcall get "repl.background")
     (funcall get "editor.current-line")
     (funcall get "ui.field")
     (funcall get "ui.status-bar")
     (funcall get "ui.accent-bg")
     (funcall get "ui.error-dialog")
     text
     (funcall get "ui.text-dim")
     (funcall get "ui.text-dimmer")
     (funcall get "ui.accent")
     (funcall get "diagnostic.error")
     (funcall get "diagnostic.warning")
     (funcall get "diagnostic.style-warning")
     (funcall get "diagnostic.info")
     (funcall get "diagnostic.error-background")
     (funcall get "diagnostic.warning-background")
     (funcall get "diagnostic.style-warning-background")
     (funcall get "diagnostic.info-background")
     (funcall get "editor.quick-result")
     (funcall get "editor.gutter")
     (funcall get "ui.border")
     (funcall get "ui.brand")
     (funcall get "scrollbar.track-fg")
     (funcall get "scrollbar.track-bg")
     (funcall get "scrollbar.thumb")
     (funcall get "syntax.string")
     (funcall get "syntax.number")
     (funcall get "syntax.quote")
     (funcall get "syntax.type-variable")
     (funcall get "syntax.constructor")
     (funcall get "syntax.accessor")
     (funcall get "syntax.dynamic"))))

(defun validate-theme-file (path)
  (%theme-from-raw (%load-raw-theme (namestring (truename path)))))

(defun current-theme-designator ()
  *active-theme-designator*)

(defun load-and-install-theme (name)
  "Load NAME, install it atomically, and return NIL. Return an error string on failure."
  (handler-case
      (let ((theme (if (string= name "default")
                       (mine/term/color:default-theme)
                       (%theme-from-raw (%load-raw-theme name)))))
        (mine/term/color:install-theme! theme)
        (setf *active-theme-designator* name)
        nil)
    (error (condition)
      (format nil "~A" condition))))

(defun reload-current-theme ()
  "Reload the active theme from its original designator."
  (load-and-install-theme *active-theme-designator*))
