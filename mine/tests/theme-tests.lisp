(defpackage #:mine-tests/theme
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:color #:mine/term/color)))

(in-package #:mine-tests/theme)

(coalton-toplevel
  (declare color-equal? (color:Color * color:Color -> Boolean))
  (define (color-equal? left right) (== left right))

  (declare style-equal? (color:Style * color:Style -> Boolean))
  (define (style-equal? left right) (== left right)))

(in-package #:mine-tests)

(defun check-custom-theme-loads-palette-colors ()
  (with-test-directory (directory)
    (let ((path (merge-pathnames "custom.toml" directory)))
      (%write-utf8-file
       path
       (format nil
               "version = 1~%inherits = 'default'~%[palette]~%bg = '#112233'~%fg = '#ddeeff'~%[ui]~%background = 'bg'~%text = 'fg'~%[syntax]~%string = '#00ff00'~%"))
      (unwind-protect
           (progn
             (%check (null (mine/theme/loader:load-and-install-theme (namestring path)))
                     "A valid custom theme failed to load")
             (%check (mine-tests/theme::style-equal?
                      (mine/term/color:Style
                       (mine/term/color:ColorRGB 221 238 255)
                       (mine/term/color:ColorRGB 17 34 51)
                       mine/term/color:AttrNone)
                      (mine/term/color:default-style))
                     "Theme foreground and background were not installed")
             (%check (mine-tests/theme::color-equal? (mine/term/color:ColorRGB 0 255 0)
                                    (mine/term/color:syntax-string-fg))
                     "Theme syntax color was not installed"))
        (mine/theme/loader:load-and-install-theme "default")))))

(defun check-theme-inheritance-and-reload ()
  (with-test-directory (directory)
    (let ((parent (merge-pathnames "parent.toml" directory))
          (child (merge-pathnames "child.toml" directory)))
      (%write-utf8-file parent
                        (format nil "version = 1~%[ui]~%background = '#101010'~%"))
      (%write-utf8-file child
                        (format nil "version = 1~%inherits = 'parent'~%[ui]~%text = '#eeeeee'~%"))
      (unwind-protect
           (progn
             (%check (null (mine/theme/loader:load-and-install-theme (namestring child)))
                     "An inherited theme failed to load")
             (%check (mine-tests/theme::color-equal? (mine/term/color:ColorRGB 16 16 16)
                                    (mine/term/color:background-color))
                     "The inherited background was not installed")
             (%write-utf8-file parent
                               (format nil "version = 1~%[ui]~%background = '#202020'~%"))
             (%check (null (mine/theme/loader:reload-current-theme))
                     "Reloading the active theme failed")
             (%check (mine-tests/theme::color-equal? (mine/term/color:ColorRGB 32 32 32)
                                    (mine/term/color:background-color))
                     "Reload did not pick up the changed parent theme"))
        (mine/theme/loader:load-and-install-theme "default")))))

(defun check-invalid-theme-keeps-current-theme ()
  (with-test-directory (directory)
    (let ((path (merge-pathnames "invalid.toml" directory))
          (before (mine/term/color:current-theme)))
      (%write-utf8-file path (format nil "version = 1~%[ui]~%unknown = '#ffffff'~%"))
      (%check (stringp (mine/theme/loader:load-and-install-theme (namestring path)))
              "An invalid theme did not report an error")
      (%check (eq before (mine/term/color:current-theme))
              "An invalid theme replaced the active theme"))))

(defun check-theme-command-line-options ()
  (%check (string= "night"
                   (mine/app/mine::%command-line-theme-override
                    '("mine" "--theme" "night" "source.ct")))
          "--theme did not select its argument")
  (%check (equal '("source.ct")
                 (mine/app/mine::%command-line-file-arguments
                  '("mine" "--theme" "night" "source.ct")))
          "--theme leaked into file arguments")
  (%check (handler-case
              (progn
                (mine/app/mine::%command-line-theme-override '("mine" "--theme"))
                nil)
            (error () t))
          "A missing --theme value was accepted"))
