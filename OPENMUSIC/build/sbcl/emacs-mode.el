;; Enter the full path, including the filename.
;; Windows:
;; (defvar openmusic-path "PATH-TO/openmusic.exe")
;; Linux:
;; (defvar openmusic-path "PATH-TO/openmusic")
;; macOS: inside the bundle, where a launcher runs the bundle's own runtime on the
;; console core (build/sbcl/deliver-sbcl.lisp, #+darwin)
;; (defvar openmusic-path "PATH-TO/OM 8.1 SBCL/OM 8.1.app/Contents/Resources/openmusic")

;; SLIME from the same version as the swank inside openmusic. Enter the path of the swank folder.
;; (defvar om-slime-directory "PATH-TO/swank/")

(add-to-list 'load-path om-slime-directory)
(require 'slime-autoloads)

;; the SLIME files carry no lexical-binding cookie: silence Emacs 31.1 warning per file
;; (add-to-list 'warning-suppress-types '(files))

(require 'slime)
(setq slime-lisp-implementations
      (list (list 'openmusic (list openmusic-path))))
(setq inferior-lisp-program openmusic-path)
(setq slime-protocol-version 'ignore)
(setq slime-enable-evaluate-in-emacs t)

(defun activate-om ()
  "Bring the OpenMusic process of the SLIME connection to the front."
  (let ((pid (slime-pid)))
    (cond ((eq system-type 'windows-nt)
           (call-process "powershell.exe" nil nil nil "-NoProfile" "-Command"
                         (format "$ws = New-Object -ComObject WScript.Shell; $ws.AppActivate(%d)" pid)))
          ((eq system-type 'darwin)
           (call-process "osascript" nil nil nil "-e"
                         "on run argv
  set targetPID to (item 1 of argv) as integer
  tell application \"System Events\"
    set p to first application process whose unix id is targetPID
    set frontmost of p to true
  end tell
end run"
                         (number-to-string pid)))
          (t
           (let ((window-id
                  (with-temp-buffer
                    (call-process "wmctrl" nil t nil "-lp" "-u")
                    (goto-char (point-min))
                    (when (re-search-forward
                           (format "^\\(\\S-+\\)[ \t]+\\S-+[ \t]+%d[ \t]" pid) nil t)
                      (match-string 1)))))
             (when window-id
               (call-process "wmctrl" nil nil nil "-i" "-a" window-id)))))))

(defun om-show-workspace ()
  "Show the OpenMusic workspace window and bring OpenMusic to the front."
  (interactive)
  (slime-eval-async '(cl:progn (alld:post-to-ui-thread (cl:lambda () (om::show-workspace-win))) nil)
    (lambda (_) (activate-om))))

(slime)
