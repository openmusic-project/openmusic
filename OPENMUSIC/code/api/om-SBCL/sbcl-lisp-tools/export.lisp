(in-package :oa)

;;; Kernel-compatibility surface: the OpenMusic kernel (LispWorks) reaches text/listener
;;; tools either as oa:: or qualified om-lisp:: ; OM-API/OA is the single home package.

(export '(om-text-editor
          om-text-edit-window
          om-lisp-edit-window
          om-gutter-edit-view-widget
          om-paren-edit-view-widget
          om-lisp-edit-view-widget
          om-eval-output-pane
          om-make-text-edit-window
          om-open-new-text-editor
          om-open-new-text-editor-at
          om-open-new-text-file
          om-eval-output-append
          with-safe-file-operation
          om-eval-all-buffer
          om-eval-region-buffer
          om-find-definition-at-point
          om-set-text
          om-get-text
          om-set-text-buffer
          om-kill-window-buffer
          om-texteditor-modified
          om-get-lisp-expression
          om-get-editor-panel
          om-destroy-callback
          *line-numbers*
          *line-color-numbers*
          *def-text-edit-font*
          om-make-buffer
          om-copy-buffer
          om-buffer-delete
          om-kill-buffer
          om-buffer-insert
          om-buffer-insert-newline
          om-buffer-text
          om-buffer-char
          om-buffer-size
          om-buffer-substring
          om-lines-in-buffer
          om-buffer-lines
          om-buffer-line-start
          om-buffer-line-end
          om-buffer-skip-forward
          om-buffer-insert-file
          om-buffer-write-file
          om-buffer-eval
          om-read-string-from-file
          history
          action
          linear-history
          closure-action
          actions
          clear
          description
          index
          history-pointer
          capacity
          om-listener
          om-make-new-listener
          om-listener-echo
          om-listener-abort
          set-bg-listener-color
          *om-listener*
          *om-stream*
          *text-bg-color*
          om-repl
          om-make-new-repl
          om-repl-echo
          om-repl-abort
          set-bg-repl-color
          *om-repl*
          *listener-input*
          *listener-font*
          *om-prompt*
          om-show-output-lines
          om-eval-on-process
          om-abort-eval-process
          edit-definition
          add-class-definition
          add-method-definition
          init-root-definition-pathname)
        :oa)

;;; Kernel reaches the lisp-tools surface as om-lisp:: (OM-LISP is an empty
;;; package, so these names must be imported in to mirror vanilla lw-lisp-tools,
;;; which homes the whole folder in :om-lisp).
(defparameter *om-lisp-tools-surface*
  '(;; vars
    *om-listener* *listener-on-top* *text-bg-color* *om-prompt* *om-stream* *emacs-mode*
    *line-numbers* *line-color-numbers*
    ;; eval-process + output
    om-eval-on-process om-abort-eval-process om-show-output-lines
    set-om-debugger om-error-handler *error-backtrace* om-show-error-backtrace
    ;; find-definition
    add-class-definition add-method-definition edit-definition
    init-root-definition-pathname
    ;; history/action (fork extension, kept)
    commit make-action
    ;; textbuffer
    om-make-buffer om-copy-buffer om-buffer-delete om-kill-buffer
    om-buffer-insert om-buffer-insert-newline om-buffer-text om-buffer-char
    om-buffer-size om-buffer-substring om-lines-in-buffer om-buffer-lines
    om-buffer-line-start om-buffer-line-end om-buffer-skip-forward
    om-buffer-insert-file om-buffer-write-file om-buffer-eval
    om-read-string-from-file
    ;; listener
    om-listener om-make-new-listener om-listener-echo om-listener-abort
    set-bg-listener-color
    ;; texteditor
    om-text-editor om-open-new-text-editor om-open-new-text-file
    with-safe-file-operation
    om-window-class-menubar om-set-window-title om-set-bg-color
    om-get-editor-panel om-destroy-callback))

(import *om-lisp-tools-surface* :om-lisp)
(export *om-lisp-tools-surface* :om-lisp)
