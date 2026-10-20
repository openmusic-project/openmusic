(in-package :oa)

;;; Kernel-compatibility surface: the OpenMusic kernel (LispWorks) reaches the score
;;; editors qualified as om-edit:: / oe:: ; OM-API/OA is the single home package.

(export '(om-comment-editor
          open-comment-editor-pane
          om-tree-editor
          open-tree-editor
          om-tempo-editor
          open-tempo-editor
          om-tree-tempo-editor
          open-tree-tempo-editor
          om-obj-info
          open-chordseq-info
          open-chord-info
          open-note-info
          om-table-window
          om-table-editor
          open-new-table-editor
          *table-help-window*
          import-csv-file
          export-csv-file
          om-vector
          make-vector
          set-vector-elt
          get-vector-elt)
        :oa)

(import '(open-comment-editor-pane
          *comment-text*
          *def-comment-edit-font*
          *comment-color*
          *comment-frame*
          intfunc
          fontfunc
          fontcolfunc
          resizefunc
          om-table-window
          om-table-editor
          open-new-table-editor
          *table-help-window*
          import-csv-file
          export-csv-file
          om-vector
          make-vector
          set-vector-elt
          get-vector-elt
          box
          contents
          om-select-window
          *info-pack*
          *editor-text*
          *tempo-editor-text*
          open-tree-editor
          open-tempo-editor
          open-tree-tempo-editor
          open-chordseq-info
          open-chord-info
          open-note-info
          score
          ep
          ompanel)
        :om-edit)

(export '(open-comment-editor-pane
          *comment-text*
          *def-comment-edit-font*
          *comment-color*
          *comment-frame*
          intfunc
          fontfunc
          fontcolfunc
          resizefunc
          om-table-window
          om-table-editor
          open-new-table-editor
          *table-help-window*
          import-csv-file
          export-csv-file
          om-vector
          make-vector
          set-vector-elt
          get-vector-elt
          box
          contents
          om-select-window
          *info-pack*
          *editor-text*
          *tempo-editor-text*
          open-tree-editor
          open-tempo-editor
          open-tree-tempo-editor
          open-chordseq-info
          open-chord-info
          open-note-info
          score
          ep
          ompanel)
        :om-edit)
