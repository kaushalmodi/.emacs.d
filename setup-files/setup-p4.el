;; P4 / Perforce
;; https://github.com/gareth-rees/p4.el

(use-package p4
  :init
  (progn
    (setq p4-global-key-prefix nil)) ;Don't use the default `C-x p' prefix
  :config
  (progn
    (defun modi/p4-process-finished (buffer _process-name _message)
      "Render the ANSI color codes in the P4 buffers.

If \"P4COLORS\" environment variable is set the `p4' outputs
would be colorful, and we would want to render those ANSI color
escape codes."
      (when (getenv "P4COLORS")
        (let ((inhibit-read-only t))
          (with-current-buffer buffer
            (ansi-color-apply-on-region-int (point-min) (point-max))))))
    (advice-add 'p4-process-finished :after #'modi/p4-process-finished)
    ;; (advice-remove 'p4-process-finished #'modi/p4-process-finished)

    (defhydra hydra-p4 (:color blue
                        :columns 5)
      "p4"
      ("a"   p4-add             "Add")                                     ;add
      ("X"   p4-delete          "Delete")                                  ;delete
      ("o"   p4-edit            "Check(o)ut for edit")                     ;edit
      ("O"   p4-reopen          "Reopen")                                  ;reopen
      ("R"   p4-revert          "Discard changes")                         ;revert
      ("i"   p4-submit          "Submit/check(I)n")                        ;submit
      ("m"   p4-move            "Move files")                              ;move
      ("I"   p4-integ           "Integrate sets of files")                 ;integrate
      ("r"   p4-resolve         "Resolve integ and updates to ws")         ;resolve
      ("z"   p4-reconcile       "Reconcile")                               ;reconcile

      ("v"   p4-annotate        "Annotate")                                ;annotate
      ("f"   p4-filelog         "File Changelog")                          ;filelog
      ("d"   p4-diff            "Diff with depot version of current file") ;diff
      ("e"   p4-ediff           "Use ediff to compare file with client")
      ("p"   p4-print           "See depot version of current file")       ;print
      ("="   p4-diff2           "Compare depot file sets")
      ("D"   p4-diff-all-opened "Diff all opened files")

      ("C"   p4-changes         "My Changelists")                          ;changes
      ("lo"  p4-opened          "List opened files")                       ;opened
      ("lf"  p4-files           "List depot files")                        ;files

      ("s"   p4-status          "Status")                                  ;status
      ("u"   p4-update          "Sync")                                    ;sync
      ("g"   p4-refresh         "Refresh unopened files")                  ;sync -f
      ("H"   p4-have            "List recent updates")                     ;have
      ("w"   p4-where           "Mapping of current file in depot/ws")     ;where

      ("B"   p4-branch          "Edit branch spec")                        ;branch
      ("lb"  p4-branches        "List of branch specs")                    ;branches
      ("L"   p4-label           "Label spec")                              ;label
      ("ll"  p4-labels          "List of labels")                          ;labels
      ("ls"  p4-labelsync       "Apply label")                             ;labelsync
      ("U"   p4-user            "User spec")                               ;user
      ("lu"  p4-users           "List users")                              ;users
      ("C-d" p4-describe        "Changelist Desc")                         ;describe
      ("C-f" p4-depot-find-file "Find file depot")

      ("J"   p4-job             "Job spec")                                ;job
      ("lj"  p4-jobs            "List of jobs")                            ;jobs
      ("F"   p4-fix             "Fix jobs")                                ;fix

      ("cc"  p4-client          "Client spec")                             ;client
      ("cn"  p4-get-client-name "Client name")
      ("ci"  p4-info            "Client/server info")                      ;info
      ("cp"  p4-set-p4-port     "Set P4PORT")

      ("t"   p4-toggle-vc-mode  "Toggle Offline mode")

      ("h"   p4-help            "Help")                                    ;help
      ("?"   p4-help            "Help")
      ("V"   p4-version         "Version")                                 ;version
      ("q"   nil                "cancel" :color blue))

    (bind-key "s-4" #'hydra-p4/body)
    (bind-key "C-c 4" #'hydra-p4/body)))


(provide 'setup-p4)
