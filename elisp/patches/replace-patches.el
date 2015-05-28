;; Time-stamp: <2015-05-28 17:54:48 kmodi>

;; From replace.el
;; Patch `perform-replace' to accept user defined mappings done to `query-replace-map'
;; http://emacs.stackexchange.com/a/12781/115
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20687
(defun perform-replace (from-string replacements
                                    query-flag regexp-flag delimited-flag
                                    &optional repeat-count map start end backward)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:

  (while (re-search-forward \"foo[ \\t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))

which will run faster and probably do exactly what you want.  Please
see the documentation of `replace-match' to find out how to simulate
`case-replace'.

This function returns nil if and only if there were no matches to
make, or the user didn't cancel the call."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((case-fold-search
	  (if (and case-fold-search search-upper-case)
	      (isearch-no-upper-case-p from-string regexp-flag)
	    case-fold-search))
         (nocasify (not (and case-replace case-fold-search)))
         (literal (or (not regexp-flag) (eq regexp-flag 'literal)))
         (search-string from-string)
         (real-match-data nil)       ; The match data for the current match.
         (next-replacement nil)
         ;; This is non-nil if we know there is nothing for the user
         ;; to edit in the replacement.
         (noedit nil)
         (keep-going t)
         (stack nil)
         (replace-count 0)
         (skip-read-only-count 0)
         (skip-filtered-count 0)
         (skip-invisible-count 0)
         (nonempty-match nil)
	 (multi-buffer nil)
	 (recenter-last-op nil)	; Start cycling order with initial position.

         ;; If non-nil, it is marker saying where in the buffer to stop.
         (limit nil)

         ;; Data for the next match.  If a cons, it has the same format as
         ;; (match-data); otherwise it is t if a match is possible at point.
         (match-again t)
         (message
          (if query-flag
              (apply 'propertize
                     (substitute-command-keys
                      ;; Thu May 28 17:53:44 EDT 2015 - kmodi
                      ;; Added %s at the very beginning of the string below
                      "%sQuery replacing %s with %s: (\\<query-replace-map>\\[help] for help) ")
                     ;;
                     minibuffer-prompt-properties))))

    ;; If region is active, in Transient Mark mode, operate on region.
    (if backward
	(when end
	  (setq limit (copy-marker (min start end)))
	  (goto-char (max start end))
	  (deactivate-mark))
      (when start
	(setq limit (copy-marker (max start end)))
	(goto-char (min start end))
	(deactivate-mark)))

    ;; If last typed key in previous call of multi-buffer perform-replace
    ;; was `automatic-all', don't ask more questions in next files
    (when (eq (lookup-key map (vector last-input-event)) 'automatic-all)
      (setq query-flag nil multi-buffer t))

    ;; REPLACEMENTS is either a string, a list of strings, or a cons cell
    ;; containing a function and its first argument.  The function is
    ;; called to generate each replacement like this:
    ;;   (funcall (car replacements) (cdr replacements) replace-count)
    ;; It must return a string.
    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      (setq replacements (cons 'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (if backward
			(not (or (bobp) (and limit (<= (point) limit))))
		      (not (or (eobp) (and limit (>= (point) limit)))))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (cond ((consp match-again)
				 (goto-char (if backward
						(nth 0 match-again)
					      (nth 1 match-again)))
				 (replace-match-data
				  t real-match-data match-again))
				;; MATCH-AGAIN non-nil means accept an
				;; adjacent match.
				(match-again
				 (and
				  (replace-search search-string limit
						  regexp-flag delimited-flag
						  case-fold-search backward)
				  ;; For speed, use only integers and
				  ;; reuse the list used last time.
				  (replace-match-data t real-match-data)))
				((and (if backward
					  (> (1- (point)) (point-min))
					(< (1+ (point)) (point-max)))
				      (or (null limit)
					  (if backward
					      (> (1- (point)) limit)
					    (< (1+ (point)) limit))))
				 ;; If not accepting adjacent matches,
				 ;; move one char to the right before
				 ;; searching again.  Undo the motion
				 ;; if the search fails.
				 (let ((opoint (point)))
				   (forward-char (if backward -1 1))
				   (if (replace-search search-string limit
						       regexp-flag delimited-flag
						       case-fold-search backward)
				       (replace-match-data
					t real-match-data)
				     (goto-char opoint)
				     nil))))))

	  ;; Record whether the match is nonempty, to avoid an infinite loop
	  ;; repeatedly matching the same empty string.
	  (setq nonempty-match
		(/= (nth 0 real-match-data) (nth 1 real-match-data)))

	  ;; If the match is empty, record that the next one can't be
	  ;; adjacent.

	  ;; Otherwise, if matching a regular expression, do the next
	  ;; match now, since the replacement for this match may
	  ;; affect whether the next match is adjacent to this one.
	  ;; If that match is empty, don't use it.
	  (setq match-again
		(and nonempty-match
		     (or (not regexp-flag)
			 (and (if backward
				  (looking-back search-string)
				(looking-at search-string))
			      (let ((match (match-data)))
				(and (/= (nth 0 match) (nth 1 match))
				     match))))))

	  (cond
	   ;; Optionally ignore matches that have a read-only property.
	   ((not (or (not query-replace-skip-read-only)
		     (not (text-property-not-all
			   (nth 0 real-match-data) (nth 1 real-match-data)
			   'read-only nil))))
	    (setq skip-read-only-count (1+ skip-read-only-count)))
	   ;; Optionally filter out matches.
	   ((not (funcall isearch-filter-predicate
                          (nth 0 real-match-data) (nth 1 real-match-data)))
	    (setq skip-filtered-count (1+ skip-filtered-count)))
	   ;; Optionally ignore invisible matches.
	   ((not (or (eq search-invisible t)
		     ;; Don't open overlays for automatic replacements.
		     (and (not query-flag) search-invisible)
		     ;; Open hidden overlays for interactive replacements.
		     (not (isearch-range-invisible
			   (nth 0 real-match-data) (nth 1 real-match-data)))))
	    (setq skip-invisible-count (1+ skip-invisible-count)))
	   (t
	    ;; Calculate the replacement string, if necessary.
	    (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)))
	    (if (not query-flag)
		(progn
		  (unless (or literal noedit)
		    (replace-highlight
		     (nth 0 real-match-data) (nth 1 real-match-data)
		     start end search-string
		     regexp-flag delimited-flag case-fold-search backward))
		  (setq noedit
			(replace-match-maybe-edit
			 next-replacement nocasify literal
			 noedit real-match-data backward)
			replace-count (1+ replace-count)))
	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this
		;; occurrence.  Any command that sets `done' should
		;; leave behind proper match data for the stack.
		;; Commands not setting `done' need to adjust
		;; `real-match-data'.
		(while (not done)
		  (set-match-data real-match-data)
		  (replace-highlight
		   (match-beginning 0) (match-end 0)
		   start end search-string
		   regexp-flag delimited-flag case-fold-search backward)
		  ;; Bind message-log-max so we don't fill up the message log
		  ;; with a bunch of identical messages.
		  (let ((message-log-max nil)
			(replacement-presentation
			 (if query-replace-show-replacement
			     (save-match-data
			       (set-match-data real-match-data)
			       (match-substitute-replacement next-replacement
							     nocasify literal))
			   next-replacement)))
		    (message message
                             ;; Thu May 28 17:54:19 EDT 2015 - kmodi
                             ;; Show whether `case-fold-search' is `t' or `nil'
                             (if case-fold-search "[case] " "[CaSe] ")
                             ;;
                             (query-replace-descr from-string)
                             (query-replace-descr replacement-presentation)))
		  (setq key (read-event))
		  ;; Necessary in case something happens during read-event
		  ;; that clobbers the match data.
		  (set-match-data real-match-data)
		  (setq key (vector key))
		  (setq def (lookup-key map key))
		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (with-output-to-temp-buffer "*Help*"
			   (princ
			    (concat "Query replacing "
				    (if delimited-flag
					(or (and (symbolp delimited-flag)
						 (get delimited-flag 'isearch-message-prefix))
					    "word ") "")
				    (if regexp-flag "regexp " "")
				    (if backward "backward " "")
				    from-string " with "
				    next-replacement ".\n\n"
				    (substitute-command-keys
				     query-replace-help)))
			   (with-current-buffer standard-output
			     (help-mode))))
			((eq def 'exit)
			 (setq keep-going nil)
			 (setq done t))
			((eq def 'exit-current)
			 (setq multi-buffer t keep-going nil done t))
			((eq def 'backup)
			 (if stack
			     (let ((elt (pop stack)))
			       (goto-char (nth 0 elt))
			       (setq replaced (nth 1 elt)
				     real-match-data
				     (replace-match-data
				      t real-match-data
				      (nth 2 elt))))
			   (message "No previous match")
			   (ding 'no-terminate)
			   (sit-for 1)))
			((eq def 'act)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq done t replaced t))
			((eq def 'act-and-exit)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq keep-going nil)
			 (setq done t replaced t))
			((eq def 'act-and-show)
			 (if (not replaced)
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)
				   real-match-data (replace-match-data
						    t real-match-data)
				   replaced t)))
			((or (eq def 'automatic) (eq def 'automatic-all))
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq done t query-flag nil replaced t)
			 (if (eq def 'automatic-all) (setq multi-buffer t)))
			((eq def 'skip)
			 (setq done t))
			((eq def 'recenter)
			 ;; `this-command' has the value `query-replace',
			 ;; so we need to bind it to `recenter-top-bottom'
			 ;; to allow it to detect a sequence of `C-l'.
			 (let ((this-command 'recenter-top-bottom)
			       (last-command 'recenter-top-bottom))
			   (recenter-top-bottom)))
			((eq def 'edit)
			 (let ((opos (point-marker)))
			   (setq real-match-data (replace-match-data
						  nil real-match-data
						  real-match-data))
			   (goto-char (match-beginning 0))
			   (save-excursion
			     (save-window-excursion
			       (recursive-edit)))
			   (goto-char opos)
			   (set-marker opos nil))
			 ;; Before we make the replacement,
			 ;; decide whether the search string
			 ;; can match again just after this match.
			 (if (and regexp-flag nonempty-match)
			     (setq match-again (and (looking-at search-string)
						    (match-data)))))
			;; Edit replacement.
			((eq def 'edit-replacement)
			 (setq real-match-data (replace-match-data
						nil real-match-data
						real-match-data)
			       next-replacement
			       (read-string "Edit replacement string: "
                                            next-replacement)
			       noedit nil)
			 (if replaced
			     (set-match-data real-match-data)
			   (setq noedit
				 (replace-match-maybe-edit
				  next-replacement nocasify literal noedit
				  real-match-data backward)
				 replaced t))
			 (setq done t))

			((eq def 'delete-and-edit)
			 (replace-match "" t t)
			 (setq real-match-data (replace-match-data
						nil real-match-data))
			 (replace-dehighlight)
			 (save-excursion (recursive-edit))
			 (setq replaced t))
                        ;; Thu May 28 17:24:39 EDT 2015 - kmodi
                        ;; Patch begins here
                        (def (call-interactively def)) ; User-defined key - invoke it.
                        ;; Patch ends here
			;; Note: we do not need to treat `exit-prefix'
			;; specially here, since we reread
			;; any unrecognized character.
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t)))
		  (when query-replace-lazy-highlight
		    ;; Force lazy rehighlighting only after replacements.
		    (if (not (memq def '(skip backup)))
			(setq isearch-lazy-highlight-last-string nil)))
		  (unless (eq def 'recenter)
		    ;; Reset recenter cycling order to initial position.
		    (setq recenter-last-op nil)))
		;; Record previous position for ^ when we move on.
		;; Change markers to numbers in the match data
		;; since lots of markers slow down editing.
		(push (list (point) replaced
;;;  If the replacement has already happened, all we need is the
;;;  current match start and end.  We could get this with a trivial
;;;  match like
;;;  (save-excursion (goto-char (match-beginning 0))
;;;		     (search-forward (match-string 0))
;;;                  (match-data t))
;;;  if we really wanted to avoid manually constructing match data.
;;;  Adding current-buffer is necessary so that match-data calls can
;;;  return markers which are appropriate for editing.
			    (if replaced
				(list
				 (match-beginning 0)
				 (match-end 0)
				 (current-buffer))
			      (match-data t)))
		      stack))))))

      (replace-dehighlight))
    (or unread-command-events
	(message "Replaced %d occurrence%s%s"
		 replace-count
		 (if (= replace-count 1) "" "s")
		 (if (> (+ skip-read-only-count
			   skip-filtered-count
			   skip-invisible-count) 0)
		     (format " (skipped %s)"
			     (mapconcat
			      'identity
			      (delq nil (list
					 (if (> skip-read-only-count 0)
					     (format "%s read-only"
						     skip-read-only-count))
					 (if (> skip-invisible-count 0)
					     (format "%s invisible"
						     skip-invisible-count))
					 (if (> skip-filtered-count 0)
					     (format "%s filtered out"
						     skip-filtered-count))))
			      ", "))
		   "")))
    (or (and keep-going stack) multi-buffer)))
