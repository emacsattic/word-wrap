;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; word-wrap-mode  (kind of a minor mode)
;; Version:  0.6
;; Modified: 11/18/97
;; Created by Dan Steinmark
;; 4 Sep 97
;;
;; Description:
;;   Allows the text to be displayed in auto-fill
;;   but the returns within a paragraph are not saved.
;;   Paragraphs are defined according to that used by forward-paragraph and
;;   backward-paragraph.
;;   Use (require 'word-wrap-mode) to load.
;;   Turn on and off using M-x word-wrap-mode
;;
;;   Also provides the following routines which may be useful:
;;      unfill-paragraph         For removing soft returns in current paragraph
;;      unfill-buffer            For removing soft returns in buffer
;;      convert-returns-to-soft  For converting hard returns in region
;;
;;   Configuration variables:
;;      ww-force-all-returns-hard  defaults to nil, which helps with
;;                                 conversion from non-word-wrapped files,
;;                                 but does not work with a few types of
;;                                 files that have been created with
;;                                 word-wrap.  see on-line docs.
;;
;;   Would be nice if:
;;      it provided a mode hook
;;      it did a lazy-lock style fill/unfill so saves & mode changes don't
;;         take a long time on large files.
;;      fill command was configurable
;;      autoload were added (?)
;;      etc...
;;
;;   Bugs/Side Affects/Limitations (?):
;;      User can turn off auto-fill, which doesn't really make much sense.
;;      For this to be totally correct, hard vs. soft newlines need to
;;        be encoded.
;;      Refills buffer after saves.
;;      Point repositioning does not accommodate changes.
;;      Does not handle fill-prefixes, indented paragraphs, or right margins
;;      etc...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ww-force-all-returns-hard nil
"*Configures word-wrap-mode

Typical usage:
If you need to convert 'filled' files into word-wrapped format
(returns internal to paragraphs displayed during editing, but deleted
upon saving) then
   set ww-force-all-returns-hard to nil  (the default)
This will make conversion fairly automatic.

If you do not often need to convert 'filled' files, then
   set ww-force-all-returns-hard to t
This will work well with files already in word-wrapped format, but
will necessitate you first convert 'filled' files using
M-x unfill-buffer
otherwise there will be hard returns placed within all the
paragraphs.

Description:
On startup, word-wrap-mode converts some or all returns
to hard returns.  If there are no lines longer than the screen, and
there are no hard returns in the buffer, (typical of a file using
auto-fill-mode and saved not using word-wrap-mode), and then the
returns are place only the end of paragraphs.  Normally, a
word-wrapped file will have long lines, which will cause all returns
to be made hard returns, which are then maintained until saved.

If you have adjacent, short lines, which are not separated by
whitespace, and have no long lines, in your word wrapped file, this
variable may be set to 't' to force all returns to hard, but
conversion of non- word-wrapped files will need to be first unfilled
using M-x unfill-buffer.  Otherwise there will be hard returns
inserted within paragraphs.")
(setq ww-force-all-returns-hard nil)

(defvar word-wrap-mode nil
"It is an error to set this value manually.  It should be set using
word-wrap-mode, turn-on-word-wrap, or turn-off-word-wrap only.")
(make-variable-buffer-local 'word-wrap-mode)

;;(defvar temp-save-buffer nil)
;;(defvar temp-save-buffer-point nil)
;;(defvar temp-save-buffer-mark)

;;(defvar old-after-save-hook after-save-hook)
;;(defvar old-write-contents-hooks write-contents-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add to mode line.
(or (assq 'word-wrap-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(word-wrap-mode (" Wrap"))
                minor-mode-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun store-buffer-temporarily ()
;  "saves the current buffer, point, and mark in the global temp-save-buffer...
;variables"
;  (interactive)
;  (if (not (buffer-live-p temp-save-buffer))
;      (setq temp-save-buffer (create-file-buffer "*temp-save-buffer*")))
;
;  (copy-to-buffer temp-save-buffer (point-min) (point-max))
;  (setq temp-save-buffer-point (point))
;  (setq temp-save-buffer-mark (mark))
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; somehow, the point and mark get clobbered anyway, when used with
;; after-save-hook, even though they are restored prior to finishing.
;(defun restore-current-buffer ()
;  "restores the current buffer from the global temp-save-buffer, (if active)
;which is usually created using save-buffer-temporarily"
;  (interactive)
;  (let ((curr-buffer (current-buffer)))
;
;    (if (not (buffer-live-p temp-save-buffer))
;       (error "*temp-save-buffer* not active")
;      ;; else
;      (set-buffer temp-save-buffer)
;      (copy-to-buffer curr-buffer (point-min) (point-max))
;      (set-buffer curr-buffer)
;      (set-mark temp-save-buffer-mark)
;      (goto-char temp-save-buffer-point)
;      (message "went to char: %d" (point))
;      ))
;
;  );; end restore-current-buffer

;; Maybe save-excursion should be used, instead.

(defun restore-current-buffer ()
"restores the current buffer for word-wrap-mode for use following a save"

  (let ((old-point (point)))
    (fill-region (point-min) (point-max) nil t)
    (goto-char old-point)
    );; end let
);; end restore-current-buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun unfill-and-restore ()
;  (interactive)
;
;  (store-buffer-temporarily)
;
;  (unfill-buffer)
;
;  (restore-current-buffer)
;  )

(defun store-then-unfill-buffer ()
  "Originally store buffer in a temp buffer for later restoration,
then unfills the current buffer.
Now it just unfills the buffer.  The restore no longer relies on
the save temp-buffer."

;;  (store-buffer-temporarily)
  (unfill-buffer)
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unfill-paragraph looks for return followed by a non-ws char, and then
;; merges the lines with a single space (or possibly a double space for
;; end of sentences and after colons, according to sentence-end-double-space
;; and colon-double-space).  Moves point to after paragraph.
;;
(defun unfill-paragraph ()
  "unfills the paragraph after or around the point.  Does not handle
fill-prefixes."
  (interactive)
  (let ((end-of-paragraph-loc nil)
        (replace-p nil)
        (replace-with-spaces 1)
        (spaces " "))
    (forward-paragraph)
    (setq end-of-paragraph-loc (point))
    (backward-paragraph)
    (while (and (looking-at "[ \t]*\n")
                (< (point) end-of-paragraph-loc))
      (forward-line))
    (while (< (point) end-of-paragraph-loc)

      (end-of-line)
      (backward-char)
      (cond ((looking-at (concat sentence-end   ;; end of sentence
                                 "\n[^ \t\n]")) ;; return followed by non ws
             (progn
               (if sentence-end-double-space
                   (setq replace-with-spaces 2)
                 (setq replace-with-spaces 1))
               (setq replace-p t)))

            ((looking-at ":\n[^ \t\n]")         ;; a colon, return, non ws
             (progn
               (if colon-double-space
                   (setq replace-with-spaces 2)
                 (setq replace-with-spaces 1))
               (setq replace-p t)))

            ((looking-at ".\n[^ \t\n]")         ;; any other char, ret non ws
             (progn
               (setq replace-with-spaces 1)
               (setq replace-p t)))

            (t                                  ;; otherwise
             (progn
               (forward-line)
               (setq replace-p nil))))

      (if replace-p
          (progn
            (forward-char)
            (if (get-text-property (point) 'hard)
                (forward-line)               ;; only replace soft returns
              ;;else
              (delete-char 1)
              (if (= replace-with-spaces 1)
                  (setq spaces " ")
                (setq spaces "  "))
              (insert spaces)
              (setq end-of-paragraph-loc (+ (- replace-with-spaces 1)
                                            end-of-paragraph-loc))
            )))

      );; end while
    );; end let
  );; end unfill-paragraph

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; convert-returns-to-hard
;;
(defun make-return-hard (hard-p)
  "If the point is on a return,
      this makes it 'hard' by setting the 'hard' property to t,
      if argument HARD-P is t, otherwise it makes it nil"
  (interactive)
  (if (looking-at "\n")
      (put-text-property (point) (1+ (point)) 'hard hard-p))
  )

(defun convert-buffer-returns-to-hard (&optional convert-all-p)
  "Converts all returns in the buffer to hard returns, if the optional
parameter convert-all-p is non-nil, otherwise, converts only returns
that are at paragraph ends."
  (interactive "P")
  (let ((old-point (point)))

    (goto-char (point-min))

    (if convert-all-p
        (while (search-forward "\n" nil t)
          (goto-char (match-beginning 0))
          (make-return-hard t)
          (goto-char (match-end 0))
          )
      ;; else
      (while (< (point) (point-max))
        (forward-paragraph)
        (backward-char)
        (make-return-hard t) ;; only does it if its a return
        (forward-char)))

    (goto-char old-point)
    )
  );; end convert-buffer-returns-to-hard

(defun convert-returns-to-soft ()
  "Converts all returns in the region to soft returns."
  (interactive)
  (let ((old-point (point))
        (reg-beg   (region-beginning))
        (reg-end   (region-end)))

    (goto-char reg-beg)

    (while (search-forward "\n" reg-end t)
      (goto-char (match-beginning 0))
      (make-return-hard nil)
      (goto-char (match-end 0))
      )

    (goto-char old-point)
    )
  );; end convert-returns-to-soft

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unfill-buffer does the whole buffer, return the point to about the
;; right place
;;
(defun unfill-buffer ()
  "unfills every paragraph in the buffer using unfill-paragraph"
  (interactive)
  (let ((old-point (point))
        (point-before nil)
        (progress-made-p t))
    (goto-char (point-max))

    (while (and progress-made-p
                (> (point) (point-min)))
      (setq point-before (point))
      (backward-paragraph)
      (unfill-paragraph)
      (backward-paragraph)
      (if (= (point) point-before)
          (setq progress-made-p nil))
      )

    (goto-char old-point)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Puts the buffer back to filled, for use with after-save-hook.
;;
(defun wordwrap-after-save-hook ()
  (restore-current-buffer)
  (set-buffer-modified-p nil)
  (message "Wrote %s without soft returns" (buffer-file-name))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun long-lines-exist-p ()
  "checks to see if there are any lines longer than the fill-column"
  (interactive)
  (let ((old-point (point))
        (long-line-found nil))
    (goto-char (point-min))
    (while (and (not (eobp))
                (not long-line-found))
      (end-of-line)
      (if (> (current-column) fill-column)
          (setq long-line-found t))
      (forward-line)
      )
    (goto-char old-point)
    long-line-found
    )
  );; end long-lines-exist-p

(defun hard-returns-exist-p ()
  "checks to see if there are any lines longer than the fill-column"
  (interactive)
  (let ((old-point (point))
        (hard-return-found nil))
    (goto-char (point-min))
    (while (and (not (eobp))
                (not hard-return-found))
      (end-of-line)
      (if (get-text-property (point) 'hard)
          (setq hard-return-found t))
      (forward-line)
      )
    (goto-char old-point)
    hard-return-found
    )
  );; end hard-returns-exist-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; puts the minor mode into word wrap
;;   inserts returns
;;   changes mode line
;;   sets fill-column to full frame width
;;   modifies save hooks to save without the returns
;;
;;
;;when going into word-wrap:
;;if long lines (> fill-column) and
;;   there exist hard returns   ;; an inbetween state, some are not filled
;;then
;;   put hard returns only at paragraph ends
;;
;;if no long lines and
;;   no hard returns   ;; buffer in need of word wrap, but has returns "saved"
;;then
;;   put hard returns only at paragraph ends
;;
;;if long lines and
;;   no hard returns   ;; typical file using word wrap,
;;                     ;; or a file with some but not all lines wrapped
;;then
;;   put hard returns everywhere
;;
;;if no long lines and
;;   there exist hard returns   ;; should already be set up as ready for wrap
;;then
;;   put hard returns only at paragraph ends
;;

(defun turn-on-word-wrap ()
  " puts the mode into word wrap
      inserts returns
      changes mode line
      sets fill-column to full frame width
      modifies save hooks to save without the returns

word-wrap-mode changes certain returns to hard returns when invoked.  If they
wind up where you didn't want them, it is probably because there were long
lines in some places but not others.  To prevent this from happening, you
must fill ALL of the paragraphs before going into word-wrap-mode, OR remove
the 'soft' style returns from ALL paragraphs.  It must be consistent.  If
word-wrap-mode has made returns hard where you didn't want them, and you want
them changed to be soft returns, use convert-returns-to-soft over a
region."

  (interactive)

  (let ((old-point (point))
        (temp-buffer-modified-p (buffer-modified-p)))
    (turn-on-auto-fill)
    (if (not word-wrap-mode)
        (progn
          (setq word-wrap-mode t)
          (setq fill-column (- (frame-width) 1))

          ;; convert some or all returns to hard
          (if (or ww-force-all-returns-hard
                  (and (long-lines-exist-p)
                       (not (hard-returns-exist-p))))
              ;; typical file using word wrap,
              ;; or a file with some but not all lines wrapped
              (convert-buffer-returns-to-hard t) ;; convert all
            ;; else
            (convert-buffer-returns-to-hard nil) ;; convert only paragraph
                                                 ;; ends
            );; end if

          (setq use-hard-newlines t)

          (fill-region (point-min) (point-max) nil t)

          ;; already buffer-local whenever set
          ;; (make-local-hook 'write-contents-hooks)
          (add-hook 'write-contents-hooks 'store-then-unfill-buffer)

          (make-local-hook 'after-save-hook)
          (add-hook 'after-save-hook 'wordwrap-after-save-hook t t)

          (set-buffer-modified-p temp-buffer-modified-p)
          (goto-char old-point)
          );;end progn
      );; end if
    );; end let
  );; end turn-on-word-wrap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  removes word wrap from the minor mode
;;     removes the returns within paragraphs
;;     changes mode line
;;     reverts save hooks
;;

(defun turn-off-word-wrap ()
  "Turns off word wrap by removing the save hooks that were added for
word wrap.  It also reverts the file to its saved state, i.e. without the
returns."
  (interactive)
  (let ((temp-buffer-modified-p (buffer-modified-p)))
    (if word-wrap-mode
        (progn
          (setq word-wrap-mode nil)
          (unfill-buffer)
          (remove-hook 'write-contents-hooks 'store-then-unfill-buffer)
          (remove-hook 'after-save-hook 'wordwrap-after-save-hook t)
          (set-buffer-modified-p temp-buffer-modified-p)
          (auto-fill-mode -1) ;; turn off auto-fill
        )))
  );; end turn-off-word-wrap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-wrap-mode ()
  "Toggles word-wrap-mode (a minor mode).
Word wrap mode allows the words to wrap on the screen without the returns
that are internal to a paragraph being saved.  Does not handle fill-prefixes.
Use word-wrap-mode to exit the minor mode, and return the buffer to
soft-returns not displayed.

word-wrap-mode changes certain returns to hard returns when invoked.  If they
wind up where you didn't want them, it is probably because there were long
lines in some places but not others.  To prevent this from happening, you
must fill ALL of the paragraphs before going into word-wrap-mode, OR remove
the 'soft' style returns from ALL paragraphs.  It must be consistent.  If
word-wrap-mode has made returns hard where you didn't want them, and you want
them changed to be soft returns, use M-x convert-returns-to-soft over a
region.

Also see the documentation for ww-force-all-returns-hard."

  (interactive)
  (if word-wrap-mode
      (turn-off-word-wrap)
    ;;else
    (turn-on-word-wrap))
)

(provide 'word-wrap)

;;; word-wrap.el ends here
