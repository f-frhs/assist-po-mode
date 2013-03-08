;;; assist-po-mode.el --- jump functions for po-mode.el

;; What is this?:
;;   You can edit po file with following mode that always refers to at
;;   corresponding line of texi file.  When you push C-j in the texi
;;   file, you will jump to the corresponding line of po file.  When
;;   you move to next/provious msgid in po file with n/p key, you will
;;   find the sticky window that always shows corresponding line of po
;;   file.
;;
;; Screenshot:
;;   - https://cacoo.com/diagrams/AVAmgqMyVsValX2g-1A11D.png :jump to po file
;;
;; Requirements:
;;   + po-mode.el
;;   + texinfo.el
;;
;; Keybindings:
;;   In the buffer of po file:
;;     n => move to the next msgid with following mode
;;     p => move to the previous msgid with following mode
;;     . => show the current msgid with folloing mode
;;   In the buffer of texi file:
;;     C-j => jump-to-coresponding-line-in-po-file
;;

;; Usage:
;; 1. Open texi file (you have defined it as a variable my-texi-file-name)
;;    and open po file (also defined it as my-po-file-name).
;; 2. In the texi file buffer, press C-j at the line you want to jump.
;;    You will find that you jumped to the corresponding line of
;;    my-po-file-name in another buffer.
;; 3. In the po file buffer, press n or p to move a next or previous entry.

;;;How to install:
;;;1. Add the following lines to .emacs, after adjusting to suite your system.
;;;
;;;(add-to-list 'load-path (expand-file-name "/path/to/assist-po-mode.el"))
;;;(require 'assist-po-mode)
;;;
;;;(setq my-proj-path "/path/to/proj")
;;;(setq my-texi-file-name "sample.texi")
;;;;(setq my-po-file-directory (concat my-proj-path "/" "tmp/"))
;;;(setq my-po-file-directory (concat my-proj-path "/"))
;;;(setq my-po-file-name "sample.po")
;;;
;;;   It is assumed that you have 2 related files.
;;;    - /path/to/project/sample.texi
;;;    - /path/to/project/sample.po
;;;   where, `sample.po' was generated from `sample.texi' and is to
;;;   edit to proceed translation.
;;;
;;;

;; To close related buffers
;; (delete-buffers-for-po)

(require 'po-mode)
(require 'texinfo)

(defvar my-proj-path "/home/user/proj"
  "The directory which you manage.")
(defvar my-texi-file-name "sample.texi"
  "The texi file to refer when you are editing po file.")
;(defvar my-po-file-directory (concat my-proj-path "/" "tmp/")
;  "The directory name in case you have po file in sub directory of `my-proj-path'")
(defvar my-po-file-directory (concat my-proj-path "/")
  "The directory your po file stored.")
(defvar my-po-file-name "sample.po"
  "The file name of po file to edit.")


(defun show1-po-file-other-window (line-in-texi) "\
Display the corresponding line of po-file in another window.
The corresponding line is found by searching the current line number
(LINE-IN-TEXI) of texi-file buffer, in the po-file buffer.
"
  (interactive "N")
  (unless (window-live-p (get-buffer-window my-po-file-name))
    (pop-to-buffer my-po-file-name t))
  (with-selected-window (get-buffer-window my-po-file-name)
    (prog2 (goto-char (point-min))
	(search-forward-regexp
	 (concat my-texi-file-name ":" (number-to-string line-in-texi) "[^0-9]")
	 (point-max)
	 t)
      (recenter))))

(defun show-po-file-smartly-in-other-window (&optional arg) "\
Display in another window the corresponding block the current line
describes.  When the corresponding line to follow is not found, the
next nearest corresponding line will be shown.

With a prefix arg ARG, makes po-file buffer follow only the current
line.  In this case, if nothing is found to follow, nothing will
happen.

When this command is called twice with no break, the point moves to
the next line forcibly and then searchs the next corresponding line.
"
  (interactive "P")
  (cond ((equal arg '(4))
	 (show1-po-file-other-window (line-number-at-pos)))
	(t
	 (when (eq last-command this-command)
	   (forward-line))
	 (while (and (not (eobp))
		     (not (show1-po-file-other-window (line-number-at-pos))))
	   (forward-line))
	 (when (eobp)
	   (message "Reached the end of file with no msgid found.")))))

(defun my-po-current-entry-followed-by-recentered-texi-window () "\
Shows and recenters the corresponding line of texi buffer in other window,
according to po-file's current entry, such as
\"#:texi-filename:12345\"."
  (interactive)
  (save-excursion
    (re-search-backward (concat "^#: " my-texi-file-name ":" "\\([0-9]+\\\)[ ]*"))
    (let* ((line-no-str (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	   (line-no (string-to-number line-no-str)))
    (unless (window-live-p (get-buffer-window my-texi-file-name))
      (pop-to-buffer my-texi-file-name t))
    (with-selected-window (get-buffer-window my-texi-file-name)
      (goto-line line-no)
      (recenter)))))

(defun delete-buffers-for-po ()
  "delete all buffers related with the editing po file."
  (interactive)

  (if (get-buffer "*Occur*")
      (kill-buffer "*Occur*")
    (message (concat "There is no " "*Occur*" " buffer here.")))
  (if (get-buffer my-texi-file-name)
      (kill-buffer my-texi-file-name)
    (message (concat "There is no " my-texi-file-name " buffer here.")))
  (if (get-buffer my-po-file-name)
      (kill-buffer my-po-file-name)
    (message (concat "There is no " my-po-file-name " buffer here.")))

  (delete-other-windows)
  (switch-to-buffer (get-buffer "*scratch*"))
  (message "Killing buffers finished."))


(defadvice po-current-entry (after po-current-entry-and-recenter-texi-buffer)
  (my-po-current-entry-followed-by-recentered-texi-window))
(ad-activate 'po-current-entry)

(define-key texinfo-mode-map "\C-j" 'show-po-file-smartly-in-other-window)

(provide 'assist-po-mode)

;; -*- assist-po-mode.el ends here -*-
