;;; assist-po-mode.el --- jump functions for po-mode.el

;; Commentary
;; You can edit po file with referring to at corresponding line of texi file.
;; This gives functions to  assists when using po-mode.el, to translate po file.
;;
;; Usage:
;; To start by texi fie with M-x Occur to show structure, and po file.

;;;1. Add the following lines to .emacs
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
;;;2. Open texi file (you have defined with the variable my-texi-file-name)
;;;3. press C-j at the line and you will find that you jumped to the corresponding line of my-po-file-name in another buffer.
;;;

(defun proj-translation-org2ja ()
  "The batch process to start translating org2ja."
  (interactive)
  (progn (find-file (concat my-po-file-directory my-po-file-name))
    (find-file (concat my-po-file-directory my-texi-file-name))
	 (texinfo-show-structure)))

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


(defun move-to-first-window ()
  "Move the most top-left window in this frame. This should have the name, my-occur-buffer-name."
  (interactive)
  (select-window (frame-first-window (selected-frame))))

(defun hilight-line ()
  "Hilight just one region. The hilighted region moves when this function is called."
  (interactive)
  (if (not (overlayp ovl))
      (progn (setq ovl (make-overlay (point-at-bol) (point-at-eol) (current-buffer)))
	     (overlay-put ovl 'face 'highlight)))

; remove all overlays in the current-buffer.
;  (remove-overlays)
  (move-overlay ovl (point-at-bol) (point-at-eol) (current-buffer)))


(defun jump-to-coresponding-line-in-po-file ()
  ""
  (interactive)
  (let ()
    (search-backward-regexp "^$")
    (next-line)

;; FIX ME
;;       to avoid "^@node" when (search-forward-regexp "^$") and to (next-line)
;;
    (if (equal (buffer-substring-no-properties (point) (+ (point) 5)) "@node")
	(next-line))
;;
;;

    (setq min-line-in-texi-file (line-number-at-pos))
    (search-forward-regexp "^$")
;    (hilight-line)
    (setq max-line-in-texi-file (line-number-at-pos))

    (pop-to-buffer my-po-file-name)
    (goto-char 1)

    (setq reexp-for-lines
	  (concat "\\("
		  (mapconcat 'identity (number-to-string-array (generate-seq
								min-line-in-texi-file
								max-line-in-texi-file))
			     "\\|")
		  "\\)"))
;   if 111-114
;   (111, 112, 113, 114)
;   \\(111\\|112\\|113\\|114\\)


    (search-forward-regexp reexp-for-lines)))


(defun generate-seq (begin end)
  "if ``begin''=1 and ``end''=5, generates array '(1, 2, 3, 4, 5). "
  (interactive"n\nn")
  (let ()
    (setq list-array (list end))
    (setq element end)
    (while (> element begin)
      (progn (setq element (1- element))
	     (setq list-array (cons element list-array))))
    list-array))

(defun number-to-string-array (list)
  "(1, 2, 3) -> (\"1\", \"2\", \"3\")"
;  (interactive)
  (let ()
    (setq value nil)
    (dolist (element
	     list
	     value)
      (setq value (cons (number-to-string element) value)))
    (reverse value)))



(defun my-po-move-in-texi-file-to-current-po-entry ()
  "Get line number to jump in texi-file-to-current-po-entry by searching \"#: texi-filename:12345\". After getting target line number (e.g. 12345), goto-line the line number and redisplay at the center of the window."
  (interactive)
  (save-excursion

;    (hilight-line)

    (re-search-backward (concat "^#: " my-texi-file-name ":" "\\([0-9]+\\\)[ ]*"))
    (setq line-no (string-to-number
		   (setq line-no-str
			 (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
		   10))
    (pop-to-buffer my-texi-file-name)
    (goto-line line-no)
    (recenter)

;    (hilight-line)

    (pop-to-buffer my-po-file-name)
    ))



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
  (my-po-move-in-texi-file-to-current-po-entry))
(ad-activate 'po-current-entry)

(define-key texinfo-mode-map "\C-j" 'jump-to-coresponding-line-in-po-file)

(provide 'assist-po-mode)

;; -*- assist-po-mode.el ends here -*-
