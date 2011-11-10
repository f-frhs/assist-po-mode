# asssit-po-mode

## What is assist-po-mode?

This program displays the corresponding line of po file in the Emacs buffer.

![screenshot-jump-to-po-file](https://cacoo.com/diagrams/AVAmgqMyVsValX2g-1A11D.png?width=600)

### Screenshots

![screenshot-jump-to-po-file](https://cacoo.com/diagrams/AVAmgqMyVsValX2g-1A11D.png?width=600)
## For member of org-ja
### Setting example:
Firstly, you need to
    git clone git@github.com:org-mode-doc-ja/org-ja.git
and obtain these 2 files at the directory of "/path/to/org-ja".
1. org.texi
2. org-ja.po

Insert these elisp code into .emacs and make the variable my-proj-path fit to your system.

    ;;; for project org-ja to translate with po file and texi fiel, by using po-mode and texinfo-mode.
    (add-to-list 'load-path (expand-file-name "/path/to/assist-po-mode/"))
    (require 'assist-po-mode)
    
    (setq my-proj-path "/path/to/org-ja")
    (setq my-texi-file-name "org.texi")
    (setq my-po-file-directory (concat my-proj-path "/"))
    (setq my-po-file-name "org-ja.po")
    
    (defun proj-translation-org2ja ()
      "The batch process to start translating org2ja."
      (interactive)
      (progn (find-file (concat my-po-file-directory my-po-file-name))
        (find-file (concat my-po-file-directory my-texi-file-name))
    	 (texinfo-show-structure)))

