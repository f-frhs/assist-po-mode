# asssit-po-mode

## What is assist-po-mode?

This program displays the corresponding line of po file to texi file in the Emacs buffer.
This program also displays the crresponding line of texi file to po file.

### Screenshots

![screenshot-jump-to-po-file](https://cacoo.com/diagrams/AVAmgqMyVsValX2g-1A11D.png?width=600)

## Installation
see the section ("For member of org-ja") or assist-po-mode.el.

## Key bindings
see assist-po-mode.el.

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

### Usage
After pressing M-x proj-translation-org2ja,
Pressing C-c C-s (texinfo-show-structure) in org.texi buffer displayes document structure in another buffer.
![screenshot-obtain-document-sturucture](https://cacoo.com/diagrams/b0ePWd9YDa7GWGuN-CA91A.png)

Now, let's assume that you will translate the chapter "Working with source code",  you can find the chapter to translate by pressing
    C-s working wi
in the \*Occur\* buffer because of incremental search.
By pressing C-m (or ENTER) at the 219th line in the \*Occur\* buffer

    218:  11183:    @section Triggering publication
    219:  11210:@chapter Working with source code
    210:  11254:    @section Structure of code blocks

, you can jump the corresponding line in org.texi buffer.
![screenshot-jump-from-TOC-to-texi-file](https://cacoo.com/diagrams/8oeHU91mLTl04ZLc-0E9D9.png)

Move to the next paragraph and press C-j to jump to the corresponding line of org-ja.po.
![screenshot from texi fileto po file](https://cacoo.com/diagrams/2XcQLQY3Bh17J4WN-34B97.png)
In this case, you can jump to 23253rd line containing

    #: org.texi:11217

in org-ja.po buffer, by pressing C-j at 11214th - 11216th lines in org.texi buffer.

    11213: @cindex source code, working with
    11214: 
    11215: Source code can be included in Org-mode documents using a @samp{src} block,
    11216: e.g.@:
    11217: 
    11218: @example

Next, pressing n in org-ja.po buffer lead you to next paragraph which contains
    #: org.texi: 11232
and you will find the org.texi buffer also shows corresponding line, that is 11232nd line, as if the org.texi buffer follows org-ja.po buffer.
![screenshot-follow-mode-after-movement-in-po-file](https://cacoo.com/diagrams/iz850PbIruJWmyYP-B26F7.png)
You can find the same phenomena by pressing p (previous) and . (current).

This is all that assist-po-mode.el provides.
