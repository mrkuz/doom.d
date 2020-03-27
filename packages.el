;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Org mode
(package! ox-reveal)
(package! org-reverse-datetree)
(package! org-sticky-header)
(package! org-present
  :recipe (:host github :repo "rlister/org-present"))

;; Completion
(package! company-quickhelp)

;; Navigation
(package! ace-link)
(package! goto-last-change)

;; Miscellanoues
(package! bm)
(package! flycheck-pos-tip)
(package! google-this)
(package! impatient-mode)
(package! pandoc-mode)
(package! string-inflection)
