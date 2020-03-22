;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       :completion
       company             ; the ultimate code completion backend
       (ivy +fuzzy +prescient) ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom                ; what makes DOOM look the way it does
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline            ; snazzy, Atom-inspired modeline, plus API
       treemacs            ; a project drawer, like neotree but cooler
       vc-gutter           ; vcs diff in the fringe
       workspaces          ; tab emulation, persistence & separate workspaces

       :editor
       ;;file-templates    ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;multiple-cursors  ; editing in many places at once
       ;;rotate-text       ; cycle region at point between text candidates
       ;;snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired               ; making dired pretty [functional]
       ;;electric          ; smarter, keyword-based electric-indent
       ibuffer             ; interactive buffer management
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs

       :checkers
       ;;syntax            ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;(eval +overlay)   ; run code, run (also, repls)
       ;;lookup            ; navigate your code and its documentation
       magit               ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pdf               ; pdf enhancements
       ;;rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :lang
       data                ; config/data formats
       emacs-lisp          ; drown in parentheses
       go                  ; the hipster dialect
       java                ; the poster child for carpal tunnel syndrome
       javascript          ; all(hope(abandon(ye(who(enter(here))))))
       markdown            ; writing docs for people to ignore
       (org)               ; organize your plain life in plain text
        ;;+dragndrop       ; drag & drop files/images into org buffers
        ;;+hugo            ; use Emacs for hugo blogging
        ;;+pandoc          ; export-with-pandoc support
        ;;+present)        ; using org-mode for presentations
       ;;plantuml          ; diagrams for confusing people more
       rest                ; Emacs as a REST client
       (sh +fish)          ; she sells {ba,z,fi}sh shells on the C xor
       web                 ; the tubes

       :email
       :app
       :config
       ;;literate
       (default +bindings +smartparens))
