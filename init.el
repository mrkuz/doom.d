;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       :completion
       company             ; the ultimate code completion backend
       (ivy +fuzzy +prescient) ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom                ; what makes DOOM look the way it does
       ;;fill-column       ; a `fill-column' indicator
       ;;hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       modeline            ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink the current line after jumping
       ;;ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;pretty-code       ; replace bits of code with pretty symbols
       treemacs            ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter           ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       ;;file-templates    ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
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
       ;;vterm             ; another terminals in Emacs

       :checkers
       ;;syntax            ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;(eval +overlay)   ; run code, run (also, repls)
       ;;lookup            ; navigate your code and its documentation
       ;;lsp
       magit               ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :lang
       ;;data              ; config/data formats
       ;;emacs-lisp        ; drown in parentheses
       ;;go                ; the hipster dialect
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;markdown          ; writing docs for people to ignore
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;(org              ; organize your plain life in plain text
        ;;+dragndrop       ; drag & drop files/images into org buffers
        ;;+hugo            ; use Emacs for hugo blogging
        ;;+pandoc          ; export-with-pandoc support
        ;;+present)        ; using org-mode for presentations
       ;;plantuml          ; diagrams for confusing people more
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes

       :email
       :app
       ;;calendar

       :config
       ;;literate
       (default +bindings +smartparens))
