;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal information

(setq user-full-name "Markus")
(setq user-mail-address "markus@bitsandbobs.net")
(setq smtpmail-smtp-user user-mail-address)
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-service 465)

;; Doom

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14))
(setq doom-theme 'doom-one)

;; Org

(setq org-directory "~/org/")
(after! org
  ;; Set up diary file
  (setq diary-file "~/org/diary")
  ;; Include diary in agenda
  (setq org-agenda-include-diary t)
  ;; Don't indent lines after editing
  (setq org-edit-src-content-indentation 0)
  ;; Insert log notes into drawer
  (setq org-log-into-drawer t)
  ;; TODO keywords
  (setq org-todo-keywords '((sequence "TODO" "WIP" "|" "DONE" "MOVED" "CANCELED")))
  ;; Add CLOSED timestamp to DONE items
  (setq org-log-done 'time)
  ;; Allow setting refile targets as local file variable
  (put 'org-refile-targets 'safe-local-variable (lambda (_) t))
  ;; Add files to the agenda
  (setq org-agenda-files '("~/org" "~/org/mobile"))
  ;; Set archive file
  (setq org-archive-location "~/org/.archive.org::* File: %s")
  ;; No header in archive file
  (setq org-archive-file-header-format nil)
  ;; Custom templates
  (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp :results none\n?\n#+END_SRC"))
  ;; Custom capture templates
  (setq org-capture-templates '(
                                ("b" "Backlog" entry (file+olp "todo.org" "Backlog")
                                 "* TODO %:link%?" :prepend t)
                                ("n" "Next" entry (file+olp "todo.org" "Next")
                                 "* TODO %?" :prepend t)
                                ("t" "Todo" entry (file+function "todo.org" org-reverse-datetree-goto-read-date-in-file)
                                 "* TODO %?\nSCHEDULED: <%(org-read-date nil nil org-read-date-final-answer)>")
                                ("N" "Note" entry (file+olp+datetree "notes.org")
                                 "** %<%H:%M> %:link%?")
                                ("J" "Journal" entry (file+olp+datetree "journal.org")
                                 "** %<%H:%M> %?")
                                )))

(use-package! org-reverse-datetree
  :after org
  :config
  ;; Customize defaut date tree format
  (setq org-reverse-datetree-date-format "%Y-%m-%d %A")
  (setq org-reverse-datetree-week-format "%Y-%m KW%V")
  (setq org-reverse-datetree-year-format "%Y"))

(use-package! org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-always-show-header nil)
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-outline-path-separator " → "))

(defun my-org-protocol-capture-hook ()
  (let ((name (cdr (assoc 'name (frame-parameters)))))
    (when (equal name "org-protocol-capture")
      (delete-other-windows)
      (select-frame-set-input-focus (selected-frame)))))

(defun my-org-protocol-after-capture-hook ()
  (let ((name (cdr (assoc 'name (frame-parameters)))))
    (when (equal name "org-protocol-capture")
      (delete-frame))))

(add-hook 'org-capture-mode-hook 'my-org-protocol-capture-hook)
(add-hook 'org-capture-after-finalize-hook 'my-org-protocol-after-capture-hook)

;; Terminal

(after! multi-term
  :config
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode)))

;; Calendar

;; Start week with monday
(setq calendar-week-start-day 1)
;; Use ISO date format
(setq calendar-date-style 'iso)
;; Configure holidays
(setq calendar-holidays (append holiday-general-holidays holiday-christian-holidays))

;; Miscellaneous

;; Enable all disabled commands
(setq disabled-command-hook nil)
;; Display line numbers
(setq display-line-numbers-type t)
;; Show buffer name in frame title
(setq-default frame-title-format '("Emacs - %b"))
;; Use bash as default shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)
;; Set fill column
(setq-default fill-column 100)

;; Modeline

;; Show only calendar week
(setq display-time-format " W%V")
;; Hide system load
(setq display-time-default-load-average nil)
;; Show date/time in modeline
(display-time-mode 1)

;; Buffers

(setq ibuffer-use-other-window t)
(map! "C-x b" #'ivy-switch-buffer)
(map! "C-x B" #'+ivy/switch-workspace-buffer)
(map! "C-x C-b" #'ibuffer)

;; Navigation

(after! ace-window
  :config
  ;; Use letters instead of numbers
  (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h)))

;; Auto completion

(after! company
  ;; Show completion after short delay
  (setq company-idle-delay 0.4)
  ;; Show numbers to select completion
  (setq company-show-numbers t)
  ;; Limit results
  (setq company-tooltip-limit 9))

(use-package! company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config
  ;; Configure delay util help is shown
  (setq company-quickhelp-delay 1.2)
  ;; Limit nubmer of lines
  (setq company-quickhelp-max-lines 20)
  ;; Allow colors and fonts
  (setq company-quickhelp-use-propertized-text t))

;; Other packages

(after! gcmh
  (setq gcmh-low-cons-threshold 80000000)
  (setq gcmh-high-cons-threshold most-positive-fixnum))

(after! anzu
  (global-anzu-mode))

(after! treemacs
  ;; Collapse directories
  (setq treemacs-collapse-dirs 10)
  ;; Customize face of root item
  (set-face-attribute 'treemacs-root-face nil :height 1.0 :underline nil)
  ;; Customize root icon
  (setq treemacs-icon-root-png
        (concat " "
                (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2 :face 'font-lock-string-face)
                " ")))

(after! flyspell
  (unbind-key "C-;" flyspell-mode-map))

(after! plantuml-mode
  (setq plantuml-jar-path "~/opt/plantuml/plantuml.jar")
  (setq org-plantuml-jar-path plantuml-jar-path))

;; Google thing-at-point
(use-package! google-this
  :commands google-this)

;; Go to last change
(use-package! goto-last-change
  :commands goto-last-change)

;; Visual bookmarks
(use-package! bm
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle))
)

;; Pandoc integration
(use-package! pandoc-mode
  :commands pandoc-mode)

;; Simple presentations
(use-package! org-present
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;; Reveal.js
(use-package! ox-reveal
  :after ox
  :config
  (setq org-reveal-root "file:///~/opt/reveal.js/"))

;; Deft
(use-package! deft
  :config
  (setq deft-directory "~/Notes/deft/")
  (setq deft-use-filter-string-for-filename t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase))))

;; Key bindings

(setq my-map (make-sparse-keymap))
(global-set-key (kbd "C-;") my-map)
(global-set-key (kbd "C-ö") my-map)
(define-key my-map (kbd "M-c") 'string-inflection-lower-camelcase)
(define-key my-map (kbd "M-C") 'string-inflection-camelcase)
(define-key my-map (kbd "M-k") 'string-inflection-kebab-case)
(define-key my-map (kbd "M-u") 'string-inflection-underscore)
(define-key my-map (kbd "M-U") 'string-inflection-upcase)
(define-key my-map (kbd "M-x") 'string-inflection-all-cycle)
(define-key my-map (kbd "d") 'deft)
(define-key my-map (kbd "f c") 'my-open-config)
(define-key my-map (kbd "f j") 'my-open-journal)
(define-key my-map (kbd "f n") 'my-open-notes)
(define-key my-map (kbd "f t") 'my-open-todos)
(define-key my-map (kbd "l g") 'google-this)
(define-key my-map (kbd "s p") 'goto-last-change)
(define-key my-map (kbd "s w") 'avy-goto-line)
(define-key my-map (kbd "s w") 'avy-goto-word-1)
(define-key my-map (kbd "s W") 'ace-window)
(define-key my-map (kbd "t t") 'toggle-truncate-lines)
(define-key my-map (kbd "t a") 'goto-address-mode)
(define-key my-map (kbd "t w") 'whitespace-mode)
(define-key my-map (kbd "T") 'multi-term)
(define-key my-map (kbd "v d") 'git-gutter:popup-hunk)
(define-key my-map (kbd "v g") 'magit-file-dispatch)
(define-key my-map (kbd "v G") 'magit-dispatch)
(define-key my-map (kbd "TAB") 'fold/toggle)
(define-key my-map (kbd "1") (lambda () (interactive ) (my-workspace-switch "main")))
(define-key my-map (kbd "2") (lambda () (interactive ) (my-workspace-switch "2")))
(define-key my-map (kbd "3") (lambda () (interactive ) (my-workspace-switch "3")))
(define-key my-map (kbd "4") (lambda () (interactive ) (my-workspace-switch "4")))
(define-key my-map (kbd "5") (lambda () (interactive ) (my-workspace-switch "5")))
(define-key my-map (kbd "6") (lambda () (interactive ) (my-workspace-switch "6")))
(define-key my-map (kbd "7") (lambda () (interactive ) (my-workspace-switch "7")))
(define-key my-map (kbd "8") (lambda () (interactive ) (my-workspace-switch "8")))
(define-key my-map (kbd "9") (lambda () (interactive ) (my-workspace-switch "9")))
(define-key my-map (kbd ".") 'dumb-jump-go)
(define-key my-map (kbd ",") 'dumb-jump-back)
(define-key my-map (kbd "$ ;") 'flyspell-auto-correct-previous-word)
(define-key my-map (kbd "& i") 'company-yasnippet)
(define-key my-map (kbd "& t") 'yas-describe-tables)
(define-key my-map (kbd "?") 'which-key-show-top-level)
(define-key my-map (kbd "SPC") 'company-complete)

;; Functions

(defun my-create-scratch (name)
  "Create a new scratch buffer."
  (interactive)
  (let ((scratch (generate-new-buffer (concat "*scratch<" name ">*"))))
    (switch-to-buffer scratch)
    (funcall initial-major-mode)
    scratch))

(defun my-workspace-switch (name)
  (if (+workspace-exists-p name)
      (+workspace-switch name)
    (progn
      (+workspace-switch name t)
      (my-create-scratch name))))

(defun my-open-config ()
  "Open config file."
  (interactive)
  (find-file "~/.doom.d/config.el"))

(defun my-open-journal ()
  "Open journal."
  (interactive)
  (find-file (concat org-directory "/journal.org")))

(defun my-open-notes ()
  "Open notes."
  (interactive)
  (find-file (concat org-directory "/notes.org")))

(defun my-open-todos ()
  "Open todos."
  (interactive)
  (find-file (concat org-directory "/todo.org")))
