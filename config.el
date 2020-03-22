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
;; Start indendet
(setq org-startup-indented t)

;; Calendar

;; Start week with monday
(setq calendar-week-start-day 1)
;; Use ISO date format
(setq calendar-date-style 'iso)
;; Configure holidays
(setq calendar-holidays (append holiday-general-holidays holiday-christian-holidays))

;; Miscellaneous

;; Display line numbers
(setq display-line-numbers-type t)
;; Show buffer name in frame title
(setq-default frame-title-format '("Emacs - %b"))

;; Modeline

;;;; Show only calendar week
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

;; Packages

(after! company
  ;; Show completion after short delay
  (setq company-idle-delay 0.4)
  ;; Show numbers to select completion
  (setq company-show-numbers t)
  ;; Limit results
  (setq company-tooltip-limit 9))

(after! gcmh
  (setq gcmh-low-cons-threshold 80000000)
  (setq gcmh-high-cons-threshold most-positive-fixnum))

(after! anzu
  (global-anzu-mode))

(after! treemacs
  ;; Customize face of root item
  (set-face-attribute 'treemacs-root-face nil :height 1.0 :underline nil)
  ;; Customize root icon
  (setq treemacs-icon-root-png
        (concat " "
                (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2 :face 'font-lock-string-face)
                " ")))

(use-package! dashboard
  :init
  ;; Configure dashobard items
  (setq dashboard-items '((agenda . 5) (recents . 5) (projects . 5)))
  (setq dashboard-set-footer nil)
  (dashboard-setup-startup-hook))

(use-package! company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config
  ;; Configure delay util help is shown
  (setq company-quickhelp-delay 1.2)
  ;; Limit nubmer of lines
  (setq company-quickhelp-max-lines 20)
  ;; Allow colors and fonts
  (setq company-quickhelp-use-propertized-text t))

;; Key bindings

(setq my-map (make-sparse-keymap))
(global-set-key (kbd "C-;") my-map)
(global-set-key (kbd "C-ö") my-map)
(define-key my-map (kbd "f c") 'my-open-config)
(define-key my-map (kbd "f j") 'my-open-journal)
(define-key my-map (kbd "f n") 'my-open-notes)
(define-key my-map (kbd "f t") 'my-open-todos)
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
