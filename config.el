;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal information

(setq user-full-name "Markus")
(setq user-mail-address "markus@bitsandbobs.net")
(setq smtpmail-smtp-user user-mail-address)
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-service 465)

;; Miscellaneous

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type t)

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

;; Functions

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
