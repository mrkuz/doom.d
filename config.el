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
