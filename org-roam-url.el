;;; org-roam-url.el --- Url Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Natnael Kahssay <thisnkk@gmail.com>
;; Author: Natnael Kahssay <thisnkk@gmail.com>
;; URL: https://github.com/natask/org-roam
;; Keywords: org-mode, roam, convenience, url
;; Version: 1.2.2
;; Package-Requires: ((emacs "26.1") (org "9.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; We extend org-protocol, adding custom Org-roam handlers. The setup
;; instructions for `org-protocol' can be found in org-protocol.el.
;;
;; We define 1 protocol:
;;
;; 1. "roam-url": This protocol opens a list of files that link to url. directly creates a note using "roam-ref" protocol otherwise.
;;
;;; Code:
(require 'org-protocol)
(require 'org-roam-protocol)
(require 'org-roam)
;;;; Functions

(defun org-roam--get-url-title-path-completions (url)
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* ((url-path (s-replace-regexp "http[s]?:"  "" url))
         (rows (org-roam-db-query  [:select [files:file titles:title tags:tags files:meta] :from titles
                                 :left :join tags
                                 :on (= titles:file tags:file)
                                 :left :join files
                                 :on (= titles:file files:file)
                                 :left :join links
                                 :on (= files:file links:source)
                                 :where (= links:dest $s1)
                                ] url-path))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--add-tag-string title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))


(defun org-roam--prepend-url-place (props title file-from tags)
  (concat (org-roam--add-tag-string title tags) " :" (number-to-string (plist-get props :point)) ":"
                         "\n"
                         "* "
                         (if-let ((outline (plist-get props :outline)))
                             (string-join outline " > ")
                           "Top")
                         "\n"
                          "=> " (s-trim (s-replace "\n" " "
                                                   (funcall org-roam-buffer-preview-function file-from (plist-get props :point))))
                         "\n\n"
                         ))

(defun org-roam--get-url-place-title-path-completions (url)
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* ((url-path (s-replace-regexp "^http[s]?:"  "" url))
         (rows (org-roam-db-query  [:select [links:properties files:file titles:title tags:tags files:meta] :from links
                                  :left :join titles
                                 :on (= links:source titles:file)
                                 :left :join tags
                                 :on (= titles:file tags:file)
                                 :left :join files
                                 :on (= titles:file files:file)
                                 :where (= links:dest $s1)
                                 :order-by (asc links:source)
                                ] url-path))
         completions)
    ;; sort by point in file
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 0 x) :point))
                            #'<
                            rows))
    ;; then by file opening time
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 4 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,props ,file-path ,title ,tags) row))
        (let ((k (org-roam--prepend-url-place props title file-path tags))
              (v (list :path file-path :title title :point (plist-get props :point))))
          (push (cons k v) completions))))
    ))

(cl-defun org-roam-completion--completing-read-url (prompt choices &key
                                                       require-match initial-input
                                                       action)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
Return user choice."
  (let (res)
    (setq res
          (cond
           ((eq org-roam-completion-system 'ido)
            (let ((candidates (mapcar #'car choices)))
              (ido-completing-read prompt candidates nil require-match initial-input)))
           ((eq org-roam-completion-system 'default)
            (completing-read prompt choices nil require-match initial-input))
           ((eq org-roam-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :require-match require-match
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'org-roam--completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           ((eq org-roam-completion-system 'helm)
            (unless (and (fboundp 'helm)
                         (fboundp 'helm-make-source))
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
            (let ((source (helm-make-source prompt 'helm-source-sync
                            :candidates (mapcar #'car choices)
                            :multiline t
                            :filtered-candidate-transformer
                            (and (not require-match)
                                 #'org-roam-completion--helm-candidate-transformer)))
                  (buf (concat "*org-roam "
                               (s-downcase (s-chop-suffix ":" (s-trim prompt)))
                               "*")))
              (or (helm :sources source
                        :action (if action
                                    (prog1 action
                                      (setq action nil))
                                  #'identity)
                        :prompt prompt
                        :input initial-input
                        :buffer buf)
                  (keyboard-quit))))))
    (if action
        (funcall action res)
      res)))

(defun org-roam-find-file-url (initial-prompt completions &optional filter-fn no-confirm setup-fn)
  "Find and open an Org-roam file.
  INITIAL-PROMPT is the initial title prompt.
  COMPLETIONS is a list of completions to be used instead of
  `org-roam--get-title-path-completions`
  FILTER-FN is the name of a function to apply on the candidates
  which takes as its argument an alist of path-completions.  See
  `org-roam--get-title-path-completions' for details.
  If NO-CONFIRM, assume that the user does not want to modify the initial prompt."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               completions))
         (title-with-tags (pcase (length completions)
                             (0 nil)
                             (1 (progn (when setup-fn (funcall setup-fn)) (caar completions)))
                             (_ (if no-confirm
                             initial-prompt
                             (when setup-fn (funcall setup-fn))
                             (org-roam-completion--completing-read-url "File: " completions
                                                                       :initial-input initial-prompt)))))
         (res (cdr (assoc title-with-tags completions)))
         (file-path  (plist-get res :path))
         (point  (plist-get res :point)))
    (if file-path
        (progn (find-file file-path) (goto-char point) '(t))
      nil)
    ))

(defun org-roam-protocol-open-url (info)
  "Process an org-protocol://roam-url?ref= style url with INFO.

  It checks, opens, searchs or creates a note with the given ref.

When check is available in url, no matter what it is set to, just check if file exists, if not don't open anything or create org file.

    javascript:location.href = \\='org-protocol://roam-url?template=r&ref=\\='+ \\
          encodeURIComponent(location.href) + \\='&title=\\=' \\
          encodeURIComponent(document.title) + \\='&body=\\=' + \\
          encodeURIComponent(window.getSelection()) + \\ + \\='&check=\\='
"
  (let* (
  (ref (plist-get info :ref))
  (check (plist-get info :check))
  (opened-file (org-roam-find-file-url nil (org-roam--get-url-place-title-path-completions ref) nil nil (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame))))))
  (unless (or check opened-file)
    (org-roam-protocol-open-ref info)
    )
  ))

(push '("org-roam-url"  :protocol "roam-url"   :function org-roam-protocol-open-url)
      org-protocol-protocol-alist)
(provide 'org-roam-url)
;;; org-roam-url.el ends here
