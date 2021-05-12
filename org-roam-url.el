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

;;;; Vars
(defconst helm-org-url-fontify-buffer-name " *helm-org-url-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defcustom org-roam-url-completion-system 'helm
  "The completion system to be used by `org-roam-url'."
  :type '(radio
          (const :tag "Default" default)
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)
          (function :tag "Custom function"))
  :group 'org-roam-url)

(defcustom org-roam-url-max-depth 3
  "The max depth of url examined by `org-roam-url'."
  :type '(integer)
  :group 'org-roam-url)

(defcustom org-roam-url-max-results 30
  "The max number of results returned through querying database."
  :type '(integer)
  :group 'org-roam-url)

(defcustom org-roam-url-stop-on-first-result 't
  "Stops after there is a result or depth which ever one comes first."
  :type '(boolean)
  :group 'org-roam-url)

(defcustom org-roam-url-auto-complete-on-single-result 't
  "Auto complete if their is only a single result."
  :type '(boolean)
  :group 'org-roam-url)

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
           ((eq org-roam-url-completion-system 'ido)
            (let ((candidates (mapcar #'car choices)))
              (ido-completing-read prompt candidates nil require-match initial-input)))
           ((eq org-roam-url-completion-system 'default)
            (completing-read prompt choices nil require-match initial-input))
           ((eq org-roam-url-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :require-match require-match
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'org-roam--completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           ((eq org-roam-url-completion-system 'helm)
            (unless (and (fboundp 'helm)
                         (fboundp 'helm-make-source))
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
            (let ((source (helm-build-sync-source prompt
                            :candidates (mapcar (-compose #'helm-org-url-fontify-like-in-org-mode #'car) choices)
                            :multiline t
                            :volatile t
                            :match 'identity
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

;;; progressive
(defun org-roam-url--url-components (url)
  "Take URL and return a reversed url list split on /."
  (let ((url-head (s-replace-regexp "http[s]?://"  "" url)))
    (reverse (split-string url-head "/"))))

(defun org-roam-url--progressive-paths (url-comp)
  "Take a list of URL-COMP and generate upto `org-roam-url-max-depth'."
  (org-roam-url--progressive-paths-helper url-comp org-roam-url-max-depth))

(defun org-roam-url--progressive-paths-helper (url-comp depth)
  "Take a list of URL-COMP and generate upto DEPTH."
  (pcase (list url-comp depth)
    (`((,_ . ,_) 0) (list url-comp))
                                        ;(`(,first ,second ,third) `(("%%" ,first ,second ,third)))
    (`((,_ . ,tail) ,n) (cons (copy-seq url-comp) (org-roam-url--progressive-paths-helper (copy-seq tail) (- n 1))))
    (`(nil ,_) `nil)))

(defun org-roam-url--to-url-list (url-list)
  "Take a list of URL-LIST and turn into a list of urls."
  (mapcar (lambda (x) (reduce
                       (lambda (y z) (concat z "/" y)) x))
          url-list))

(defun org-roam-url--cap-url (url-list)
  "Prepend urls in URL-LIST with //."
  (mapcar (lambda (x) (concat "//" x)) url-list))

(defun org-roam-url--term-url (url-list)
  "Suffix urls in URL-LIST with %%."
  (mapcar (lambda (x) (concat x "%%")) url-list))

(defun org-roam-url--progressive-urls (url)
  "Turn a URL into a list of progressive url paths."
  (-as-> url it
         (org-roam-url--url-components it)
         (org-roam-url--progressive-paths it)
         (org-roam-url--to-url-list it)
         (org-roam-url--cap-url it)
         (org-roam-url--term-url it)))

(defun org-roam-url-db--query-files (url-path)
  "Find files containing a url that is like URL-PATH."
  (org-roam-db-query  [:select [links:properties files:file titles:title tags:tags files:meta] :from links
                       :left :join titles
                       :on (= links:source titles:file)
                       :left :join tags
                       :on (= titles:file tags:file)
                       :left :join files
                       :on (= titles:file files:file)
                       :where (like links:dest $s1)
                       :order-by (asc links:source)
                       :limit $s2
                       ] url-path org-roam-url-max-results))

(defun org-roam--get-url-place-title-path-completions-progressively (url)
  "Return title path completions for URL progressively.
Find files that contain a portion of URL.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* ((rows '())
         completions)
    (dolist (progressive-url (org-roam-url--progressive-urls url))
      (if (not (and rows org-roam-url-stop-on-first-result))
        (setq rows (append rows (org-roam-url-db--query-files progressive-url)))))
    ;; sort by point in file
    (setq rows (delete-dups rows))
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
          (push (cons k v) completions))))))

;;; common tools
(defun helm-org-url-fontify-like-in-org-mode (s &optional odd-levels)
"Fontify string S like in Org-mode.

stripped from org-rifle.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
(let ((buffer (get-buffer helm-org-url-fontify-buffer-name)))
  (unless buffer
    (setq buffer (get-buffer-create helm-org-url-fontify-buffer-name))
    (with-current-buffer buffer
      (org-mode)))
  (with-current-buffer buffer
    (erase-buffer)
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      ;; FIXME: "Warning: ‘font-lock-fontify-buffer’ is for interactive use only; use
      ;; ‘font-lock-ensure’ or ‘font-lock-flush’ instead."
      (font-lock-fontify-buffer)
      (buffer-string)))))

(cl-defun org-roam-find-file-url (initial-prompt completions &key filter-fn no-confirm setup-fn)
  "Find and open an Org-roam file.
  INITIAL-PROMPT is the initial title prompt.
  COMPLETIONS is a list of completions to be used instead of
  `org-roam--get-title-path-completions`
  FILTER-FN is the name of a function to apply on the candidates
  which takes as its argument an alist of path-completions.  See
  `org-roam--get-title-path-completions' for details.
  If NO-CONFIRM, assume that the user does not want to modify the initial prompt.
  Call SETUP-FN before conducting completion. Useful to focus Emacs."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               completions))
         (title-with-tags (pcase (list (length completions) (not org-roam-url-auto-complete-on-single-result))
                            (`(0 . cdr) nil)
                            (`(1 nil) (progn (when setup-fn (funcall setup-fn)) (caar completions)))
                            (_ (if no-confirm
                                   initial-prompt
                                 (when setup-fn (funcall setup-fn))
                                 (org-roam-completion--completing-read-url "File: " completions
                                                                           :initial-input initial-prompt)))))
         (res (cdr (assoc title-with-tags completions)))
         (file-path  (plist-get res :path))
         (point  (plist-get res :point)))
    (if file-path
        (org-roam-with-file file-path 't
          (let ((win (get-buffer-window buf 't)))
            (if win
                (select-window win)
              (switch-to-buffer buf)) ; If FILE is already visited, find buffer
              (goto-char point)
              (org-show-context)
              't)))))

(defun org-roam-protocol-open-url (info)
  "Process an org-protocol://roam-url?ref= style url with INFO.

  It checks, opens, searchs or creates a note with the given ref.

When check is available in url, no matter what it is set to, just check if file exists, if not don't open anything or create org file.

    javascript:location.href = \\='org-protocol://roam-url?template=r&ref=\\='+ \\
          encodeURIComponent(location.href) + \\='&title=\\=' \\
          encodeURIComponent(document.title) + \\='&body=\\=' + \\
          encodeURIComponent(window.getSelection()) + \\ + \\='&check=\\=' + anything + \\='&progressive=\\=' anything
"
  (let* ((ref (plist-get info :ref))
  (check (plist-get info :check))
  (org-roam-url-auto-complete-on-single-result (not (plist-get info :noauto)))
  (progressive (plist-get info :progressive))
  (opened-file (if progressive
                   (org-roam-find-file-url nil (org-roam--get-url-place-title-path-completions-progressively ref) :setup-fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame))))
                   (org-roam-find-file-url nil (org-roam--get-url-place-title-path-completions ref) :setup-fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame)))))))
  (unless (or check opened-file)
    (org-roam-protocol-open-ref info)
    )))

(push '("org-roam-url"  :protocol "roam-url"   :function org-roam-protocol-open-url)
      org-protocol-protocol-alist)
(provide 'org-roam-url)
;;; org-roam-url.el ends here
