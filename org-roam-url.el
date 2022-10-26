;;; org-roam-url.el --- Url Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Natnael Kahssay <thisnkk@gmail.com>
;; Author: Natnael Kahssay <thisnkk@gmail.com>
;; URL: https://github.com/natask/org-roam
;; Keywords: org-mode, roam, convenience, url
;; Version: 1.2.2
;; Package-Requires: ((emacs "27.1") (org "9.3") (s "1") (org-roam "2"))

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
(require 's)
(require 'helm)

;;;; Vars
(defconst org-roam-url-fontify-buffer-name " *org-url-fontify*"
  "The name of the invisible buffer used to fontify `org-mode' strings.")

(defcustom org-roam-url-max-depth 3
  "The max depth of url examined by `org-roam-url'."
  :type '(integer)
  :group 'org-roam-url)

(defcustom org-roam-url-max-results 300
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

(defun org-roam-url--prepend-place (props title file-from tags) ;TODO: implement
  "NOT IN USE"
  (concat (org-roam--add-tag-string title tags) " :" (number-to-string (plist-get props :point)) ":"
          "\n"
          "* "
          (if-let ((outline (plist-get props :outline)))
              (string-join outline " > ")
            "Top")
          "\n"
          "=> " (s-trim (s-replace "\n" " "
                                   (funcall org-roam-buffer-preview-function file-from (plist-get props :point))))
          "\n\n"))

(defun org-roam-url--fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode.

stripped from org-rifle.

`org-fontify-like-in-org-mode' is a very, very slow function
because it creates a new temporary buffer and runs `org-mode' for
every string it fontifies.  This function reuses a single
invisible buffer and only runs `org-mode' when the buffer is
created."
  (let ((buffer (get-buffer org-roam-url-fontify-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create org-roam-url-fontify-buffer-name))
      (with-current-buffer buffer
        (org-mode)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert s)
      (let ((org-odd-levels-only odd-levels))
        ;; FIXME: "Warning: ‘font-lock-fontify-buffer’ is for interactive use only; use
        ;; ‘font-lock-ensure’ or ‘font-lock-flush’ instead."
        (font-lock-ensure)
        (buffer-string)))))

(defun org-roam-url--nodes-to-candidates (nodes)
  (mapcar
   (lambda (node)
     (cons
      (concat (reduce (lambda (x y) (concat x "\n" y)) `(,(org-roam-node-title node) ,@(org-roam-node-aliases node)))
              "\n" (concat "tags: " (org-make-tag-string (org-roam-node-tags node)))
              "\n" (concat "olp+: " (combine-and-quote-strings (cons (org-roam-node-file-title node) (org-roam-node-olp node))))
              "\n" (concat "outline: " (combine-and-quote-strings (plist-get (org-roam-node-properties node) :outline))))
      node))
   nodes))

(defvar org-roam-url--kill-buffers-list 'nil
  "List of buffers to kill when completion framework exits.
buffers opened using persistent-action.")

(cl-defun org-roam-url-completion--completing-read (prompt choices &key
                                                           require-match initial-input)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
Return user choice."
  (let ((nodes  (org-roam-url--nodes-to-candidates choices)))
    (let ((source (helm-build-sync-source prompt
                    :candidates nodes
                    :multiline t
                    :volatile t
                    :match 'identity
                    :persistent-action  #'(lambda (candidate)
                                            (let ((condition (find-buffer-visiting (org-roam-node-file candidate)))
                                                  (buffer (org-roam-node-visit candidate 't)))
                                              (unless condition
                                                (add-to-list 'org-roam-url--kill-buffers-list buffer))))))
          (buf (concat "*org-roam-url "
                       (s-downcase (s-chop-suffix ":" (s-trim prompt)))
                       "*")))
      (or
       (let ((res (helm :sources source
                        :input initial-input
                        :prompt prompt
                        :buffer buf)))
         (dolist (buf org-roam-url--kill-buffers-list)
           (kill-buffer buf))
         (setq org-roam-url--kill-buffers-list 'nil)
         res)
       (keyboard-quit)))))

;;; progressive
(defun org-roam-url--url-components (url)
  "Take URL and return a reversed url list split on /.
Treats & and # as url seperators when they occur on the final path.
Example:
https://google.com/search&=happy#complete
(google.com google.com/search google.com/search&=happy google.come/search&=happy#complete)"
  (let ((url-head (s-replace-regexp ".*http[s]?://"  "" url)))
    (-as-> (split-string url-head "/") it
           (mapcar (lambda (x) (list x "/")) it)
           (reverse it)
           (let ((before it))
             (-as-> (caar it) string
                    (let ((index 0)
                          (next-index 0)
                          res)
                      (while (and (setq next-index (string-match "[&#]" string
                                                                 (if (and (not (equal index 0))
                                                                          (< index (length string)))
                                                                     (1+ index) index)))
                                  (< index (length string)))
                        (push (list (substring string index next-index) (substring string  next-index (+ 1 next-index))) res)
                        (setq index (+ 1 next-index)))
                      (if (< index  (length string))
                          (push (list (substring string index) "") res))
                      res)
                    (append string (cdr before)))))))

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
  (mapcar (lambda (x) (cl-reduce
                       (lambda (y z) (concat (apply 'concat z) (if (listp y) (apply 'concat y) y))) (cdr x) :initial-value (caar x)))
          url-list))

(defun org-roam-url--term-url (url-list)
  "Suffix urls in URL-LIST with %%."
  (mapcar (lambda (x) (concat "%%" x "%%")) url-list))

(defun org-roam-url--progressive-urls (url)
  "Turn a URL into a list of progressive url paths."
  (-as-> url it
         (org-roam-url--url-components it)
         (org-roam-url--progressive-paths it)
         (org-roam-url--to-url-list it)
         (org-roam-url--term-url it)
         (cons url it)))

(defun org-roam-url--query (url-path)
  "Find files containing a url that is like URL-PATH.
Return `org-roam-search-max' nodes stored in the database containg URL-PATH as dest as a list of `org-roam-node's."
  (when-let* ((nodeids-extra (-some--> url-path
                                    `[:select :distinct [source pos]
                                      :from links
                                      :where (like dest ,url-path)
                                      :limit ,org-roam-url-max-results]
                                    (org-roam-db-query it)))
              (nodeids (cl-loop for nodeid-extra in nodeids-extra
                                     collect (car nodeid-extra)))
              (where-clause (-some--> nodeids
                              (mapcar (lambda (nodeid)
                                        `(= id ,nodeid)) it)
                              (cons 'or it)
                              (emacsql-prepare `[:where ,it])
                              (car it)))
              (org-roam-db-super-main-clause
               "
SELECT id, file, filetitle, level, todo, pos, priority,
  scheduled, deadline, title, properties, olp,
  atime, mtime, tags, aliases, refs FROM
  (
  SELECT id, file, filetitle, \"level\", todo, pos, priority,
    scheduled, deadline, title, properties, olp, atime,
    mtime, '(' || group_concat(tags, ' ') || ')' as tags,
    aliases, refs FROM
    -- outer from clause
      (
      SELECT  id,  file, filetitle, \"level\", todo,  pos, priority,  scheduled, deadline ,
        title, properties, olp, atime,  mtime, tags,
        '(' || group_concat(aliases, ' ') || ')' as aliases,
        refs FROM
        -- inner from clause
          (
          SELECT  nodes.id as id,  nodes.file as file,  nodes.\"level\" as \"level\",
            nodes.todo as todo,   nodes.pos as pos,  nodes.priority as priority,
            nodes.scheduled as scheduled,  nodes.deadline as deadline,  nodes.title as title,
            nodes.properties as properties,  nodes.olp as olp,  files.atime as atime,
            files.title as filetitle,
            files.mtime as mtime,  tags.tag as tags,    aliases.alias as aliases,
            '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
          FROM nodes
            LEFT JOIN files ON files.file = nodes.file
            LEFT JOIN tags ON tags.node_id = nodes.id
            LEFT JOIN aliases ON aliases.node_id = nodes.id
            LEFT JOIN refs ON refs.node_id = nodes.id
          GROUP BY nodes.id, tags.tag, aliases.alias )
        -- end inner from clause
      GROUP BY id, tags )
    -- end outer from clause
  GROUP BY id)")
              (query  (string-join
                       (list
                        org-roam-db-super-main-clause
                        where-clause) "\n"))
              (rows (org-roam-db-query query)))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                    ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row))
                      (list
                       (org-roam-node-create :id id
                                             :file file
                                             :file-title file-title
                                             :file-atime atime
                                             :file-mtime mtime
                                             :level level
                                             :point (car (alist-get id nodeids-extra 0 'nil 'equal))
                                             :todo todo
                                             :priority priority
                                             :scheduled scheduled
                                             :deadline deadline
                                             :title title
                                             :aliases aliases
                                             :properties properties
                                             :olp olp
                                             :tags tags
                                             :refs refs)))))) ;;NOTE: maybe make a specific struct that would let me hold pos and properities that are link and node specific. also hold link dest.

(defun org-roam-url--get-place-title-path-completions (url &optional level)
  "Return title path completions for URL upto LEVEL depth.
Find files that contain a portion of URL.
The car is the displayed title for completion, and the cdr is the
to the file."
  (let* (rows (org-roam-url-max-depth (or level org-roam-url-max-depth)))
    (dolist (progressive-url (org-roam-url--progressive-urls url) rows)
      (if (not (and rows org-roam-url-stop-on-first-result))
          (setq rows (append rows (org-roam-url--query progressive-url)))))))

;;; common tools
(cl-defun org-roam-url-find-file (completions &key initial-prompt filter-fn no-confirm setup-fn)
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
  (if-let* ((completions (funcall (or filter-fn #'identity)
                                  completions))
            (node (pcase (list (length completions) (not org-roam-url-auto-complete-on-single-result))
                    (`(0 . ,cdr) nil)
                    (`(1 nil) (progn (when setup-fn (funcall setup-fn)) (car completions)))
                    (_ (if no-confirm
                           initial-prompt
                         (when setup-fn (funcall setup-fn))
                         (org-roam-url-completion--completing-read "node with url: " completions
                                                                   :initial-input initial-prompt))))))
      (org-roam-node-visit node 'nil current-prefix-arg)
    node))

;;;###autoload
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
         (org-roam-capture-additional-template-props (list :no-save 't))
         (progressive (plist-get info :progressive))
         (opened-file (if progressive
                          (org-roam-url-find-file (org-roam-url--get-place-title-path-completions ref) :setup-fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame))))
                        (org-roam-url-find-file (org-roam-url--get-place-title-path-completions ref 0) :setup-fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame)))))))
    (unless (or check opened-file)
      (org-roam-protocol-open-ref info))))

(push '("org-roam-url"  :protocol "roam-url"   :function org-roam-protocol-open-url)
      org-protocol-protocol-alist)
(provide 'org-roam-url)
;;; org-roam-url.el ends here
