;;; consult-yasnippet.el --- A consulting-read interface for yasnippet -*- lexical-binding: t; -*-

;; Copyright (C) 2021  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Package-Requires: ((emacs "27.1") (yasnippet "0.14") (consult "0.16"))
;; Version: 0.2
;; URL: https://github.com/mohkale/consult-yasnippet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactively select a yasnippet snippet through completing-read with in
;; buffer previews.

;;; Code:

(require 'map)
(require 'consult)
(require 'yasnippet)

(defgroup consult-yasnippet nil
  "Consult interface for yasnippet."
  :group 'consult
  :group 'editing)

(defcustom consult-yasnippet-use-thing-at-point nil
  "Use `thing-at-point' as initial value for `consult-yasnippet'."
  :type 'boolean)

(defun consult-yasnippet--expand-template (template region region-contents)
  "Expand TEMPLATE at point saving REGION and REGION-CONTENTS."
  (deactivate-mark)
  (goto-char (car region))

  ;; Restore marked region (when it existed) so that `yas-expand-snippet'
  ;; overwrites it.
  (when (not (string-equal "" region-contents))
    (push-mark (point))
    (push-mark (cdr region) nil t))

  (cl-letf (((symbol-function 'yas-completing-read)
             (lambda (&rest _args) ""))
            ;; yasnippet doesn't have a multiple variant.
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _args) "")))
    (yas-expand-snippet (yas--template-content template)
                        nil nil
                        (yas--template-expand-env template))))

(defun consult-yasnippet--bounds-of-thing-at-point (template)
  "Check if `thing-at-point' is a substring of either `template-key' or
`template-name'. Matches only if `consult-yasnippet-use-thing-at-point' is t."
  (if template
      (let* ((thing (or (thing-at-point 'symbol) ""))
             (template-key (yas--template-key template))
             (template-name (yas--template-name template))
             (use-thing-at-point
              (when consult-yasnippet-use-thing-at-point
                (or (string-match-p thing (regexp-quote template-key))
                    (string-match-p thing (regexp-quote template-name)))))
             (thing-bounds
              (if use-thing-at-point
                  (bounds-of-thing-at-point 'symbol)
                (cons nil nil))))
        thing-bounds)
    (cons nil nil)))

(defun consult-yasnippet--preview ()
  "Previewer for `consult--read'.
This function expands TEMPLATE at point in the buffer
`consult-yasnippet--read-template' was started in. This includes
overwriting any region that was active and removing any previous
previews that're already active.

When TEMPLATE is not given, this function essentially just resets
the state of the current buffer to before any snippets were previewed.

If `consult-yasnippet-use-thing-at-point' is `t' and region is not selected,
this function removes the matching prefix from the preview."
  (let* ((buf (current-buffer))
         (region-active-initially (region-active-p))
         (region (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point) (point))))
         (region-contents (buffer-substring (car region) (cdr region))))
    (lambda (action template)
      (with-current-buffer buf
        (let* ((yas-verbosity 0)
               (inhibit-redisplay t)
               (inhibit-read-only t)
               (orig-offset (- (point-max) (cdr region)))
               (yas-prompt-functions '(yas-no-prompt))
               (thing-bounds (consult-yasnippet--bounds-of-thing-at-point template))
               (thing-start (car thing-bounds))
               (thing-end (cdr thing-bounds))
               (initial-prefix (when thing-start (buffer-substring thing-start thing-end))))
          (when (and thing-start (not region-active-initially))
            (setq region thing-bounds
                  region-contents initial-prefix))
          ;; We always undo any snippet previews before maybe setting up
          ;; some new previews.
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (setcar region (point))
          (insert region-contents)
          (setcdr region (point))
          ;; Restore the region if it was initially active, so that yasnippet can overwrite
          (when (and region-active-initially (eq action 'return))
            (activate-mark)
            (set-mark (car region))
            (goto-char (cdr region)))

          (when (and template (not (eq action 'return)))
            (unwind-protect
                (consult-yasnippet--expand-template template region region-contents)
              (unwind-protect
                  (mapc #'yas--commit-snippet
                        (yas-active-snippets (point-min) (point-max)))
                (setcdr region (- (point-max) orig-offset))
                (deactivate-mark)))
            (redisplay)))))))

(defun consult-yasnippet--candidates (templates)
  "Convert TEMPLATES into candidates for completing-read."
  (mapcar
   (lambda (template)
     (cons (concat
            (propertize (concat (yas--table-name (yas--template-table template))
                                " ")
                        'invisible t)
            (yas--template-name template)
            " ["
            (propertize (or (yas--template-key template)
                            (and (functionp 'yas--template-regexp-key)
                                 (yas--template-regexp-key template)))
                        'face 'consult-key)
            "]")
           template))
   templates))

(defun consult-yasnippet--annotate (candidates)
  (lambda (cand)
    (when-let ((template (cdr (assoc cand candidates)))
               (table-name (yas--table-name (yas--template-table template))))
      (concat
       " "
       (propertize " " 'display `(space :align-to (- right ,(+ 1 (length table-name)))))
       table-name))))

(defun consult-yasnippet--read-template (&optional all-templates)
  "Backend implementation of `consult-yasnippet'.
This starts a `completing-read' session with all the snippets in the current
snippet table with support for previewing the snippet to be expanded and
replacing the active region with the snippet expansion. When ALL-TEMPLATES
is non-nil you get prompted with snippets from all snippet tables, not just
the current one.

This function doesn't actually expand the snippet, it only reads and then
returns a snippet template from the user."
  (unless (bound-and-true-p yas-minor-mode)
    (error "`consult-yasnippet' can only be called while `yas-minor-mode' is active"))

  (barf-if-buffer-read-only)

  (let* ((buffer-undo-list t)                                                  ; Prevent querying user (and showing previews) from updating the undo-history
         (candidates
          (consult-yasnippet--candidates
           (if all-templates
               (yas--all-templates (map-values yas--tables))
             (yas--all-templates (yas--get-snippet-tables))))))
    (consult--read
     candidates
     :prompt "Choose a snippet: "
     :annotate (consult-yasnippet--annotate candidates)
     :initial
     (when consult-yasnippet-use-thing-at-point
       (thing-at-point 'symbol))
     :lookup 'consult--lookup-cdr
     :require-match t
     :state (consult-yasnippet--preview)
     :category 'yasnippet)))

;;;###autoload
(defun consult-yasnippet-visit-snippet-file (template)
  "Visit the snippet file associated with TEMPLATE.
When called interactively this command previews snippet completions in
the current buffer, and then opens the selected snippets template file
using `yas--visit-snippet-file-1'."
  (interactive (list (consult-yasnippet--read-template t)))
  (yas--visit-snippet-file-1 template))

;;;###autoload
(defun consult-yasnippet (arg)
  "Interactively select and expand a yasnippet template.
This command presents a completing read interface containing all currently
available snippet expansions, with live previews for each snippet. Once
selected a chosen snippet will be expanded at point using
`yas-expand-snippet'.

With ARG select snippets from all snippet tables, not just the current one."
  (interactive "P")
  (when-let ((template (consult-yasnippet--read-template arg)))
    (let* ((thing-bounds (if (region-active-p)
                             (cons nil nil)
                           (consult-yasnippet--bounds-of-thing-at-point template)))
           (thing-start (car thing-bounds))
           (thing-end (cdr thing-bounds)))
      (yas-expand-snippet (yas--template-content template)
                          thing-start thing-end
                          (yas--template-expand-env template)))))

(provide 'consult-yasnippet)
;;; consult-yasnippet.el ends here
