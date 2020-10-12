;;; boidem-jump.el --- Build hydra to navigate org doc headings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Todd Foster

;; Author: Todd Foster
;; Keywords: convenience, outlines, org-mode

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

;;

;;; Code:
(provide 'boidem-jump)

(defun boidem-jump-heading-marker-regex (level)
  "Render the heading marker for the specified level, set up for a reg-ex.

Reg-ex allows me to specify beginning of line to ensure it's a proper org-mode. Trailing space included.
"
  (let ((marker "^"))
    (dotimes (i level) (setq marker (concat marker "\\\*")))
    (concat marker " ")))


(defun boidem-jump-heading-marker (level)
"Render the heading marker as plain text.
Reg-ex's begin to choke on special characters, such as in embedded links. When jumping to a heading, this raises the possibility of false positives within the text.
"
  (let ((marker ""))
    (dotimes (i level) (setq marker (concat marker "*")))
    (concat marker " ")))


(defun boidem-jump-get-headings (level)
"Get org-mode headings for the specified level in the currently visible buffer."
  (save-excursion
    (let ((headings)
          (marker (boidem-jump-heading-marker-regex level)))
      (goto-char (point-min))
      (while (search-forward-regexp marker nil 't)
        (setq foundheader (buffer-substring-no-properties (point) (line-end-position)))
        (setq headings (append headings (list foundheader))))
      headings)))


(defun boidem-jump-another-letter (chars)
  "Find a letter not already contained in CHARS.

CHARS is a list of *lowercase* characters. Start searching from z and go backwards."
  (let ((found) (letter ?z))
    (while (and (> letter ?a) (member letter chars))
      (setq letter (1- letter)))
    (if (member letter chars)
	?- letter)))


(defun boidem-letterp (char)
"Is CHAR a letter a-z?"
  (and (>= char ?a) (<= char ?z)))


(defun boidem-jump-unique-letter (heading chars)
"Find the first letter in HEADING that isn't already listed in CHARS.

Convert upper-case letters to lower-case."
(let ((candidate (downcase (string-to-char heading))))
      (if
        (or
          (not (boidem-letterp candidate))
          (member candidate chars))
        (if (> (length heading) 0)
	    (boidem-jump-unique-letter (substring heading 1) chars)
	    (boidem-jump-another-letter chars))
        candidate)))


(defun boidem-jump-selected (target level)
  "Jump to the selected heading; offer a sub-heading if one exists."
  (let* ((marker (boidem-jump-heading-marker level))
  	 (precise-target (concat marker target)))
    (goto-char (point-min))
    ; TODO: handle false positives: matches embedded in text, not at
    ;  beginning of line
    (if (search-forward precise-target)
       (progn
	 (beginning-of-line)
	 (org-show-subtree)))
   ; Try to offer a sub-heading
   (save-restriction
     (org-narrow-to-subtree)
     (boidem-jump-menu (1+ level)))))


(defun boidem-jump-menu (level)
"Jump to a heading in the currently visible buffer at specified level."
      ; TODO Get headings for highest level (closest to 1) we can find
(let ((headings (boidem-jump-get-headings level)))
  (if headings (boidem-jump-menu-show level headings))))


(defun boidem-jump-menu-heading-simplify (original)
  "Simplify the rendering of the jump menu for display."
  (let ((max-length 20))
    ; TODO: Also restrict which characters to display?
    ; TODO: remove/simplify links
    (if (> (length original) max-length)
	(substring original 0 max-length)
      original)))


(defun boidem-jump-menu-show (level headings)
  (save-excursion
    (let ((letters '(?q)))

      (dolist (h headings)	; Find key letters for each heading
	(setq u (boidem-jump-unique-letter h letters))
	(setq letters (append letters (list u))))

      (let ((buf (generate-new-buffer "*boidem-jump-dynamic*"))
	    (nl "
"))
	(set-buffer buf)
	(insert "; Dynamically generated hydra for boidem-jump" nl)
	(insert (format "; headings=%s" headings) nl)
	(insert (format "; Found letters=%s" (concat letters)) nl)
	(insert nl nl)
	(insert "(defhydra boidem-jump-hydra (:color blue :exit t)" nl)
	(dotimes (i (length letters))
	  (let ((prompt (car (nthcdr i letters)))
		(target (car (nthcdr (1- i) headings))))
            (if (> i 0)
		(progn
		  (insert "(\"" prompt "\" ")
		  (insert " (boidem-jump-selected \"" target "\" " (number-to-string level) ") ")
                  (insert " \"" (boidem-jump-menu-heading-simplify target) "\")" nl)))))
	(insert "(\"q\" nil \"quit\"))")
	(eval-buffer buf)
	(kill-buffer buf)
	))
    (boidem-jump-hydra/body)))

(defun boidem-jump ()
"Jump to an org-mode heading in the current file.

If the selected heading has sub-headings, offer a sub-menu."
(interactive)
(boidem-jump-menu 1))


;;; boidem-jump.el ends here
