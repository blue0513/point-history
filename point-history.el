;;; point-history --- Show the history of points you visited before

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; Package-Requires: ((popwin "1.0.0"))
;; URL: https://github.com/blue0513/point-history
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'point-history)
;;

;;; Code:

(require 'popwin)

(defvar point-history-list nil)
(defvar point-history-show-buffer " *point-history-show*")

(defcustom point-history-max-item-num 100
  "Max number of points saved in history."
  :type 'integer
  :group 'point-history)

(defcustom point-history-show-buffer-height 30
  "Buffer height to show point-history."
  :type 'integer
  :group 'point-history)

(defcustom point-history-save-timer 1
  "Interval time to save point in history."
  :type 'integer
  :group 'point-history)

(defcustom point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*"
  "Regular expression describing buffer names that are never saved in history."
  :type '(choice (const nil) (regexp))
  :group 'point-history)

(defvar point-history-show-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") 'point-history-goto)
    (define-key kmap (kbd "n") 'point-history-next-line)
    (define-key kmap (kbd "TAB") 'point-history-next-line)
    (define-key kmap (kbd "p") 'point-history-prev-line)
    (define-key kmap (kbd "<C-tab>") 'point-history-prev-line)
    kmap))

(defun point-history-show-mode nil
  "Major mode for `point-history-show-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'point-history-show-mode)
  (setq mode-name "point-history")
  (use-local-map point-history-show-mode-map))

(defun point-history-goto ()
  "Go to the point in point-history list."
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'point-history-buffer))
	 (pos-str (get-text-property (point) 'point-history-position))
	 (pos (string-to-number pos-str)))
    (if (null buffer-str)
	(message "No point at this line.")
      (pop-to-buffer (get-buffer buffer-str))
      (goto-char pos))))

(defun point-history-next-line ()
  "Go to next line in `point-history-show-mode'.
If the current line number is end of the buffer, go to the first line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (>= current-line-num total-line-num)
        (goto-line begining-line-num)
      (goto-line (+ 1 current-line-num)))))

(defun point-history-prev-line ()
  "Go to previous line in `point-history-show-mode'.
If the current line number is begining of the buffer, go to the last line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (<= current-line-num begining-line-num)
        (goto-line total-line-num)
      (goto-line (- current-line-num 1)))))

(defun point-history--build-unique-list! ()
  "Delete duplicated element in point-history-list."
  (delq nil (delete-dups point-history-list)))

(defun point-history--remove-duplicate-element (content buffer line-num)
  "Remove duplicated element by comparing CONTENT & BUFFER & LINE-NUM."
  (seq-remove (lambda (elt)
		(and (string-equal (substring-no-properties (nth 1 elt)) content)
		     (eq (nth 2 elt) line-num)
		     (string-equal (buffer-name (nth 3 elt)) (buffer-name buffer))))
	      point-history-list))

(defun point-history--maybe-unique-push! (item)
  "Push ITEM into re-builded point-history-list containing maybe unique elements."
  (let* ((content (substring-no-properties (nth 1 item)))
	 (line-num (nth 2 item))
	 (buffer (nth 3 item))
	 (unique-element-list (point-history--remove-duplicate-element
			       content buffer line-num)))
	 (setq point-history-list unique-element-list)
	 (push item point-history-list)))

(defun point-history--push-item! (item)
  "Push ITEM to point-history-list."
  (point-history--maybe-unique-push! item)
  (point-history--build-unique-list!)
  (if (> (length point-history-list) point-history-max-item-num)
      (let* ((last-item (car (last point-history-list)))
	     (new-point-history--list (remove last-item point-history-list)))
	(setq point-history-list new-point-history--list))))

(defun point-history--update-list! ()
  "Build point-history to push point-history-list."
  (let* ((marker (point-marker))
	 (buffer (marker-buffer marker))
	 (line-num (line-number-at-pos))
	 (line-content (buffer-substring
			(line-beginning-position) (line-end-position)))
	 (point-item (list marker line-content line-num buffer)))
    (if (not (string-match-p point-history-ignore-buffer (buffer-name buffer)))
	(point-history--push-item! point-item))))

(defun point-history--build-history (points)
  "Build human readable history-list from POINTS."
  (dolist (point points)
    (let* ((pos-info (format "%s" (marker-position (nth 0 point))))
	   (buffer-info (format "%s" (marker-buffer (nth 0 point))))
	   (content-info (format "%s" (nth 1 point)))
	   (str (concat buffer-info ": " content-info)))
      (put-text-property 0 (length str) 'point-history-buffer buffer-info str)
      (put-text-property 0 (length str) 'point-history-position pos-info str)
      (insert (concat str "\n"))))
  (setq header-line-format header)
  (setq buffer-read-only t)
  (beginning-of-buffer))

(defun point-history--show ()
  "Open the list buffer of point-history-list."
  (let* ((points point-history-list)
	 (header "buffer:content"))
    (popwin:popup-buffer
     (generate-new-buffer point-history-show-buffer)
     :height point-history-show-buffer-height)
    (point-history-show-mode)
    (point-history--build-history points)))

(defun point-history-show ()
  "Show point-history-list."
  (interactive)
  (point-history--show))

;;;###autoload
(define-minor-mode point-history-mode
  "Global minor mode for point-history-mode"
  :init-value nil
  :global t
  :lighter " ph"
  (if point-history-mode
      (setq point-history-timer
	    (run-with-idle-timer
	     point-history-save-timer t 'point-history--update-list!))
    (cancel-timer point-history-timer)))

;; * provide

(provide 'point-history)

;;; point-history.el ends here
