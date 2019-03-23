;;; point-history --- History of points you visited before

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
;; URL: https://github.com/blue0513/point-history
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'point-history)
;;

;;; Code:

(require 'popwin)

(defvar-local point-history--list nil)
(defvar point-history-show-buffer "*point-history-show*")
(defvar point-history-max-item-num 100)

(defvar point-history-show-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") 'point-history-goto)
    kmap))

(defun point-history-show-mode nil
  "Major mode for `point-history-show-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'point-history-show-mode)
  (setq mode-name "point-history")
  (use-local-map point-history-show-mode-map))

(defun point-history-goto ()
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'point-history-buffer))
	(pos-str (get-text-property (point) 'point-history-point))
	(pos (string-to-number pos-str)))
    (if (null buffer-str)
	(message "No point at this line.")
      (pop-to-buffer (get-buffer buffer-str))
      (goto-char pos))))

(defun point-history--uniqu-list! ()
  (delq nil (delete-dups point-history--list)))

(defun point-history--push-item! (item)
  (push item point-history--list)
  (point-history--uniqu-list!)
  (if (> (length point-history--list) point-history-max-item-num)
      (let* ((last-item (car (last point-history--list)))
	     (new-point-history--list (remove last-item point-history--list)))
	(setq point-history--list new-point-history--list))))

(defun point-history--update-list! ()
  (let* ((pos (point))
	 (buffer (current-buffer))
	 (line-content (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))
	 (point-item (list pos buffer line-content)))
    (point-history--push-item! point-item)))

(defun point-history--build-history (points)
  (dolist (point points)
    (let* ((pos-info (format "%s" (nth 0 point)))
	   (buffer-info (format "%s" (nth 1 point)))
	   (line-info (format "%s" (nth 2 point)))
	   (str (concat buffer-info ":" pos-info ":" line-info)))
      (put-text-property 0 (length str) 'point-history-buffer buffer-info str)
      (put-text-property 0 (length str) 'point-history-point pos-info str)
      (insert (concat str "\n"))))
  (setq header-line-format header)
  (setq buffer-read-only t))

(defun point-history-show ()
  (interactive)
  (let* ((points point-history--list)
	 (header "buffer:pos:line"))
    (popwin:popup-buffer
     (generate-new-buffer point-history-show-buffer))
    (point-history-show-mode)
    (point-history--build-history points)))

(run-with-idle-timer 1 t 'point-history--update-list!)

;; * provide

(provide 'point-history)

;;; point-history.el ends here
