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
(defvar point-history-last-visited-buffer-info nil)
(defvar point-history-last-previewed-buffer nil)

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

(defcustom point-history-should-preview t
  "If non-nil, it shows the preview of buffers at the cursor in `point-history-show-buffer'."
  :type 'boolean
  :group 'point-history)

(defcustom point-history-ignore-major-mode nil
  "List of ignored major modes."
  :type '(repeat symbol)
  :group 'point-history)

(defvar point-history-show-mode-map
  (let ((kmap (make-sparse-keymap)))
    ;; point-history-goto
    (define-key kmap (kbd "RET")     'point-history-goto)
    ;; point-history-next-line
    (define-key kmap (kbd "n")       'point-history-next-line)
    (define-key kmap (kbd "C-n")   'point-history-next-line)
    (define-key kmap (kbd "TAB")     'point-history-next-line)
    ;; point-history-prev-line
    (define-key kmap (kbd "p")       'point-history-prev-line)
    (define-key kmap (kbd "C-p")     'point-history-prev-line)
    (define-key kmap (kbd "<C-tab>") 'point-history-prev-line)
    ;; point-history-close
    (define-key kmap (kbd "g")       'point-history-close)
    (define-key kmap (kbd "C-g")     'point-history-close)
    kmap))

(defun point-history-show-mode nil
  "Major mode for `point-history-show-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'point-history-show-mode)
  (setq mode-name "point-history")
  (use-local-map point-history-show-mode-map))

(defun point-history-close (&optional only-close)
  "Close point-history-show-buffer.
If ONLY-CLOSE is non-nil, it dons not call `pop-to-buffer'."
  (interactive)
  (if only-close
      (popwin:close-popup-window)
    (with-selected-window (get-buffer-window point-history-last-previewed-buffer)
      (let* ((buffer (nth 0 point-history-last-visited-buffer-info))
             (pos (nth 1 point-history-last-visited-buffer-info)))
        (pop-to-buffer-same-window buffer)
        (goto-char pos)
        (popwin:close-popup-window)))))

(defun point-history-goto ()
  "Go to the point in point-history list."
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'point-history-buffer))
         (pos-str (get-text-property (point) 'point-history-position))
         (pos (string-to-number pos-str)))
    (if (null buffer-str)
        (message "No point at this line.")
      (point-history--goto buffer-str pos))))

(defun point-history--goto (buffer-str pos)
  "Pop to BUFFER-STR and go to POS, then close `point-history-show-buffer'."
  (with-selected-window (get-buffer-window point-history-last-previewed-buffer)
    (goto-char pos)
    (point-history-close t)))

(defun point-history-preview-at-point ()
  "Preview the buffer at point in the other window."
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'point-history-buffer))
         (buffer (get-buffer buffer-str))
         (pos-str (get-text-property (point) 'point-history-position))
         (pos (string-to-number pos-str)))
    (point-history--preview-at-point buffer pos)
    (popwin:select-popup-window)))

(defun point-history--preview-at-point (buffer pos)
  "Show BUFFER and set the cursor at POS."
  (with-selected-window (get-buffer-window point-history-last-previewed-buffer)
    (pop-to-buffer-same-window buffer)
    (goto-char pos)
    (point-history--save-last-previewed-buffer! buffer)))

(defun point-history-next-line ()
  "Go to next line in `point-history-show-mode'.
If the current line number is end of the buffer, go to the first line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (>= current-line-num total-line-num)
        (goto-line begining-line-num)
      (goto-line (+ 1 current-line-num)))
    (if point-history-should-preview
        (point-history-preview-at-point))))

(defun point-history-prev-line ()
  "Go to previous line in `point-history-show-mode'.
If the current line number is begining of the buffer, go to the last line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (<= current-line-num begining-line-num)
        (goto-line total-line-num)
      (goto-line (- current-line-num 1)))
    (if point-history-should-preview
        (point-history-preview-at-point))))

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
         (maybe-unique-element-list
          (point-history--remove-duplicate-element
           content buffer line-num))
         (unique-element-list
          (delq nil (delete-dups maybe-unique-element-list))))
    (setq point-history-list unique-element-list)
    (push item point-history-list)))

(defun point-history--save-last-visited-buffer! (buffer)
  "Set BUFFER as `point-history-last-visited-buffer-info'."
  (let* ((buffer-info buffer)
         (pos-info (point))
         (info (list buffer-info pos-info)))
    (setq point-history-last-visited-buffer-info info)))

(defun point-history--save-last-previewed-buffer! (buffer)
  "Set BUFFER as `point-history-last-previewed-buffer'."
  (setq point-history-last-previewed-buffer buffer))

(defun point-history--build-valid-buffer-list! ()
  "Remove killed buffer from point-history-list."
  (let* ((old-point-history-list point-history-list)
         (new-point-history-list
          (seq-filter (lambda (elt) (buffer-live-p (nth 3 elt)))
                      old-point-history-list)))
    (setq point-history-list new-point-history-list)))

(defun point-history--push-item! (item)
  "Push ITEM to point-history-list."
  (point-history--maybe-unique-push! item)
  (point-history--build-valid-buffer-list!)
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
    (if (and (not (string-match-p point-history-ignore-buffer (buffer-name buffer)))
             (not (member major-mode point-history-ignore-major-mode)))
        ;; Each point-item has
        ;; nth 0: marker info
        ;; nth 1: line content
        ;; nth 2: line number
        ;; nth 3: buffer info
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
    (point-history--save-last-visited-buffer! (current-buffer))
    (point-history--save-last-previewed-buffer! (current-buffer))
    (popwin:popup-buffer
     (generate-new-buffer point-history-show-buffer)
     :stick t
     :height point-history-show-buffer-height)
    (point-history-show-mode)
    (point-history--build-history points)
    (point-history-preview-at-point)))

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
