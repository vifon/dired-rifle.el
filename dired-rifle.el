;;; dired-rifle.el --- Call rifle(1) from dired      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wojciech Siewierski

;; Author: Wojciech Siewierski <wojciech dot siewierski at onet dot pl>
;; URL: https://github.com/vifon/dired-rifle.el
;; Keywords: files, convenience
;; Version: 0.9

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

;; Run rifle(1) known from ranger <https://github.com/ranger/ranger>
;; from a dired buffer.

;; Press 'r' to open the current file.  See the `dired-rifle' command
;; documentation for more running options.

;;; Code:

(require 'dired)

(defun rifle-open (path &optional program-number output-buffer)
  "Open a file with rifle(1).

PATH is the file to open.

PROGRAM-NUMBER is the argument passed to `rifle -p', i.e. which
of the matching rules to use.

OUTPUT-BUFFER is the buffer for the rifle output.  If nil, the
output gets discarded."
  (when output-buffer
    (view-buffer-other-window output-buffer
                              nil
                              #'kill-buffer-if-not-modified))
  (call-process "rifle"
                nil (or output-buffer 0) nil
                "-p" (number-to-string (or program-number 0))
                "--" path))

(defun rifle-get-rules (path)
  "Get the matching rifle rules for PATH as a list of strings."
  (with-temp-buffer
    (call-process "rifle"
                  nil (current-buffer) nil
                  "-l"
                  "--" path)
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun dired-rifle (arg)
  "Call rifle(1) on the currently focused file in dired.

With `\\[universal-argument]' the output is saved to a buffer named
*dired-rifle*.  Otherwise the output is discarded.

With `\\[universal-argument] \\[universal-argument]' show the
matching rifle rules for manual selection.

With a numeric prefix argument ARG, run ARGth rifle rule
instead of the default one (0th)"
  (interactive "P")
  (let ((inhibit-read-only t))
    (let ((output-buffer (when (consp arg)
                           (with-current-buffer
                               (get-buffer-create "*dired-rifle*")
                             (erase-buffer)
                             (current-buffer))))
          (path (dired-get-filename)))
      (cond
       ((equal arg '(4))
        (rifle-open path nil output-buffer))
       ((equal arg '(16))
        (rifle-open path
                    (string-to-number
                     (replace-regexp-in-string
                      "^\\([0-9]+\\).*" "\\1"
                      (completing-read "Rifle rule: "
                                       (rifle-get-rules path))))
                    output-buffer))
       (t
        (rifle-open path arg)))
      (when (bufferp output-buffer)
        (with-current-buffer output-buffer
          (goto-char (point-min)))))))

(define-key dired-mode-map (kbd "r") #'dired-rifle)

(provide 'dired-rifle)
;;; dired-rifle.el ends here
