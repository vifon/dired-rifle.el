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

;;;###autoload
(defun dired-rifle (arg)
  "Call rifle(1) on the currently focused file in dired.

With `\\[universal-argument]' the output is saved to a buffer named
*dired-rifle*.  Otherwise the output is discarded.

With `\\[universal-argument] \\[universal-argument]' the output
of `rifle -l' is shown, i.e. the programs available via rifle.

With a numeric prefix argument ARG, run ARGth rifle rule
instead of the default one (0th)"
  (interactive "P")
  (let ((out-buffer (if (consp arg)
                        (with-current-buffer
                            (get-buffer-create "*dired-rifle*")
                          (erase-buffer)
                          (current-buffer))
                      0))
        (rule-number (if (integerp arg)
                         arg
                       0)))
    (if (equal arg '(16))
        (call-process "rifle"
                      nil out-buffer nil
                      "-l"
                      "--" (dired-get-filename))
      (call-process "rifle"
                    nil out-buffer nil
                    "-p" (int-to-string rule-number)
                    "--" (dired-get-filename)))
    (when (bufferp out-buffer)
      (with-current-buffer out-buffer
        (goto-char (point-min))))))

(define-key dired-mode-map (kbd "r") #'dired-rifle)

(provide 'dired-rifle)
;;; dired-rifle.el ends here
