;;; ivy-ghq.el --- ghq with ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Youhei SASAKI

;; Original Author of ido-ghq: Youhei SASAKI
;; Small edits to acheive ivy compatability by github.com/analyticd
;; $Lastupdate: 2018-12-22 02:04:09$
;; Version: 0.0.2
;; Package-Requires: nil
;; Keywords: tools
;; URL: https://github.com/analyticd/ivy-ghq

;; This file is not part of GNU Emacs.
;;
;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentry:

;; original version is https://github.com/uwabami/ido-ghq

;;; Code:

(defgroup ivy-ghq nil
  "ghq with ivy interface"
  :prefix "ivy-ghq-"
  :group 'ivy)

(defcustom ivy-ghq-command
  "ghq"
  "*A ghq command"
  :type 'string
  :group 'ivy-ghq)

(defcustom ivy-ghq-command-arg-root
  '("root")
  "*Arguments for getting ghq root path using ghq command"
  :type '(repeqt string)
  :group 'ivy-ghq)

(defcustom ivy-ghq-short-list nil
  "*Whether display full path or short path"
  :type 'boolean
  :group 'ivy-ghq)

(defun ivy-ghq--command-arg-list ()
  (if ivy-ghq-short-list
      '("list")
    '("list" "--full-path")))

(defun ivy-ghq--open-dired (file)
  (dired
   (if ivy-ghq-short-list
       (format "%s%s" (ivy-ghq--get-root) file)
     (format "%s" file))))

(defun ivy-ghq--delete-dired (file)
  (dired-delete-file
   (if ivy-ghq-short-list
       (format "%s%s" (ivy-ghq--get-root) file)
     (format "%s" file))
   t t))

(defun ivy-ghq--get-root ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          ivy-ghq-command nil t nil
                          ivy-ghq-command-arg-root))
      (error "Failed: Can't get ghq's root"))
    (replace-regexp-in-string "\n+$" "/"
                              (buffer-substring-no-properties
                               (goto-char (point-min))(goto-char (point-max))))))

(defun ivy-ghq--list-candidates ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
                          ivy-ghq-command nil t nil
                          (ivy-ghq--command-arg-list)))
      (error "Failed: Can't get ghq list candidates"))
    (let ((paths))
      (goto-char (point-min))
      (while (not (eobp))
        (push
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position))
         paths)
        (forward-line 1))
      (reverse paths))))

;;; autoload
(defun ivy-ghq-open ()
  "Use `ivy-completing-read' to \\[dired] a ghq list"
  (interactive)
  (let ((path (ivy-completing-read "Find ghq repo.: "
                                   (ivy-ghq--list-candidates))))
    (if (ivy-ghq--open-dired path)
        (message (format "Open ghq repository: %s" path)))))

;;; autoload
(defun ivy-ghq-delete-repo ()
  "Use `ivy-completing-read' to \\[dired-delete-file] a ghq managed repo"
  (interactive)
  (let ((path (ivy-completing-read "Find ghq repo.: "
                                   (ivy-ghq--list-candidates))))
    (when (y-or-n-p (format "Are you sure you want to delete %s? " path))
      (ivy-ghq--delete-dired path)
      (when (not (file-exists-p path))
        (when (y-or-n-p
               (format
                "Deleted ghq repository: %s. Would you like to navigate to its parent directory?" path))
          (dired (file-name-directory (directory-file-name path))))))))

(provide 'ivy-ghq)
;;; ivy-ghq.el ends here
