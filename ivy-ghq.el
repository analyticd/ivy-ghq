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

;;; autoload
(defun ivy-ghq-add-repo (start end)
  "Ask ghq to add a git repo defined in the highlighted region of
the current buffer. This is best used with emacs-w3m or another
Emacs-based web browser."
  (interactive "r")
  (compilation-start
   (format  "ghq get %s" (buffer-substring start end))))

(defcustom ivy-ghq-github-username "analyticd"
  "Your username on github.com")

;;; autoload
(defun ivy-ghq-clone-and-fork-repo (start end)
  "Ask ghq to get a git repo defined in the highlighted region of
the current buffer, use hub to fork it, clone the forked repo,
optionally add a git remote depending on user's feedback in
minibuffer, and put the original repo url on the kill ring for use
in Magit or on command line to add an upstream remote reference
if the user prefers to add the remote using a different remote
name than 'upstream'."
  (interactive "r")
  (let ((user-and-repo-name (buffer-substring start end))
        (hub-path (executable-find "hub")))
    (when (null hub-path) (error "Must have 'hub' binary installed on system and somewhere in path."))
    (when (not (or (and (string-match-p "http\\|git" user-and-repo-name)
                        (string-match-p "github.com" user-and-repo-name))
                   (not (string-match-p "http\\|git" user-and-repo-name))))
      (error "Must be a github repo to fork repo using 'hub'."))
    (let* ((repo-name (car (last (split-string user-and-repo-name "/"))))
           (ghq-root (if ghq-root ghq-root "~/.ghq")) ; You can define ghq-root elsewhere if desired.
           (domain (if (string-match-p "http" user-and-repo-name)
                       (third (split-string user-and-repo-name "/"))
                     "github.com"))
           (full-path-to-orig-repo
            (expand-file-name (if (string-match-p "http\\|git" user-and-repo-name)
                                  ;; Handle weird repo urls that have more than a user and repo name.
                                  (string-join (cddr (split-string user-and-repo-name "/")) "/")
                                (concat "github.com/" user-and-repo-name))
                              ghq-root))
           (full-remote-url (concat "https://"
                                    (if (string-match-p "http\\|git" user-and-repo-name)
                                        ;; Handle weird repo urls that have more than a user and repo name.
                                        (string-join (cddr (split-string user-and-repo-name "/")) "/")
                                      (concat "github.com/" user-and-repo-name))))
           (full-path-to-new-repo
            (expand-file-name (if (string-match-p "http\\|git" user-and-repo-name)
                                  ;; Handle weird repo urls that have more than a user and repo name.
                                  (string-join (list domain ivy-ghq-github-username repo-name) "/")
                                (string-join (list "github.com" ivy-ghq-github-username repo-name) "/"))
                              ghq-root))
           (prompt (format "Do you want to add '%s' as a remote so that you can pull changes from the original repo?" full-remote-url))
           (command-string
            (if (y-or-n-p prompt)
                (format "ghq get %s && cd %s && %s fork && ghq get %s/%s && cd %s && git remote add upstream %s"
                        user-and-repo-name
                        full-path-to-orig-repo
                        hub-path
                        ivy-ghq-github-username
                        repo-name
                        full-path-to-new-repo
                        full-remote-url)
              (format "ghq get %s && cd %s && %s fork && ghq get %s/%s"
                        user-and-repo-name
                        full-path-to-orig-repo
                        hub-path
                        ivy-ghq-github-username
                        repo-name))))
      (copy-region-as-kill start end) ; Put the original repo url in the kill ring.
      (compilation-start
       command-string))))

;; Example URL cases that ghq can handle, but some of which
;; ivy-ghq-clone-and-fork-repo cannot because hub only works with github repos:
;; foo/bar                                 ; github brief
;; https://bitbucket.com/foo/bar           ; non-github; can't fork a non-github repo with hub
;; https://git.sf.com/foo/bar/bat          ; non-github extended; can't fork a non-github repo with hub
;; https://github.com/uwabami/ido-ghq      ; github full


;;; autoload
(defun ivy-ghq-add-org-link ()
  "Use `ivy-completing-read' to insert an Org link to a ghq
managed repo."
  (interactive)
  (let* ((path (ivy-completing-read "Find ghq repo.: "
                                   (ivy-ghq--list-candidates)))
         (description (read-string "Link description (optional): ")))
    (insert (format "[[ghq:%s][%s]]" path (if (not (string-empty-p description)) description path)))))

(provide 'ivy-ghq)
;;; ivy-ghq.el ends here
