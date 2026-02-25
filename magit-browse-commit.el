;;; magit-browse-commit.el --- Browse pull/merge requests from magit-blame -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Angeldswang

;; Author: Angeldswang <angeldswang@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (magit "3.0.0") (s "1.12.0"))
;; Keywords: vc, tools, git
;; URL: https://github.com/angeldswang/magit-browse-commit

;; This file is not part of GNU Emacs.

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

;; This package provides functionality to browse GitHub pull requests or
;; GitLab merge requests directly from magit-blame mode.  When invoked on
;; a blamed line, it will attempt to find the merge commit that introduced
;; the change and open the corresponding PR/MR in your browser.

;; Usage:
;;   M-x magit-browse-commit-at-point
;;
;; Or bind it to a key:
;;   (with-eval-after-load 'magit-blame
;;     (define-key magit-blame-mode-map (kbd "M-o") #'magit-browse-commit-at-point))

;;; Code:

(require 'magit)
(require 'magit-blame)
(require 's)

(defgroup magit-browse-commit nil
  "Browse pull/merge requests from magit-blame."
  :group 'magit
  :prefix "magit-browse-commit-")

(defcustom magit-browse-commit-gitlab-host "gitlab.com"
  "Default GitLab host for private/self-hosted GitLab instances."
  :type 'string
  :group 'magit-browse-commit)

(defcustom magit-browse-commit-default-branch "master"
  "Default branch to use when finding merge commits."
  :type 'string
  :group 'magit-browse-commit)

(defun magit-browse-commit-remote-default-head (default)
  "Find the default head in remote, like if it's main or master(DEFAULT)."
  (when-let ((output (magit-git-string "ls-remote" "--symref" "origin" "HEAD")))
    (cond
     ((string-match "ref: refs/heads/\\([^ ]+\\)\\s-+HEAD" output)
      (match-string 1 output))
     (t default))))

(defun magit-browse-commit--parse-merge-commit (commit)
  "Find the merge commit that introduced COMMIT.
Uses git ancestry-path to find the first merge commit between
COMMIT and the default branch."
  (let ((default-directory (magit-toplevel)))
    (s-trim
     (shell-command-to-string
      (format "git log --merges --oneline --reverse --ancestry-path %s...%s | head -n 1 | cut -f1 -d' '"
              (shell-quote-argument commit)
              (shell-quote-argument (magit-browse-commit-remote-default-head
                                     magit-browse-commit-default-branch)))))))

(defun magit-browse-commit--parse-pr-number (commit)
  "Extract GitHub pull request number from merge COMMIT message."
  (let ((msg (magit-rev-format "%B" commit)))
    (when (string-match "Merge pull request #\\([0-9]+\\)" msg)
      (match-string 1 msg))))

(defun magit-browse-commit--parse-mr-number (commit)
  "Extract GitLab merge request number from merge COMMIT message."
  (let ((msg (shell-command-to-string (format "git --no-pager log -1 --format=%%B %s" commit))))
    (cond
     ((string-match "See merge request.*!\\([0-9]+\\)" msg)
      (match-string 1 msg))
     ((string-match "Iid: \\([0-9]+\\)" msg)
      (match-string 1 msg)))))

(defun magit-browse-commit--parse-github-repo (remote-url)
  "Extract GitHub repository name from REMOTE-URL."
  (cond
   ((string-match "github\\.com[:/]\\(.+\\)\\.git\\'" remote-url)
    (match-string 1 remote-url))
   ((string-match "github\\.com[:/]\\(.+\\)\\'" remote-url)
    (match-string 1 remote-url))))

(defun magit-browse-commit--parse-gitlab-repo (remote-url host)
  "Extract GitLab repository name from REMOTE-URL for HOST."
  (let ((host-escaped (regexp-quote host)))
    (cond
     ((string-match (format "%s[:/]\\(.+\\)\\.git\\'" host-escaped) remote-url)
      (match-string 1 remote-url))
     ((string-match (format "%s[:/]\\(.+\\)\\'" host-escaped) remote-url)
      (match-string 1 remote-url)))))

;;;###autoload
(defun magit-browse-commit-at-point ()
  "Browse the pull/merge request for the commit at point in magit-blame.
Finds the merge commit that introduced the blamed commit and opens
the corresponding GitHub pull request or GitLab merge request in
your browser."
  (interactive)
  (let ((chunk (magit-current-blame-chunk)))
    (unless chunk
      (user-error "No blame chunk at point"))
    (let* ((commit-hash (oref chunk orig-rev))
           (merge-commit (magit-browse-commit--parse-merge-commit commit-hash)))
      (when (string-empty-p merge-commit)
        (user-error "Could not find merge commit for %s" commit-hash))
      (let ((remote (or (magit-get-remote) "origin")))
        (let ((remote-url (magit-get "remote" remote "url")))
          (unless remote-url
            (user-error "No remote URL found for %s" remote))
          (cond
           ;; GitHub
           ((string-match-p "github\\.com" remote-url)
            (let ((repo-name (magit-browse-commit--parse-github-repo remote-url))
                  (pr-num (magit-browse-commit--parse-pr-number merge-commit)))
              (unless repo-name
                (user-error "Could not parse GitHub repository from URL: %s" remote-url))
              (unless pr-num
                (user-error "Could not find pull request number in merge commit: %s" merge-commit))
              (let ((url (format "https://github.com/%s/pull/%s" repo-name pr-num)))
                (message "Opening: %s" url)
                (browse-url url))))
           ;; GitLab
           ((string-match-p (regexp-quote magit-browse-commit-gitlab-host) remote-url)
            (let ((repo-name (magit-browse-commit--parse-gitlab-repo
                              remote-url
                              magit-browse-commit-gitlab-host))
                  (mr-num (magit-browse-commit--parse-mr-number merge-commit)))
              (unless repo-name
                (user-error "Could not parse GitLab repository from URL: %s" remote-url))
              (unless mr-num
                (user-error "Could not find merge request number in merge commit: %s" merge-commit))
              (let ((url (format "https://%s/%s/merge_requests/%s"
                                 magit-browse-commit-gitlab-host
                                 repo-name
                                 mr-num)))
                (message "Opening: %s" url)
                (browse-url url))))
           (t
            (user-error "Remote URL is neither GitHub nor configured GitLab: %s" remote-url))))))))

(provide 'magit-browse-commit)
;;; magit-browse-commit.el ends here
