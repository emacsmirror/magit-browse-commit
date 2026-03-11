;;; magit-browse-commit-test.el --- Tests for magit-browse-commit -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (magit "3.0.0"))

;;; Commentary:

;; ERT tests for magit-browse-commit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-browse-commit)

;;; --- URL parsing tests ---

(ert-deftest magit-browse-commit-test-parse-github-repo-ssh ()
  "Parse GitHub repo from SSH URL."
  (should (equal "user/repo"
                 (magit-browse-commit--parse-github-repo
                  "git@github.com:user/repo.git"))))

(ert-deftest magit-browse-commit-test-parse-github-repo-https ()
  "Parse GitHub repo from HTTPS URL."
  (should (equal "user/repo"
                 (magit-browse-commit--parse-github-repo
                  "https://github.com/user/repo.git"))))

(ert-deftest magit-browse-commit-test-parse-github-repo-https-no-dotgit ()
  "Parse GitHub repo from HTTPS URL without .git suffix."
  (should (equal "user/repo"
                 (magit-browse-commit--parse-github-repo
                  "https://github.com/user/repo"))))

(ert-deftest magit-browse-commit-test-parse-github-repo-ssh-no-dotgit ()
  "Parse GitHub repo from SSH URL without .git suffix."
  (should (equal "user/repo"
                 (magit-browse-commit--parse-github-repo
                  "git@github.com:user/repo"))))

(ert-deftest magit-browse-commit-test-parse-github-repo-non-github ()
  "Return nil for non-GitHub URL."
  (should (null (magit-browse-commit--parse-github-repo
                 "git@gitlab.com:user/repo.git"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-ssh ()
  "Parse GitLab repo from SSH URL."
  (should (equal "group/project"
                 (magit-browse-commit--parse-gitlab-repo
                  "git@gitlab.com:group/project.git" "gitlab.com"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-https ()
  "Parse GitLab repo from HTTPS URL."
  (should (equal "group/project"
                 (magit-browse-commit--parse-gitlab-repo
                  "https://gitlab.com/group/project.git" "gitlab.com"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-no-dotgit ()
  "Parse GitLab repo from URL without .git suffix."
  (should (equal "group/project"
                 (magit-browse-commit--parse-gitlab-repo
                  "https://gitlab.com/group/project" "gitlab.com"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-custom-host ()
  "Parse GitLab repo from custom host."
  (should (equal "team/project"
                 (magit-browse-commit--parse-gitlab-repo
                  "git@git.mycompany.com:team/project.git" "git.mycompany.com"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-nested-group ()
  "Parse GitLab repo with nested group path."
  (should (equal "group/subgroup/project"
                 (magit-browse-commit--parse-gitlab-repo
                  "git@gitlab.com:group/subgroup/project.git" "gitlab.com"))))

(ert-deftest magit-browse-commit-test-parse-gitlab-repo-wrong-host ()
  "Return nil when host doesn't match."
  (should (null (magit-browse-commit--parse-gitlab-repo
                 "git@gitlab.com:group/project.git" "other.host.com"))))

;;; --- PR/MR number parsing tests (mocked) ---

(ert-deftest magit-browse-commit-test-parse-pr-number ()
  "Extract PR number from GitHub merge commit message."
  (cl-letf (((symbol-function 'magit-rev-format)
             (lambda (_fmt _commit)
               "Merge pull request #42 from user/feature-branch\n\nSome description")))
    (should (equal "42" (magit-browse-commit--parse-pr-number "abc123")))))

(ert-deftest magit-browse-commit-test-parse-pr-number-no-match ()
  "Return nil when commit message has no PR number."
  (cl-letf (((symbol-function 'magit-rev-format)
             (lambda (_fmt _commit)
               "Some regular commit message")))
    (should (null (magit-browse-commit--parse-pr-number "abc123")))))

(ert-deftest magit-browse-commit-test-parse-pr-number-large ()
  "Extract large PR numbers."
  (cl-letf (((symbol-function 'magit-rev-format)
             (lambda (_fmt _commit)
               "Merge pull request #99999 from user/branch")))
    (should (equal "99999" (magit-browse-commit--parse-pr-number "abc123")))))

(ert-deftest magit-browse-commit-test-parse-mr-number-see-merge-request ()
  "Extract MR number from 'See merge request' format."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               "Merge branch 'feature' into 'main'\n\nSee merge request group/project!123")))
    (should (equal "123" (magit-browse-commit--parse-mr-number "abc123")))))

(ert-deftest magit-browse-commit-test-parse-mr-number-iid ()
  "Extract MR number from 'Iid:' format."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               "Some merge commit\n\nIid: 456")))
    (should (equal "456" (magit-browse-commit--parse-mr-number "abc123")))))

(ert-deftest magit-browse-commit-test-parse-mr-number-no-match ()
  "Return nil when commit message has no MR number."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               "Regular commit without MR info")))
    (should (null (magit-browse-commit--parse-mr-number "abc123")))))

;;; --- Default branch caching tests ---

(ert-deftest magit-browse-commit-test-default-branch-cache-hit ()
  "Return cached branch without calling ls-remote."
  (let ((magit-browse-commit--default-branch-cache
         '(("/path/to/repo/" . "main")))
        (ls-remote-called nil))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/path/to/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args)
                 (setq ls-remote-called t)
                 nil)))
      (should (equal "main" (magit-browse-commit-remote-default-head "master")))
      (should-not ls-remote-called))))

(ert-deftest magit-browse-commit-test-default-branch-cache-miss ()
  "Populate cache on first call and use it on second call."
  (let ((magit-browse-commit--default-branch-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/path/to/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args)
                 (cl-incf call-count)
                 "ref: refs/heads/develop\tHEAD")))
      ;; First call should hit ls-remote
      (should (equal "develop" (magit-browse-commit-remote-default-head "master")))
      (should (= 1 call-count))
      ;; Second call should use cache
      (should (equal "develop" (magit-browse-commit-remote-default-head "master")))
      (should (= 1 call-count)))))

(ert-deftest magit-browse-commit-test-default-branch-fallback ()
  "Fall back to default when ls-remote returns nil."
  (let ((magit-browse-commit--default-branch-cache nil))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/path/to/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args) nil)))
      (should (equal "master" (magit-browse-commit-remote-default-head "master"))))))

(ert-deftest magit-browse-commit-test-default-branch-per-repo ()
  "Cache different branches for different repos."
  (let ((magit-browse-commit--default-branch-cache
         '(("/repo-a/" . "main")
           ("/repo-b/" . "develop"))))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/repo-a/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args) nil)))
      (should (equal "main" (magit-browse-commit-remote-default-head "master"))))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/repo-b/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _args) nil)))
      (should (equal "develop" (magit-browse-commit-remote-default-head "master"))))))

;;; --- Merge commit parsing tests (mocked) ---

(ert-deftest magit-browse-commit-test-parse-merge-commit ()
  "Find earliest merge commit from rev-list output."
  (cl-letf (((symbol-function 'magit-browse-commit-remote-default-head)
             (lambda (_default) "main"))
            ((symbol-function 'magit-git-lines)
             (lambda (&rest _args)
               '("cccccc" "bbbbbb" "aaaaaa"))))
    (should (equal "aaaaaa" (magit-browse-commit--parse-merge-commit "abc123")))))

(ert-deftest magit-browse-commit-test-parse-merge-commit-single ()
  "Handle single merge commit in rev-list output."
  (cl-letf (((symbol-function 'magit-browse-commit-remote-default-head)
             (lambda (_default) "main"))
            ((symbol-function 'magit-git-lines)
             (lambda (&rest _args)
               '("aaaaaa"))))
    (should (equal "aaaaaa" (magit-browse-commit--parse-merge-commit "abc123")))))

(ert-deftest magit-browse-commit-test-parse-merge-commit-none ()
  "Return empty string when no merge commits found."
  (cl-letf (((symbol-function 'magit-browse-commit-remote-default-head)
             (lambda (_default) "main"))
            ((symbol-function 'magit-git-lines)
             (lambda (&rest _args) nil)))
    (should (equal "" (magit-browse-commit--parse-merge-commit "abc123")))))

(provide 'magit-browse-commit-test)
;;; magit-browse-commit-test.el ends here
