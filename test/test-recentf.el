;;; test-recentf.el --- Test recentf initialization  -*- lexical-binding: t; -*-

(require 'ert)
(require 'recentf)

(ert-deftest dashboard-recentf-initialized-before-capture ()
  "Ensure recentf-mode is active before the list is captured.
Regression test for issue #166."
  (let ((recentf-mode nil))
    (recentf-mode 1)
    (should (recentf-enabled-p))))

(ert-deftest dashboard-recentf-mode-idempotent ()
  "Calling `recentf-mode' when already enabled is a no-op."
  (recentf-mode 1)
  (let ((list-before recentf-list))
    (recentf-mode 1)
    (should (equal recentf-list list-before))))

;;; test-recentf.el ends here
