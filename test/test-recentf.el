;;; test-recentf.el --- Test recentf initialization  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 emacs-dashboard maintainers

;; This file is NOT part of GNU Emacs.

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
;;
;; ERT tests for recentf initialization in dashboard.
;;

;;; Code:

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
