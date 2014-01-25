;; Copyright (c) 2014 Sébastien Le Callonnec

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar angular-version "0.1.0")

(defgroup angular nil
  "Angular Minor mode Group."
  :group 'programing
  :prefix "angular/")

(defcustom angular/source-dir "app"
  "Location of the application sources in the project."
  :type 'string
  :group 'angular)

(defcustom angular/scripts-dir (concat angular/source-dir "/scripts")
  "Location of scripts in the angular project."
  :type 'string
  :group 'angular)

(defcustom angular/test-dir "test"
  "Location of tests in the angular project."
  :type 'string
  :group 'angular)

(defcustom angular/spec-dir (concat angular/test-dir "/spec")
  "Location of specs in the angular project."
  :type 'string
  :group 'angular)

(defcustom angular/spec-suffix "Spec"
  "Suffix for specs."
  :type 'string
  :group 'angular)

(defcustom angular/controller-spec-suffix "CtrlSpec"
  "Suffix for controllers specs."
  :type 'string
  :group 'angular)

(defun angular/find-root (file)
  (when file
    (let ((current-dir (file-name-directory file))
          (found nil))
      (while (not found)
        (if (file-exists-p (concat current-dir angular/source-dir))
            (setq found t)
          (setq current-dir (concat current-dir "../"))))
      (if found
          current-dir))))

(defun angular/test-file-p (root file)
  (let ((relative-path (file-relative-name file root)))
    (string-match (concat "^" angular/test-dir "/.*" angular/spec-suffix "\\.js$") relative-path)))

(defun angular/find-root-from-current-buffer ()
  "Find the root of the project from the current buffer"
  (interactive)
  (message (angular/find-root (buffer-file-name (current-buffer)))))

(defun angular/open-associated-script (file)
  (find-file-other-window
   (concat root (file-name-as-directory angular/scripts-dir) (replace-regexp-in-string "\\(Ctrl\\)?Spec" "" file))))

(defun angular/open-associated-test (file)
  (find-file-other-window
   (concat root (file-name-as-directory angular/spec-dir) (replace-regexp-in-string "\\.js$" "Spec.js" file))))

(defun angular/toggle-test ()
  "Switch between file and its associated spec."
  (interactive)
  (let* ((current-buf (current-buffer))
         (current-file (buffer-file-name current-buf))
         (root (angular/find-root current-file)))
    (if (angular/test-file-p root current-file)
        (let ((path-to-test (file-relative-name current-file (concat root angular/spec-dir))))
          (angular/open-associated-script path-to-test))
      (let ((path-to-test (file-relative-name current-file (concat root angular/scripts-dir))))
        (angular/open-associated-test path-to-test)))))

(defvar angular-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap
      (kbd "C-c t") 'angular/toggle-test)
    keymap)
  "Key map for angular-mode.")

(define-minor-mode angular-mode
  "Angular JS minor mode."
  :group 'angular
  :lighter " Angular"
  :keymap angular-mode-keymap)

(provide 'angular-mode)
