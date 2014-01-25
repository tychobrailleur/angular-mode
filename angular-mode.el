;; Copyright 2014 Sébastien Le Callonnec

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


(defun angular/find-root (file)
  (when file
    (let ((current-dir (file-name-directory file))
          (found nil))
      (while (not found)
        (if (file-exists-p (concat current-dir "app"))
            (setq found t)
          (setq current-dir (concat current-dir "../"))))
      (if found
          current-dir))))

(defun angular/test-file-p (root file)
  (string-match "^test/.*Spec\\.js$" (file-relative-name file root)))

(defun angular/find-root-from-current-buffer ()
  "Find the root of the project from the current buffer"
  (interactive)
  (message (angular/find-root (buffer-file-name (current-buffer)))))

(defun angular/toggle-test ()
  "Switch between file and its associated spec."
  (interactive)
  (let* ((current-buf (current-buffer))
         (current-file (buffer-file-name current-buf))
         (root (angular/find-root current-file)))
    (if (angular/test-file-p root current-file)
        (let ((path-to-test (file-relative-name current-file (concat root "test/spec"))))
          ;; TODO throw an error if spec doesn't exist.
          (find-file-other-window
           (concat root "/app/scripts/" (replace-regexp-in-string "\\(Ctrl\\)?Spec" "" path-to-test))))
      (let ((path-to-test (file-relative-name current-file (concat root "app/scripts"))))

        ;; If controller, replace with CtrlSpec.js
        (find-file-other-window
         (concat root "/test/spec/" (replace-regexp-in-string "\\.js$" "Spec.js" path-to-test))))
      )))

(define-keys custom-mode-keymap ((kbd "C-c t") 'angular/toggle-test))

(define-minor-mode angular-mode
  "Angular"
  nil
  "Angular"
  custom-mode-keymap)

(provide 'angular-mode)
