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

;; (setq debug-on-error t)

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

(defcustom angular/toggle-test-in-other-window t
  "Open test in other window."
  :type 'boolean
  :group 'angular)

(defun angular/angular-project-p (current-file)
  "Checking this project is an AngularJS project by 
ensuring app/directives directory exists."
  (let ((root (angular/find-root current-file)))
    (and (not (null root))
         (file-exists-p (concat (file-name-as-directory root)
                                (file-name-as-directory angular/scripts-dir)
                                "directives")))))

(defun angular/find-root (file)
  (when file
    (let ((current-dir (file-name-directory file))
          (found nil))
      (while (and (not found) (not (string= current-dir "/")))
        (if (file-exists-p (concat current-dir angular/source-dir))
            (setq found t)
          (setq current-dir (expand-file-name (concat current-dir "../")))))
      (if found
          current-dir))))

(defun angular/test-file-p (root file)
  (let ((relative-path (file-relative-name file root)))
    (string-match (concat "^" angular/test-dir "/.*" angular/spec-suffix "\\.js$") relative-path)))

(defun angular/controller-spec-p (root file)
  (let* ((spec-root (concat (file-name-as-directory (expand-file-name root)) angular/spec-dir))
         (relative-path (file-relative-name file spec-root)))
    (string-prefix-p (file-name-as-directory "controllers") relative-path)))

(defun angular/controller-p (root file)
  (let* ((scripts-root (concat (file-name-as-directory (expand-file-name root)) angular/scripts-dir))
         (relative-path (file-relative-name file scripts-root)))
    (string-prefix-p (file-name-as-directory "controllers") relative-path)))

(defun angular/find-root-from-current-buffer ()
  "Find the root of the project from the current buffer"
  (interactive)
  (angular/find-root (buffer-file-name (current-buffer))))

(defun angular/find-file (file)
  (if angular/toggle-test-in-other-window
      (find-file-other-window file)
    (find-file file)))

(defun angular/open-associated-script (root file)
  "Open the script file associated with the spec in FILE."
  (let ((suffix angular/spec-suffix)
        (relative-path (file-relative-name file (concat root angular/spec-dir))))
    (if (angular/controller-spec-p root file)
        (setq suffix angular/controller-spec-suffix))
    (angular/find-file
     (concat root (file-name-as-directory angular/scripts-dir)
             (replace-regexp-in-string suffix "" relative-path)))))

(defun angular/open-associated-test (root file)
  (let ((suffix angular/spec-suffix)
        (relative-path (file-relative-name file (concat root angular/scripts-dir))))
    (if (angular/controller-p root file)
        (setq suffix angular/controller-spec-suffix))
    (angular/find-file
     (concat root (file-name-as-directory angular/spec-dir)
             (replace-regexp-in-string "\\.js$" (concat suffix ".js") relative-path)))))

(defun angular/toggle-test ()
  "Switch between file and its associated spec."
  (interactive)
  (let* ((current-buf (current-buffer))
         (current-file (buffer-file-name current-buf))
         (root (angular/find-root current-file)))
    (if (angular/test-file-p root current-file)
        (let ((path-to-test (file-relative-name current-file (concat root angular/spec-dir))))
          (angular/open-associated-script root current-file))
      (let ((path-to-test (file-relative-name current-file (concat root angular/scripts-dir))))
        (angular/open-associated-test root current-file)))))

(defun angular/angular-list-of (resource-type)
  (let* ((current-buf (current-buffer))
         (current-file (buffer-file-name current-buf))
         (root (angular/find-root current-file))
         (resource-root (concat (file-name-as-directory root)
                                (file-name-as-directory angular/scripts-dir)
                                resource-type)))
    (directory-files resource-root t "\\.js$")))

(defun angular/angular-controllers-list ()
  (angular/angular-list-of "controllers"))

(defun angular/angular-directives-list ()
  (angular/angular-list-of "directives"))

(defun angular/angular-services-list ()
  (angular/angular-list-of "services"))

(defun angular/angular-views-list ()
    (let* ((current-buf (current-buffer))
         (current-file (buffer-file-name current-buf))
         (root (angular/find-root current-file))
         (resource-root (concat (file-name-as-directory root)
                                (file-name-as-directory angular/source-dir)
                                resource-type)))
    (directory-files resource-root t "\\.html$")))

(defvar helm-angular-controllers-list-cache nil)
(defvar helm-angular-controllers-list
  `((name . "Controllers")
    (init . (lambda ()
              (setq helm-angular-controllers-list-cache (angular/angular-controllers-list))))
    (candidates . helm-angular-controllers-list-cache)
    (type . file)))

(defvar helm-angular-directives-list-cache nil)
(defvar helm-angular-directives-list
  `((name . "Directives")
    (init . (lambda ()
              (setq helm-angular-directives-list-cache (angular/angular-directives-list))))
    (candidates . helm-angular-directives-list-cache)
    (type . file)))

(defvar helm-angular-services-list-cache nil)
(defvar helm-angular-services-list
  `((name . "Services")
    (init . (lambda ()
              (setq helm-angular-services-list-cache (angular/angular-services-list))))
    (candidates . helm-angular-services-list-cache)
    (type . file)))

(defvar helm-angular-views-list-cache nil)
(defvar helm-angular-views-list
  `((name . "Views")
    (init . (lambda ()
              (setq helm-angular-views-list-cache (angular/angular-views-list))))
    (candidates . helm-angular-views-list-cache)
    (type . file)))

(defun angular/helm-controllers ()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-angular-controllers-list)
                     "*helm angular*"))

(defun angular/helm-directives ()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-angular-directives-list)
                     "*helm angular*"))

(defun angular/helm-services ()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-angular-services-list)
                     "*helm angular*"))

(defun angular/helm-views ()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-angular-views-list)
                     "*helm angular*"))

(defun angular/helm-all ()
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-angular-controllers-list
                       helm-angular-directives-list
                       helm-angular-services-list
                       helm-angular-views-list)
                     "*helm angular*"))

(defvar angular-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap
      (kbd "C-c t") 'angular/toggle-test)
    (define-key keymap
      (kbd "C-c c") 'angular/helm-controllers)
    (define-key keymap
      (kbd "C-c d") 'angular/helm-directives)
    (define-key keymap
      (kbd "C-c s") 'angular/helm-services)
    (define-key keymap
      (kbd "C-c v") 'angular/helm-views)
    (define-key keymap
      (kbd "C-c a") 'angular/helm-all)
    keymap)
  "Key map for angular-mode.")

(define-minor-mode angular-mode
  "Angular JS minor mode."
  :group 'angular
  :lighter " Angular"
  :keymap angular-mode-keymap)

(add-hook 'find-file-hooks
          (lambda()
            (if (angular/angular-project-p (buffer-file-name))
                (angular-mode t))))

(provide 'angular-mode)
