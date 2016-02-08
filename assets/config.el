;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defun add-package (name)
  (unless (package-installed-p name)
    (package-install name)))

;;;;;;;;;;;
;; PATHS ;;
;;;;;;;;;;;

(defun emacs-dir/ (path)
  (concat user-emacs-directory path))

(setq backup-directory-alist `((,(emacs-dir/ ".") . ,(emacs-dir/ ".saves"))))

;;;;;;;;;;;;;;;;;;;;
;; USER INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(load-theme 'leuven t)

;;;;;;;;;;;;;;;;
;; MINIBUFFER ;;
;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-everywhere t)

(add-package 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(add-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;;;;;;;;;;;;
;; MODE LINE ;;
;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;
;; TEXT ENCODING ;;
;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;
;; STRUCTURAL EDITING ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(add-package 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;
;; LISP PROGRAMMING ;;
;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
