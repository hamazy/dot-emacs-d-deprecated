;; i like simple looking window
(menu-bar-mode -1)
(when window-system 
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(column-number-mode t)
(display-time-mode t)
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'meta)

(global-set-key "\C-h" 'delete-backward-char)

(global-hl-line-mode t)

;; package
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-list-packages t)
(defun my-package-install (name) 
  (when (not (package-installed-p name))
    (package-install name)))

;; anything, anything-config
(my-package-install 'anything)
(my-package-install 'anything-config)
(defun my-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers
     anything-c-source-buffer-not-found
     anything-c-source-file-name-history
     anything-c-source-info-pages
     anything-c-source-man-pages
     anything-c-source-emacs-commands
     anything-c-source-kill-ring)
   " *my-anything*"))
(defun my-anything-init ()
  (require 'anything-config)
  (global-set-key
   "\C-x;" 'my-anything))
(add-hook 'after-init-hook
	  'my-anything-init)

;; twittering-mode
(my-package-install 'twittering-mode)
(defun my-twittering-mode-init ()
  (require 'twittering-mode)
  (setq twittering-icon-mode nil)
  (setq twittering-use-master-password nil))
(add-hook 'after-init-hook
	  'my-twittering-mode-init)

;; auto-complete
(my-package-install 'auto-complete)
(defun my-auto-complete-init ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (require 'auto-complete-config)
  (ac-config-default))
(add-hook 'after-init-hook
	  'my-auto-complete-init)

;; w3m package needs w3m command to be in the exec-path.
(defun my-add-to-path (dir)
  (let ((sep (if (eq system-type 'windows-nt) ";" ":")))
    (when (file-accessible-directory-p dir)
      (setq exec-path (append (list dir) exec-path))
      (let ((path-env-key "PATH"))
	(setenv path-env-key (concat dir sep (getenv path-env-key)))))))
(my-add-to-path "/opt/local/bin")

;; w3m
(my-package-install 'w3m)
(defun my-w3m-init ()
  (require 'w3m))

;; mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-ssl-verify-level 0)

;; font settings
(defun font-exists-p (font)
  (member font (font-family-list)))
(when (font-exists-p "Monaco")
  (set-face-attribute 'default nil
		      :family "Monaco"
		      :height 100))
(when (font-exists-p "Hiragino Maru Gothic Pro")
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'katakana-jisx0201
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1")))
