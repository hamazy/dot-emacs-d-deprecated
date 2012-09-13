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
