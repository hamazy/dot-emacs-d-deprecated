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
(add-hook 'after-init-hook
	  'my-w3m-init)

;; clojure
(my-package-install 'nrepl)

;; scala
(my-package-install 'scala-mode)
(defun my-ensime-show-tooltips-nox ()
  (interactive)
  (ensime-tooltip-handler (point)))
(defun my-scala-init ()
  (require 'scala-mode-auto)
  (add-to-list 'load-path "/opt/local/ensime_2.9.2-0.9.8.1/elisp")
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (define-key ensime-mode-map "\C-c\C-v." 'my-ensime-show-tooltips-nox))
(add-hook 'after-init-hook
	  'my-scala-init)

;; gtags
(my-package-install 'gtags)
(defun my-gtags-init ()
  (autoload 'gtags-mode "gtags" "" t)
  (setq gtags-suggested-key-mapping t)
  (add-hook 'c++-mode-hook
	  '(lambda ()
	     (gtags-mode t)))
  (add-hook 'c-mode-hook
	  '(lambda ()
	     (gtags-mode t))))
(add-hook 'after-init-hook
	  'my-gtags-init)

;; coffee script
(my-package-install 'coffee-mode)
(defun my-coffee-init()
  (setq coffee-tab-width 2) )
(add-hook 'after-init-hook 'my-coffee-init)

;; asciidoc
(my-package-install 'adoc-mode)
(add-hook 'adoc-mode-hook '(lambda () (buffer-face-mode t)))


(setq gdb-many-windows t)

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

(setq tramp-default-method "ssh")

(defun silver (pdelta)
  (let* ((m-delta (* pdelta 10))
	(fee (* m-delta 0.5))
	(fee1 (ftruncate (* fee 0.1)))
	(fee2 (ftruncate (* fee 0.05)))
	(sum (+ fee1 fee2))
	(tax (ftruncate (* sum 0.05)))
	(charge (+ sum tax)))
    (list fee1 fee2 sum tax charge)))

;; set to the preferred frame size that fit to an 11 inch display
(if window-system
    (set-frame-size (selected-frame) 223 55))

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)))

