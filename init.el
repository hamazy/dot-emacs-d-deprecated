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
    (package-refresh-contents)
    (package-install name)))

;; anything, anything-config
(my-package-install 'anything)
(my-package-install 'anything-config)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'anything-config)
	    (global-set-key
	     "\C-x;"
	     (lambda ()
	       (interactive)
	       (anything-other-buffer
		'(anything-c-source-buffers
		  anything-c-source-buffer-not-found
		  anything-c-source-file-name-history
		  anything-c-source-info-pages
		  anything-c-source-man-pages
		  anything-c-source-emacs-commands
		  anything-c-source-kill-ring)
		" *my-anything*")))))

;; twittering-mode
(my-package-install 'twittering-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'twittering-mode)
	    (setq twittering-icon-mode t)
	    (setq twittering-use-master-password nil)
	    (setq twittering-oauth-invoke-browser t)
	    (setq twittering-status-format "%i %S @%s %@:\n%FILL[  ]{%T via %f%r%R}\n")
	    (setq twittering-convert-fix-size 32)))

;; auto-complete
(my-package-install 'auto-complete)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'auto-complete)
	    (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
	    (require 'auto-complete-config)
	    (ac-config-default)))

;; w3m package needs w3m command to be in the exec-path.
(defun my-add-to-path (dir)
  (let ((sep (if (eq system-type 'windows-nt) ";" ":")))
    (when (file-accessible-directory-p dir)
      (setq exec-path (append (list dir) exec-path))
      (let ((path-env-key "PATH"))
	(setenv path-env-key (concat dir sep (getenv path-env-key)))))))
(my-add-to-path "/opt/local/bin")
(my-add-to-path "/Users/hamazy/Downloads/ruby-1.9.3-p429-dist/bin/")

;; w3m
(my-package-install 'w3m)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'w3m)))

;; clojure
(my-package-install 'nrepl)

;; scala
(my-package-install 'scala-mode2)
(require 'cl)
(lexical-let* ((ensime-root "~/.ensime")
	       (ensime-lisp-dir (concat (file-name-as-directory ensime-root) "elisp")))
  (when (file-exists-p ensime-lisp-dir)
    (add-hook 'after-init-hook
	      (lambda ()
		(add-to-list 'load-path ensime-lisp-dir)
		(require 'ensime)
		(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode nil)))
		(define-key ensime-mode-map "\C-c\C-v."
		  (lambda ()
		    (interactive)
		    (ensime-tooltip-handler (point))))))))
;; install sbt with:
;; curl https://raw.github.com/hamazy/misc-setups/master/install-sbt.sh | sh
(my-add-to-path "~/.sbt-0.12.1/bin")

;; gtags
(my-package-install 'gtags)
(add-hook 'after-init-hook
	  (lambda ()
	    (autoload 'gtags-mode "gtags" "" t)
	    (setq gtags-suggested-key-mapping t)
	    (add-hook 'c++-mode-hook
		      '(lambda ()
			 (gtags-mode t)))
	    (add-hook 'c-mode-hook
		      '(lambda ()
			 (gtags-mode t)))))

;; coffee script
(my-package-install 'coffee-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq coffee-tab-width 2) ))

;; asciidoc
(my-package-install 'adoc-mode)
(add-hook 'adoc-mode-hook
	  (lambda ()
	    (buffer-face-mode t)))

;; moe-theme
(my-package-install 'moe-theme)
(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'moe-dark t)))

;; for gdb
(setq gdb-many-windows t)

;; for ruby
;; git clone https://github.com/Mon-Ouie/ruby-dev.el.git
;; gem install pry
;; gem install yard
(add-to-list 'load-path "~/.emacs.d/ruby-dev.el" )
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)
(add-hook 'ruby-mode-hook 'turn-on-ruby-dev)

(dolist (file-pattern '("\\.rake$" "Rakefile$" "Gemfile" "\\.gemspec$" "Guardfile"))
  (add-to-list 'auto-mode-alist (cons file-pattern 'ruby-mode)))

;; for rails
(my-package-install 'rinari)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq rinari-major-modes
		  (list 'ruby-mode-hook))))

;; for irfc
(my-package-install 'irfc)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'irfc)
	    (setq irfc-directory "~/.emacs.d/irfc")
	    (setq irfc-assoc-mode t)))

;; mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-ssl-verify-level 0)

;; for go language
(my-package-install 'go-mode)

;; install aspell with:
;; $ sudo port install aspell aspell-dict-en
(setq ispell-program-name "aspell")

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

;; to use with emacsclient
(server-start)
