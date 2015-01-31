;; i like simple looking window
(menu-bar-mode -1)
(when window-system 
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(column-number-mode t)
(display-time-mode t)
(setq display-time-day-and-date t)
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-function-modifier 'hyper)
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-?") 'help)

(global-hl-line-mode t)
(electric-pair-mode t)
(defalias 'list-buffers 'ibuffer)

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

(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'emacs-lisp-mode-hook
		      (lambda ()
			(when (fboundp 'prettify-symbols-mode)
			    (prettify-symbols-mode))))))

(my-package-install 'magit)

;; helm
(my-package-install 'helm)
(my-package-install 'helm-ls-git)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'helm-config)
	    (require 'helm-man)
	    (require 'helm-ring)
	    (require 'helm-buffers)
	    (require 'helm-files)
	    (require 'helm-info)
	    (global-set-key
	     (kbd "C-x ;")
	     (lambda ()
	       (interactive)
               (unless helm-source-buffers-list
                 (setq helm-source-buffers-list
                       (helm-make-source "Buffers" 'helm-source-buffers)))
	       (helm-other-buffer
		'(helm-source-buffers-list
		  helm-source-recentf
		  helm-source-buffer-not-found
		  helm-source-file-name-history
		  helm-source-man-pages
		  helm-source-info-pages
		  helm-source-kill-ring)
		"*my-helm*")))
	    (global-set-key (kbd "M-x") 'helm-M-x)
	    (global-set-key (kbd "C-x C-f") 'helm-find-files)
	    (add-hook 'helm-after-initialize-hook
		      (lambda ()
			(define-key helm-map (kbd "C-h") 'delete-backward-char)
			(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)))
	    (setq helm-split-window-default-side 'right)))

;; twittering-mode
(my-package-install 'twittering-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'twittering-mode)
	    (setq twittering-icon-mode nil)
	    (setq twittering-use-master-password nil)
	    (setq twittering-oauth-invoke-browser t)
	    (setq twittering-status-format "%i %S @%s %@:\n%FILL[  ]{%T via %f%r%R}\n")
	    (setq twittering-retweet-format '(nil _ " RT @%s: %t"))
	    (when (executable-find "convert")
	      ;; this needs convert available, which can be installed
	      ;; with `brew install imagemagick`.
	      (setq twittering-convert-fix-size 32))))

;; w3m package needs w3m command to be in the exec-path.
(defun my-add-to-path (dir)
  (let ((sep (if (eq system-type 'windows-nt) ";" ":")))
    (when (file-accessible-directory-p dir)
      (setq exec-path (append (list dir) exec-path))
      (let ((path-env-key "PATH"))
	(setenv path-env-key (concat dir sep (getenv path-env-key)))))))
(my-add-to-path "c:/Program Files/Git/bin")
(my-add-to-path "/usr/local/bin")

;; w3m
(my-package-install 'w3m)
(when (executable-find "w3m")
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'w3m))))

;; clojure
(my-package-install 'cider)
(my-add-to-path "~/.lein/bin")
(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'clojure-mode-hook
		      (lambda ()
			(when (fboundp 'prettify-symbols-mode)
			    (add-to-list 'prettify-symbols-alist '("->" . 8594))
			  (prettify-symbols-mode))))
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
            (setq nrepl-log-messages t)
            (setq nrepl-hide-special-buffers t)
))

;; scala
(my-package-install 'scala-mode2)
(my-package-install 'ensime)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'ensime)
	    (add-hook 'scala-mode-hook
		      (lambda ()
			(ensime-scala-mode-hook)	
			(add-to-list 'ac-sources 'ac-source-dictionary t)
			(add-to-list 'ac-sources 'ac-source-abbrev t)
			(add-to-list 'ac-sources 'ac-source-yasnippet t)
			(add-to-list 'ac-sources 'ac-source-words-in-buffer t)
			(add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers t)
			(define-abbrev-table 'scala-mode-abbrev-table
			  '(("rarr" "→") ("rdarr" "⇒") ("larr" "←")))
			(abbrev-mode t)))
	    (define-key ensime-mode-map (kbd "C-c C-v .")
	      (lambda ()
		(interactive)
		(ensime-tooltip-handler (point))))
	    (add-to-list 'ensime-doc-lookup-map
			 '("^spray\\." . (lambda (type &optional member)
					   (ensime-make-scala-doc-url-helper
					    "http://spray.io/documentation/1.1-SNAPSHOT/api/" type member))))
	    (add-to-list 'ensime-doc-lookup-map
			 '("^akka\\." . (lambda (type &optional member)
					  (ensime-make-scala-doc-url-helper
					   "http://doc.akka.io/api/akka/2.2.3/" type member))))
	    (add-to-list 'ensime-doc-lookup-map
			 '("^scala.slick\\." . (lambda (type &optional member)
						 (ensime-make-scala-doc-url-helper
						  "http://slick.typesafe.com/doc/2.0.0/api/" type member))))
	    (add-to-list 'ensime-doc-lookup-map
			 '("^x?sbti?\\." . (lambda (type &optional member)
					     (ensime-make-scala-doc-url-helper
					      "http://www.scala-sbt.org/release/api/" type member))))
	    (add-to-list 'ensime-doc-lookup-map
			 '("^org.scalatest" . (lambda (type &optional member)
						(ensime-make-scala-doc-url-helper
						 "http://doc.scalatest.org/2.1.5/" type member))))))
(let ((brew (executable-find "brew")))
  (when (and brew (not (executable-find "scala")))
    (shell-command (concat brew " install scala"))))

;; sbt
(my-add-to-path "~/.sbt-0.13.6/bin")
(when (not (executable-find "sbt"))
  (let ((sh (executable-find "sh"))
	(curl (executable-find "curl")))
    (when (and curl sh)
      (shell-command
       (concat curl " https://raw.githubusercontent.com/hamazy/misc-setups/master/install-sbt.sh | SBT_VERSION=0.13.6 " sh)))))

(add-to-list 'auto-mode-alist '("\\.cls$" . java-mode))

;; haskell
(my-package-install 'haskell-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'haskell-mode-hook
		      (lambda ()
			(turn-on-haskell-indentation)
			(turn-on-haskell-decl-scan)
			(when (fboundp 'prettify-symbols-mode)
			  (add-to-list 'prettify-symbols-alist '("<-" . 8592))
			  (add-to-list 'prettify-symbols-alist '("->" . 8594))
			  (add-to-list 'prettify-symbols-alist '("=>" . 8658))
			  (add-to-list 'prettify-symbols-alist '("\\" . 955))
			  (prettify-symbols-mode))))
	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (custom-set-variables
	     '(haskell-process-suggest-remove-import-lines t)
	     '(haskell-process-auto-import-loaded-modules t)
	     '(haskell-process-log t)
	     '(haskell-process-type 'cabal-repl))
	    (eval-after-load "haskell-mode"
	      '(progn
		 (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
		 (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
		 (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
		 (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
		 (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
		 (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
		 (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
		 (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
		 (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
		 (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
		 (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)))
	    (eval-after-load "haskell-cabal"
	      '(progn
		 (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
		 (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
		 (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
		 (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
	    (eval-after-load "which-func"
	      '(add-to-list 'which-func-modes 'haskell-mode))))
(my-add-to-path "~/.cabal/bin")

;; auto-complete
(my-package-install 'auto-complete)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'auto-complete)
	    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
	    (require 'auto-complete-config)
	    (ac-config-default)))

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
(let ((brew (executable-find "brew")))
  (when (and brew (not (executable-find "gtags")))
        (shell-command (concat brew " install global"))))

;; coffee script
(my-package-install 'coffee-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'coffee-mode-hook
		      (lambda ()
			(when (fboundp 'prettify-symbols-mode)
			    (add-to-list 'prettify-symbols-alist '("->" . 8594))
			  (add-to-list 'prettify-symbols-alist '("=>" . 8658))
			  (prettify-symbols-mode))))
	    (setq coffee-tab-width 2) ))

;; javascript
(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'js-mode-hook
		      (setq js-indent-level 2))))

;; asciidoc
(my-package-install 'adoc-mode)
(add-hook 'adoc-mode-hook
	  (lambda ()
	    (buffer-face-mode t)))

;; moe-theme
(my-package-install 'moe-theme)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'moe-theme)
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
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'go-mode)
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    (add-hook 'go-mode-hook (lambda ()
				      (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
				      (local-set-key (kbd "C-c i") 'go-goto-imports)))
	    ))
(when (file-exists-p "~/.gocode")
  (my-add-to-path "~/.gocode/bin")
  (setenv "GOPATH" "~/.gocode"))
(when (file-exists-p "~/.gocode/src/github.com/nsf/gocode/emacs-company/company-go.el")
  (my-package-install 'company)
  (add-to-list 'load-path "~/.gocode/src/github.com/nsf/gocode/emacs-company/")
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'company)
	      (require 'company-go)
	      (setq company-tooltip-limit 20)
	      (setq company-minimum-prefix-length 0)
	      (setq company-idle-delay .3)
	      (setq company-echo-delay 0)
	      (setq company-begin-commands '(self-insert-command))
	      (add-hook 'go-mode-hook
			(lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode))))))
(my-package-install 'flymake)
(my-package-install 'flycheck)
(when (file-exists-p "~/.gocode/src/github.com/dougm/goflymake/go-flymake.el")
  (add-to-list 'load-path "~/.gocode/src/github.com/dougm/goflymake/")
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'go-flymake)
	      (require 'go-flycheck))))

(my-package-install 'markdown-mode)

(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; install aspell with:
;; $ sudo port install aspell aspell-dict-en
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))

(my-package-install 'yaml-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'yaml-mode)
	    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))))

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'after-init-hook
          (lambda () (setq org-todo-keywords
                      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))))

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
            (global-set-key (kbd "C-x C-j") 'skk-mode)))

;; to use with emacsclient
(server-start)
