;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
;; add mepla to the package-archives list
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")) ; stable melpa package repository
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")) ; bleeding-edge melpa package repository
(package-initialize)

;; proxy-setup
;;-------------
;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("http" . "proxy.com:8080")
;;      ("https" . "proxy.com:8080")))

;; use-package
;;-------------
(unless (package-installed-p 'use-package) ; installs use-package
  (package-refresh-contents)
  (package-install 'use-package))

;; loads use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)


;; install required packages
;;---------------------------
(use-package auto-package-update ; used to auto update packages
  :ensure t
  :pin melpa-stable
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)) ; auto update packages on startup

;; (use-package diminish ; used to remove or change minor mode strings in your mode-line
;;  :ensure t
;;  :pin melpa-stable)

(use-package evil ; evil - adds vim-like functionality (vim-mode)
  :ensure t ; auto install package
  :pin melpa ; dependence (goto-chr) does not exist in melpa stable, so use melpa repository instead
  :defer .1 ; don't block emacs when starting, load evil immediately afterwards
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

  ;; ;; use default emacs behaviour in the following modes
  ;; (dolist (mode '(ag-mode
  ;;                 flycheck-error-list-mode
  ;;                 git-rebase-mode
  ;;                 neotree-mode))
  ;;   (add-to-list 'evil-emacs-state-modes mode))
  ;; ;; use hjkl and a few other key-bindgins in evil-emacs mode
  ;; (evil-add-hjkl-bindings occur-mode-map 'emacs
  ;;   (kbd "/")       'evil-search-forward
  ;;   (kbd "n")       'evil-search-next
  ;;   (kbd "N")       'evil-search-previous
  ;;   (kbd "C-d")     'evil-scroll-down
  ;;   (kbd "C-u")     'evil-scroll-up
  ;;   (kbd "C-w C-w") 'other-window))

(use-package swiper ; overhauls search, also includes ivy (completion system)
  :ensure t ; auto install package
  :pin melpa-stable
  :diminish ivy-mode
  :init
  (use-package counsel ; contains all counsel* functions which use ivy completion
    :ensure t ; auto install package
    :pin melpa-stable
    :bind
    ;; ivy-based interface to standard commands
    (("M-x" . counsel-M-x)
     ("C-x y" . counsel-yank-pop)
     ("C-x C-f" . counsel-find-file)
     ("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)
     ;; ("<f1> l" . counsel-load-library)
     ("C-h S" . counsel-info-lookup-symbol)
     ;; ("f2> l" . counsel-unicode-char)
     ;; ivy-based interface to sheel and system tools
     ;; ("C-c g" . counsel-git)
     ;; ("C-c j" . counsel-git-grep)
     ;; ("C-c k" . counsel-ag)
     ;; ("C-x l" . counsel-locate)
    ))
  (ivy-mode 1) ; enable ivy-mode (use ivy completion anywhere completing-read-function is used)
  :bind
  ;; ivy-based interface to standard commands
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)) ; resumes the last ivy-based completion
  :config
  (setq ivy-use-virtual-buffers t ; add recent files to buffers list
        ivy-count-format "(%d/%d) "))

;; (use-package web-mode ; mode for editing web templates
;;   :ensure t ; auto install package
;;   :pin melpa-stable
;;   :mode (("\\.phtml\\'" . web-mode)
;;          ("\\.tpl\\|\\.php\\'" . web-mode)
;;          ("\\.[agj]sp\\'" . web-mode)
;;          ("\\.as[cp]x\\'" . web-mode)
;;          ("\\.erb\\'" . web-mode)
;;          ("\\.mustache\\'" . web-mode)
;;          ("\\.djhtml\\'" . web-mode)
;;          ("\\.html?\\'" . web-mode)))

(use-package csharp-mode ; mode for editing C# files
  :ensure t ; auto install package
  :pin melpa-stable
  :mode "\\.cs\\'"
  :config
  (add-hook 'electric-pair-local-mode 1)) ;; enables electric-pair-mode when c# file is loaded up

;; (use-package omnisharp ; autocompletion and syntax checking for C# files (remember to M-X omnisharp-install-server)
;;   :ensure t ; auto install package
;;   :pin melpa-stable
;;   :config
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)) ;; auto start omnisharp  when C# file is loaded up
;;   ;; (define-key omnisharp-active-map (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;   ;; (define-key omnisharp-active-map (kbd "C-c C-c") 'recompile))

(use-package flycheck ; syntax checker
  :ensure t ; auto install package
  :pin melpa-stable
  :hook ('prog-mode-hook . #'flycheck-mode)) ; enable flycheck-mode on any programming language)
  ;; :config
  ;; (global-flycheck-mode) ; enables global-flycheck-mode
  ;; (add-hook 'csharp-mode-hook #'flycheck-mode)) ; enables flycheck-mode when C# files are loaded up

(use-package markdown-mode ; mark down preview
  :ensure t
  :pin melpa-stable
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(if (display-graphic-p)
  (use-package doom-themes ; doom theme (looks awesome!)
    :ensure t ; auto install package
    :pin melpa-stable
    :config
    ;;; Settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t  ; if nil, italics is universally disabled
          ;; doom-one specific settings
          doom-one-brighter-modeline nil
          doom-one-brighter-comments nil)
    (load-theme 'doom-one t))

  ;; requires M-x all-the-icons-install-fonts (on windows chose a directory to download the fonts too and manually install)
  (use-package doom-modeline ; doom modeline (looks awesome!)
    :ensure t
    :pin melpa-stable
    :hook (after-init . doom-modeline-mode)))

;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------
(setq inhibit-startup-screen t) ; disable startup screen

(setq auto-save-default nil); disable auto save
;; (setq make-backup-files nil) ; disable backup
;; stop littering everywhere with save files, put them somewhere else
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; sets custom file location
(setq custom-file "~/.emacs.d/custom.el")

;; (desktop-save-mode 1) ; remember what I had open when I quit

(setq-default indent-tabs-mode nil) ; TAB inserts SPACE's
(fset 'yes-or-no-p 'y-or-n-p) ; changes all yes/no questions to y/n type

;; delete trailing white-space from entire buffer before saving
;; Markdown relies on whitespace (E.g. 2 whitespaces at end of line indicates new line)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; list all plugins installed
;; (with-temp-file "~/.emacs.d/packages.txt" (insert (format "%S" package-activated-list)))


;;------------------------------------------------------------------------------
;; Theme
;;------------------------------------------------------------------------------
(menu-bar-mode -1) ; disables the menu bar
(tool-bar-mode -1) ; disables the tool bar
(scroll-bar-mode -1) ; disables the scroll bar

(show-paren-mode 1) ; highlights matching parenthesis
(column-number-mode 1) ; display column/row of cursor in mode-line

; shows line numbers on the left side of the buffer for programming files
(add-hook 'prog-mode-hook #'linum-mode)


;;------------------------------------------------------------------------------
;; Python
;;------------------------------------------------------------------------------
;; disables python native completion setup failed
(setq python-shell-completion-native-enable nil)


;;------------------------------------------------------------------------------
;; User defined variables
;;------------------------------------------------------------------------------
(setq user-emacs-file "~/.emacs.d/init.el") ; path to 'init.el' file
(setq user-emacs-file "~/.emacs") ; path to '.emacs' file

(when (or (eq window-system 'ms-dos) (eq window-system 'windows-nt))
  (setq tramp-default-method "plink"))

;; starts server if not running
;; use emacsclient.exe [file] to open file in current emacs buffer
;; use emacsclient.exe -n [file] to open file in another emacs buffer
;; use emacsclient.exe -c [file] to open file in another emacs window
(add-hook 'after-init-hook
    (lambda ()
        (require 'server)
        (setq server-socket-dir "~/.emacs.d/server") ; path to server directory
        (unless (server-running-p)
        (server-start))))


;;------------------------------------------------------------------------------
;; User defined functions
;;------------------------------------------------------------------------------

;; opens 'init.el' file in another window
(defun find-user-init-file ()
  "Edit the 'user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;; opens '.emacs' file in another window
(defun find-user-emacs-file ()
  "Edit the 'user-emacs-file', in another window."
  (interactive)
  (find-file user-emacs-file))

;; open remote file using tramp
(defun remote-file ()
  (interactive)
  (let ((prefix "/")
        (join ":")
        (hostname (read-string "Enter hostname: "))
        (filename (read-string "Enter path: ")))
    (find-file (concat prefix hostname join filename))))


;;------------------------------------------------------------------------------
;; User define keybindings
;;------------------------------------------------------------------------------
(global-set-key (kbd "C-!") 'shell) ; opens up the default shell

(global-set-key (kbd "C-x \\") 'split-window-horizontally) ; splits window horizontally
(global-set-key (kbd "C-x -") 'split-window-vertically) ; splits window vertically

(global-set-key (kbd "C-c C-c") 'comment-line) ; comments/uncomments a line

;; use more advanced buffer
;; 'm' marks current buffer
;; 'D' kills all marked buffers
;; 't' toggles markers
;; 'x' kills current buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; user define function bindings
;;---------------------------------
(global-set-key (kbd "C-c i") 'find-user-init-file)
(global-set-key (kbd "C-c e") 'find-user-emacs-file)
