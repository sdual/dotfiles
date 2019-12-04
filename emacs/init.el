;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'smyx t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not create a backup file
(setq make-backup-files nil)
(setq auto-save-default nil)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

;; share the clipboard of mac os
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bind settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto complete: select a candidate by using C-p or C-n key.
;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; backspace key: crl-h
(global-set-key "\C-h" 'delete-backward-char)

;; window switching.
(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c n")  'windmove-down)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c f") 'windmove-right)

;; completion of brackets
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)

;; scroll:
(setq
 ;; The number of lines to be scrolled with mouse wheel.
 mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control)))
 ;; ignore speed.
 mouse-wheel-progressive-speed nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default background color
(set-face-background 'default "#282828")

;; line number settings.
(global-linum-mode t)

;; tab width.
(setq default-tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq-default neo-show-hidden-files t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company and bind-key.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(require 'bind-key)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(bind-key "C-M-i" 'company-complete)
(bind-key "C-h" nil company-active-map)
(bind-key "C-n" 'company-select-next company-active-map)
(bind-key "C-p" 'company-select-previous company-active-map)
(bind-key "C-n" 'company-select-next company-search-map)
(bind-key "C-p" 'company-select-previous company-search-map)
(bind-key "<tab>" 'company-complete-common-or-cycle company-active-map)
(bind-key "<backtab>" 'company-select-previous company-active-map)
(bind-key "C-i" 'company-complete-selection company-active-map)
(bind-key "M-d" 'company-show-doc-buffer company-active-map)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-maximum-width 50)

;; company-quickhelp
(setq company-quickhelp-color-foreground "white")
(setq company-quickhelp-color-background "dark slate gray")
(setq company-quickhelp-max-lines 5)
(company-quickhelp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown file settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disactivate whitespace-action in markdown file.
(defvar delete-trailing-whitespece-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespece-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
; hook to disable deleting whitespace.
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec-path-from-shell settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq exec-path-from-shell-check-startup-files nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dumb-jump, smart-jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dumb-jump
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)

;; smart-jump
(smart-jump-setup-default-registers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'go-mode-hook 'eglot-ensure)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls"))))

;; run gofmt automatically at saving the go file.
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda()
           (company-mode)
           (setq indent-tabs-mode nil)
           (setq c-basic-offset 4)
           (setq tab-width 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'eglot-ensure)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; others.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (python-mode markdown-mode smart-jump neotree go-mode exec-path-from-shell eglot company-quickhelp bind-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
