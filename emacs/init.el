;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'smyx t)

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


;; scroll settings.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq-default neo-show-hidden-files t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown file settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disactivate whitespace-action in markdown file.
(defvar delete-trailing-whitespece-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespece-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
; hook to be disabled.
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))
