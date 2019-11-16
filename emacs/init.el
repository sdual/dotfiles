;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bind settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto complete: select a candidate by using C-p or C-n key.
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; backspace key: crl-h
(global-set-key "\C-h" 'delete-backward-char)

(setq
 ;; ホイールでスクロールする行数を設定
 mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control)))
 ;; 速度を無視する
 mouse-wheel-progressive-speed nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default background color
(set-face-background 'default "#282828")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown file settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disactivate whitespace-action in markdown file.
(defvar delete-trailing-whitespece-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespece-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)
; 無効にしたいモードのhook
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))
