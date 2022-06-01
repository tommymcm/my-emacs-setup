;; Fix bug in Emacs < 26.2
(if (version< emacs-version "26.2")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install Packages
;;  list of packages to install
(setq package-list
      '(autodisass-llvm-bitcode
        use-package
        markdown-mode
        yaml-mode
        jbeans-theme))

;;  fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;;  install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(global-display-line-numbers-mode)

(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(put 'upcase-region 'disabled nil)

;; Language Server
; (use-package lsp-mode :commands lsp)
; (use-package lsp-ui :commands lsp-ui-mode)

; (use-package ccls
;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;          (lambda () (require 'ccls) (lsp))))

;; yaml mode
(define-derived-mode yaml-mode fundamental-mode "YamlMode"
  "Comments start with `#."
  (set (make-local-variable 'comment-start) "#"))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Makefile mode
(require 'make-mode)
(defconst makefile-nmake-statements
  `("!IF" "!ELSEIF" "!ELSE" "!ENDIF" "!MESSAGE" "!ERROR" "!INCLUDE" ,@makefile-statements)
  "List of keywords understood by nmake.")

(defconst makefile-nmake-font-lock-keywords
  (makefile-make-font-lock-keywords
   makefile-var-use-regex
   makefile-nmake-statements
   t))

(define-derived-mode makefile-nmake-mode makefile-mode "nMakefile"
  "An adapted `makefile-mode' that knows about nmake."
  (setq font-lock-defaults
                  `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))

(setq auto-mode-alist
              (cons '("\\.mak\\'" . makefile-nmake-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("/Makefile.*\\'" . makefile-nmake-mode) auto-mode-alist))

;; Paren matching
(show-paren-mode 1)
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Open multi-files with vert split
(setq
 split-width-threshold 0
 split-height-threshold nil)

;; Turn off cursor blinking
(setq visible-cursor nil)

;; Load plugins
(setq load-path
      (cons (expand-file-name "~/emacs-plugins/emacs") load-path))
(require 'llvm-mode)
(require 'tablegen-mode)

(setq load-path
      (cons (expand-file-name "/project/extra/llvm/9.0.0/llvm-9.0.0.src/tools/clang/tools/clang-format") load-path))
(require 'clang-format)

(setq load-path
      (cons (expand-file-name "~/emacs-plugins/autodisass-llvm-bitcode") load-path))
(require 'autodisass-llvm-bitcode)

;; User Functions
(defun prev-window ()
  (interactive)
  (other-window -1))

;; Keybindings
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(global-set-key (kbd "C-f") 'clang-format-buffer)
(global-set-key (kbd "M-;") 'comment-line)
(global-unset-key (kbd "M-."))
(global-unset-key (kbd "M-,"))
(global-unset-key (kbd "M-=")) 
(global-set-key (kbd "M-.") 'other-window)
(global-set-key (kbd "M-,") 'prev-window)
(global-set-key (kbd "M-=") 'balance-windows)

;; Aliases
(defalias 'rs 'replace-string)
(defalias 'swp 'window-swap-states)
(defalias 'linenum 'display-line-numbers-mode)

;; Merge mode
(defalias 'merge 'smerge-mode)
(global-unset-key (kbd "C-j"))
(setq smerge-command-prefix (kbd "C-j"))

;; Theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(load-theme 'jbeans t)

;; Theme customization
(set-face-attribute 'lazy-highlight nil
                    :weight 'bold)

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3147f60a6e56480728bd702d3874a8927efee05bc5fea43b9b6fc9a6fa45b331" "30d1a7ea1425dd52b64e4ae7cbdf093985c2a62f11d9f7d34cf36f08152c234d" default)))
 '(linum-format " %3i " t)
 '(package-selected-packages (quote (ccls lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
