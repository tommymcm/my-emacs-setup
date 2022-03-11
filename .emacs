
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;;  and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Install Packages
;;  list of packages to install
(setq package-list '(autodisass-llvm-bitcode ujelly-theme jbeans-theme))

;;  fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;;  install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Personal settings
(global-display-line-numbers-mode)

(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(put 'upcase-region 'disabled nil)

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

;; Make it so line numbers dont get copied
(setq linum-format "%4d \u2502 ")

;; Turn off cursor blinking
(setq visible-cursor nil)

;; Load plugins
(setq load-path
      (cons (expand-file-name "~/workspace/llvm/llvm/utils/emacs") load-path))
(require 'llvm-mode)
(require 'tablegen-mode)

(setq load-path
      (cons (expand-file-name "~/workspace/llvm/clang/tools/clang-format") load-path))
(require 'clang-format)

(require 'autodisass-llvm-bitcode)

;; Keybindings
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

;; Aliases
(defalias 'rs 'replace-string)
(defalias 'swp 'window-swap-state)

;; Merge mode
(defalias 'merge 'smerge-mode)
(global-unset-key (kbd "C-j"))
(setq smerge-command-prefix (kbd "C-j"))

;; Theme
(load-theme 'jbeans t)


;; Other
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(srcery-theme ujelly-theme inkpot-theme autodisass-llvm-bitcode ample-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
