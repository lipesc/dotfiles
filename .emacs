;;; package --- Summary:
;;; Commentary:
;;; Code:
(setq custom-file "~/.emacs.d/emacs.custom")
(load custom-file 'noerror)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq compilation-ask-about-save nil)
(setenv "LSP_USE_PLISTS" "true")

(load-theme 'wombat t)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 10) ("nongnu" . 5) ("melpa" . 1)))


;; Refresh package list only if needed rodar M-x package-quickstart-refresh se instalar/remover pacotes
(setq package-quickstart t)


;; Install selected packages
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (condition-case err
        (package-install pkg)
      (error (message "Failed to install %s: %s" pkg err)))))
;;; compilation corretor de cor
(use-package ansi-color
  :ensure t)
 
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(defun my/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)
(unless (package-installed-p 'lsp-treemacs)


;;; treemacs
(package-install 'lsp-treemacs))
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;;;tree-sitter-langs
    
(require 'treesit)
(treesit-available-p)
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))

  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)

         
;; Garbage Collection Optimization
(use-package gcmh
  :init
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 100 1024 1024)))


;; Melhorias na interface

(menu-bar-mode -1)           ;; Desativar a barra de menu
(tool-bar-mode -1)           ;; Desativar a barra de ferramentas
(scroll-bar-mode -1)         ;; Desativar a barra de rolagem
(global-display-line-numbers-mode 1)

;; Desativar a tela inicial e a mensagem do scratch


(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;;; smex melhorado M x
(use-package smex
  :ensure t
  :config
  ;; suas configurações para smex, se houver
  )


;;; atalhos

(setq-default abbrev-mode t)
(define-abbrev global-abbrev-table "con" "console.log();")
(define-abbrev global-abbrev-table "log" "console.log();")



(use-package ido-completing-read+
  :ensure t)

(ido-mode 1)
(ido-everywhere -1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



;; Configuração do clipboard

(setq select-enable-clipboard t
      select-enable-primary t
      mouse-drag-copy-region t)

;; keys binds
(use-package multiple-cursors
  :ensure t)
           
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-j") 'er/expand-region)
(global-set-key (kbd "C-c C-<") 'mc/edit-lines)

 
(defun occur-selection-editable ()
  "Show occurrences of the selected text in a new buffer and enable editing mode."
  (interactive)
  (when (region-active-p)
    (let (deactivate-mark)
      (occur (regexp-quote (buffer-substring (region-beginning) (region-end))))
      (with-current-buffer "*Occur*"
        (occur-edit-mode)))))


;; Bind the function to a key (M-o)
(global-set-key (kbd "M-o") 'occur-selection-editable)

(define-prefix-command 'my-prefix-map)
(global-set-key (kbd "C-c C-c") 'my-prefix-map)


;; Associa M-x ao comando 'execute-extended-command'
(define-key my-prefix-map (kbd "M-x") 'execute-extended-command)


;; Atalhos para copiar, cortar e colar

;;(global-set-key (kbd "C-c c") 'clipboard-kill-ring-save)  ;; Copiar
;;(global-set-key (kbd "C-v") 'clipboard-yank)            ;; Colar


;; Resolver conflito do C-x C-z
;;(define-key key-translation-map (kbd "C-z") nil) ;; Garante que seja traduzido corretamente
;;(global-unset-key (kbd "C-z")) ;; Desativa completamente o atalho


;; Don't ask before killing the current compilation. This is useful if
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output t
      compile-command " ")

;; Taken from
;; https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))


;;;  Pacotes principais
;; Give a pulse light when switching windows, or switching focus to
;; the minibuffer.
(use-package pulse
  :ensure t)

(set-face-attribute 'pulse-highlight-start-face nil :background "#49505f")
(add-hook 'window-selection-change-functions
          (lambda (frame)
            (when (eq frame (selected-frame))
              (pulse-momentary-highlight-one-line))))


;; Enable scala-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")


(substitute-key-definition
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)


;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

  
  
;;; autoPair

;;  chords,smart-shift-left
(require 'key-chord)
      (key-chord-mode 1)

(key-chord-define-global "<<" 'smart-shift-left)


(require 'autopair)
(autopair-global-mode)


;;; TABS
;; largura do tab
(setq-default tab-width 4)

;; não use espaços, use caractere de tab (se quiser)
(setq-default indent-tabs-mode t)

;; desliga indentação automática ao apertar RET
;;(when (boundp 'electric-indent-mode)
;;  (electric-indent-mode 1))

;; TAB só indenta quando você pedir
;;(global-set-key (kbd "TAB") #'indent-for-tab-command)


;; Whitespace Mode Tweaks
;;; Whitespace mode ffffxxxxuu feeeeeuuuuu

(defun set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(remove-hook 'simpc-mode-hook 'set-up-whitespace-handling)
(add-hook 'simpc-mode-hook
          (lambda ()
          (setq show-trailing-whitespace nil)))
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab))


;;; simpc
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))


(add-hook 'simpc-mode-hook
          (lambda ()
            (interactive)
            (setq-local fill-paragraph-function 'astyle-buffer)))


;;; magit

(use-package magit
  :ensure t)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; emacs-lsp-booster
(defun lsp-booster--advice (orig-fn &rest args)
  "Advice to boost lsp-mode performance."
  (let* ((server-id (or (car (plist-get (plist-get args :plist) :server-id)
                        (car args))))
         (proc (apply orig-fn args)))
    (when (and (not (file-remote-p default-directory))
               (memq server-id '(pylsp rust-analyzer))) ; Adicione outros servidores aqui
      (let ((command (process-command proc)))
        (set-process-command
         proc (append (list "emacs-lsp-booster" "--") command))))
    proc))

(advice-add 'lsp--make-process :around #'lsp-booster--advice)
;; Increase the amount of data which Emacs reads from the process#
;;(setq gc-cons-threshold (* 100 1024 1024))
;;(setq read-process-output-max (* 1024 1024));; 1mb
;;(setq lsp-log-io nil) ; if set to true can cause a performance hit
;;(setq lsp-idle-delay 0.500)
;;lsp config
;; lsp-mode 
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((prog-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  ;; se você usa flycheck/flymake, remova essa linha
  (lsp-diagnostics-provider :none))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook #'lsp))

;; 1. Configura o auto-virtualenv para gerenciar os ambientes
(use-package auto-virtualenv
  :ensure t
  :config
  ;; Habilita o recarregamento automático do LSP quando o venv muda (essencial!)
  (setq auto-virtualenv-reload-lsp t)
  ;; Ativa o pacote
  (auto-virtualenv-setup))

;; 2. Configura o lsp-pyright para iniciar no modo Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . lsp-deferred))

;;java dap
(require 'dap-java)

;; dap node
(require 'dap-node)

;; dap python
(require 'dap-python)

;;; company exibir sugestões textos já digitados
(use-package company
  :ensure t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("TAB"   . company-complete-selection))
  :custom
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t

        ;; NÃO converter sugestões para minúsculo
        company-dabbrev-downcase nil

        ;; respeitar case original
        company-dabbrev-ignore-case nil)

  

  (setq company-backends
        '((company-capf company-dabbrev company-files company-elisp
                                 company-cmake
                                 company-yasnippet
                                 company-keywords
                                 company-etags
                                 company-gtags)))

  (global-company-mode 1))




;;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package which-key
  :config (which-key-mode))



(use-package web-mode
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js\\'"))


;;; .emacs ends here
