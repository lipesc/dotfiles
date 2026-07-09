; init.el --- Emacs Configuration arch 
;; 1. CONFIGURAÇÃO DO GERENCIADOR DE PACOTES
;; -------------------------------------------------------------------------
;; 1. Garante que o gerenciador de pacotes 'use-package' esteja instalado
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; 2. Instala as dependências de ícones (Nerd Icons)
(use-package nerd-icons)

;; 3. Configura o Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  ;; Ajuste a altura se achar muito grande ou pequeno (padrão é 25)
  (setq doom-modeline-height 25)
  ;; Mostra ícones coloridos (t para sim, nil para não)
  (setq doom-modeline-icon t)
  (setq doom-modeline-minor-modes t)
  ;; Estilo da barra (opções: 'default, 'minimal, 'arrows)
  (setq doom-modeline-style 'default))
(setq doom-modeline-buffer-encoding t)      ; Exibe codificação (UTF-8)
(setq doom-modeline-vcs-max-length 12)     ; Limita tamanho do nome da branch Git
(setq doom-modeline-env-version t)         ; Exibe versão do Python/Node/Rust
(setq doom-modeline-buffer-state-icon t)   ; Ícone indicando se o arquivo foi editado

;; -------------------------------------------------------------------------
;; 2. DEFINIÇÕES GERAIS E UI
;; -------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/emacs.custom")
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/")

;; Interface Limpa
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

;; --- FONTE ---
;; O valor 120 equivale a tamanho 12pt. Ajuste se ficar pequeno/grande.
(set-face-attribute 'default nil :family "Monospace" :height 130)

;; Tema 
(load-theme 'minimal-black t)

;; Comportamento
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq select-enable-clipboard t)
(setq make-backup-files nil)

;; Histórico e sessão entre reinicializações
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

;; markdown
(with-eval-after-load 'markdown-mode
  (defun my/markdown-preview-in-eww (orig-fn &rest args)
    (let ((browse-url-browser-function #'eww-browse-url))
      (apply orig-fn args)))
  (advice-add 'markdown-preview :around #'my/markdown-preview-in-eww))

;; Whitespace
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab))


(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;;modeline

(add-to-list 'auto-mode-alist '("\\.mms\\'" . asm-mode))
;; -------------------------------------------------------------------------
;; 3. PACOTES GERAIS
;; -------------------------------------------------------------------------
;;icons-fonts
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(use-package all-the-icons-nerd-fonts
  :config
  (all-the-icons-nerd-fonts-prefer))
;; git

(defun my-vc-git-amend ()
  (interactive)
  (vc-checkin nil 'git)
  (vc-git-log-edit-toggle-amend))

;;xref-js2
(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "M-.") nil))



(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(setq xref-js2-search-program 'rg)


(use-package gcmh
  :init (gcmh-mode 1)
  :custom (gcmh-idle-delay 5) (gcmh-high-cons-threshold (* 100 1024 1024)))

(setq read-process-output-max (* 2048 2048))

(use-package ido-completing-read+
  :config (ido-mode 1) (ido-everywhere 1) (ido-ubiquitous-mode 1))

(use-package smex
  :config (global-set-key (kbd "M-x") 'smex))

(use-package which-key
  :config (which-key-mode))

(unless (package-installed-p 'marginalia)
  (package-refresh-contents)
  (condition-case err
      (package-install 'marginalia)
    (error
     (message "Falha ao instalar marginalia: %s" (error-message-string err)))))

(use-package marginalia
  :ensure nil
  :init
  (when (fboundp 'marginalia-mode)
    (marginalia-mode 1)))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (let ((renderer (or (executable-find "pandoc")
                      (executable-find "cmark")
                      (executable-find "multimarkdown")
                      (executable-find "markdown"))))
    (if renderer
        (setq markdown-command renderer)
      (message "Markdown preview precisa de um renderizador externo (ex.: pandoc)."))))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-<" . mc/edit-lines)
         ("C-j" . er/expand-region)))
(use-package expand-region)

(use-package pulse
  :ensure nil
  :config
  (set-face-attribute 'pulse-highlight-start-face nil :background "#49505f")
  (add-hook 'window-selection-change-functions
            (lambda (frame)
              (when (eq frame (selected-frame))
                (pulse-momentary-highlight-one-line)))))

(use-package ansi-color
  :hook (compilation-filter . my/colorize-compilation-buffer)
  :config
  (defun my/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output t)

(electric-pair-mode 1)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "<<" 'smart-shift-left))

;;modeline
;; ===============================
;; EGLOT (LSP NATIVO)
;; ===============================

(use-package eglot
  :ensure nil ;; já vem com Emacs 29+
  :hook
  ((js-ts-mode
    typescript-ts-mode
    python-ts-mode
    java-mode
    java-ts-mode
    rust-ts-mode
    zig-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-extend-to-xref t)

  ;; Performance
  (read-process-output-max (* 1024 1024))
  (json-parse-string-buffer-size 100000)
  :config
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls"))))
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format))
(let ((path-env (getenv "PATH")))
  (when path-env
    (dolist (dir (parse-colon-path path-env))
      (add-to-list 'exec-path dir))))

;; ===============================
;; MODELINE MODERNA (MOOD-LINE)
;; ===============================

;; -------------------------------------------------------------------------
;; 4. DESENVOLVIMENTO & TREESITTER
;; -------------------------------------------------------------------------;;
;; eglot--managed-mode

;;etecta se você tem suporte e ativa os modos modernos (-ts-mode)
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt) ;; Pergunta se precisa baixar gramática nova
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package geiser
  :ensure t
  :config
  ;; Use this instead of the old geiser-scheme-implementations
  (setq geiser-active-implementations '(mit)))

(use-package geiser-mit
  :ensure t)


(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-show-numbers t))

;;(use-package flycheck
;;  :init (global-flycheck-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :config (yas-global-mode 1))

;; Mantive web-mode para arquivos mistos complexos, mas o Treesitter cuidará da maioria
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css?\\'" "\\.js\\'"))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; -------------------------------------------------------------------------
;; 5. PACOTES LOCAIS (~/.emacs.d/lisp/)
;; -------------------------------------------------------------------------

(require 'smart-shift nil t)
(when (featurep 'smart-shift)
  (global-smart-shift-mode 1))

(require 'simpc-mode nil t)
(when (featurep 'simpc-mode)
  ;; Remove a regra antiga que forçava simpc-mode em C/C++ se quiser usar Treesitter (c-ts-mode)
  ;; Se preferir seu simpc-mode, mantenha as linhas abaixo descomentadas:
  (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
  
  (add-hook 'simpc-mode-hook
            (lambda ()
              (interactive)
              (setq show-trailing-whitespace nil))))

(provide 'init)
;;; init.el ends here
