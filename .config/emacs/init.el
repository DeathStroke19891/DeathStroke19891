(setq user-full-name "Sridhar D Kedlaya"
      user-mail-address "sridhardked@gmail.com")

(setq x-meta-keysym 'super
      x-super-keysym 'meta)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(global-hl-line-mode +1)
(column-number-mode t)
(size-indication-mode t)
(setq split-width-threshold 0)
(setq split-height-threshold nil)

(add-to-list 'default-frame-alist '(font . "SpaceMono Nerd Font Mono"))
(set-face-attribute 'default nil :height 105)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(setq inhibit-startup-message t)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(setq make-backup-file nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq scroll-conservatively 100)
(global-prettify-symbols-mode t)


(setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default left-margin-width 1 right-margin-width 1)
(setq default-tab-width 4)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)


(straight-use-package 'use-package)
(straight-use-package 'org)

(use-package which-key
  :straight t
  :init
  (which-key-mode))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-sourcerer t)
  (doom-themes-visual-bell-config))

(use-package magit
  :straight t)

(use-package beacon
  :straight t
  :init
  (beacon-mode 1))

(use-package org-modern
  :straight t
  :init
  (with-eval-after-load 'org (global-org-modern-mode))
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :custom ((projectile-completion-system completing-read-function))
  :config
  (projectile-mode)
  :bind
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))


(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind (:map minibuffer-local-map
	      ("M-d" . backward-kill-word))
  :custom
  (vertico-cycle t))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless)))

;; Example configuration for Consult
(use-package consult
  :after vertico
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :straight t
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :straight t
  :bind
  ("C-;" . avy-goto-char-timer))


(use-package perspective
  :straight t
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x C-b". persp-list-buffers))
  :hook
  (kill-emacs-hook . #'persp-state-save)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-state-default-file "~/.config/emacs/persp")
  :init
  (persp-mode)
  (persp-state-load persp-state-default-file))

(use-package org
  :straight t
  :commands (org-agenda org-capture)
  :hook
  (org-mode . org-indent-mode)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/Documents/org/agenda.org"
                           "~/Documents/org/Birthday.org"))
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Documents/org/agenda.org" "in the flow")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Documents/org/agenda.org" "inbox")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Documents/org/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
  (setq org-link-frame-setup (quote ((vm . vm-visit-folder)
				     (vm-imap . vm-visit-imap-folder)
				     (gnus . gnus)
				     (file . find-file)
				     (wl . wl))))
  :bind
  ("C-c c t" . (lambda () (interactive) (org-capture nil "tt"))))

(defun sk/org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/Documents/orgRoam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("d" "default" plain
				 "%?"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
				 :unnarrowed t)
				("w" "word" plain "* Definition\n1. %?\n\n* Examples\n1. \n\n* Flashcard\n:PROPERTIES:\n:ANKI_DECK: English\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n\n** Front\n\n** Back\n\n"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: word")
				 :unnarrowed t)
				("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project\n\n")
				 :unnarrowed t))
			      )
  :bind
  ( :map mode-specific-map
    :prefix-map org-roam-command-map
    :prefix "r"
    ("t" . org-roam-buffer-toggle)
    ("f" . org-roam-node-find)
    ("g" . org-roam-capture)
    ("i" . org-roam-node-insert)
    ("I" . sk/org-roam-node-insert-immediate)
    ("h" . org-id-get-create)
    :map org-mode-map
    ("C-M-i" . completion-at-point-functions)) ;; Trigger Completion at point

  ;; :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (org-roam-db-autosync-enable)
  :custom
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))



(use-package websocket
  :straight t
  :after org-roam)

(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-return-follows-link t))


(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("@errand" . ?E)
        ("@home" . ?H)
        ("@work" . ?W)
        ("agenda" . ?a)
        ("planning" . ?p)
        ("publish" . ?P)
        ("batch" . ?b)
        ("note" . ?n)
        ("idea" . ?i)
        ("thinking" . ?t)
        ("recurring" . ?r)))


(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "BLOG"
                ((org-agenda-overriding-header "Blog Stuff")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("b" "Blog Stuff"
         ((todo "BLOG"
                ((org-agenda-overriding-header "Blog Stuff")))))

        ("e" "Low effort tasks"
         ((tags-todo "+TODO=\"TODO\"+Effort<15&+Effort>0"
                     ((org-agenda-overriding-header "Low Effort Tasks")
                      (org-agenda-max-todos 20)
                      (org-agenda-files org-agenda-files)))))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANCELLED"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))

(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)
        ("agenda.org" :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-idle-delay 0.1)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-eldoc-render-all t)
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.1))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc--inline-pos 'bottom))

(use-package yasnippet
  :straight t
  :config

  (yas-global-mode t))



(use-package company
  :straight t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  :init
  (global-company-mode))

(use-package lsp-treemacs
  :straight t
  :config
  (setq  treemacs-space-between-root-nodes nil))

(use-package dap-mode
  :straight t
  :config
  (setq dap-auto-configure-mode t)
  (require 'dap-cpptools))

(use-package flycheck
  :straight t)

(use-package yuck-mode
  :straight t)

(use-package golden-ratio
  :straight t
  :init
  (golden-ratio-mode 1)
  :config
  (setq golden-ratio-auto-scale t))

(global-set-key (kbd "C-c = a")
		#'(lambda () (interactive)
		    (find-file "~/Documents/org/agenda.org")))
(global-set-key (kbd "C-c = i")
		#'(lambda () (interactive)
		    (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c = d")
		#'(lambda () (interactive)
		    (find-file "~/.config/doom/config.org")))

(global-set-key (kbd "C-c C-g") 'comment-or-uncomment-region)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package anki-editor
  :straight t)
					;  :hook (org-roam . anki-editor-mode))
(use-package prettier-js
  :straight t
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "120"
                           "--tab-width" "4"
                           "--use-tabs" "true"
                           "--jsx-bracket-same-line" "false"
                           "--stylelint-integration" "true"
                           )))

(use-package smartparens
  :straight t
  :init
  (smartparens-global-mode))

(use-package js2-mode
  :straight t)

(use-package rjsx-mode
  :straight t
  :mode(("\\.js\\'" . rjsx-mode)
        ("\\.jsx\\'" . rjsx-mode))
  :hook
  (rjsx-mode-hook . prettier-js-mode)
  (rjsx-mode-hook . tide-mode))

(use-package tide
  :straight t
  :mode(("\\.ts\\'" . typescript-mode))
  :hook
  (typescript-mode-hook . tide-mode)
  (typescript-mode-hook . prettier-js-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


;; (use-package typescript-mode
;;   :straight t
;;   :hook (typescript-mode . (lambda ()
;; 			     (setq lsp-enabled-clients '(deno-ls))
;; 			     (lsp-deferred))))

;; (add-hook 'js-mode-hook (lambda ()
;; 			  (setq lsp-disabled-clients '())
;; 			  (lsp-deferred)))

