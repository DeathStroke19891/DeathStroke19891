(setq user-full-name "Sridhar D Kedlaya"
      user-mail-address "sridhar@sridharkedlaya.xyz")

(setq  x-meta-keysym 'super
       x-super-keysym 'meta)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

(setq doom-theme 'doom-monokai-machine)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font Mono" :size 14)
      doom-big-font (font-spec :family "Iosevka Nerd Font Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type 'relative)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines))

(add-hook 'find-file-hook 'recentf-save-list)
(add-hook 'find-file-hook 'flyspell-mode)

(use-package! transpose-frame)



(use-package! org-roam
  :custom
  (org-roam-directory "~/Documents/orgRoam")
  (setq org-roam-completion-everywhere t)
  :bind ( :map org-mode-map (
                             "C-M-i" . completion-at-point ;; Trigger Completion at point
                             ))
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (org-roam-db-autosync-enable))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(map! :leader
      (:prefix-map ("r" . "roam")
       :desc "Org roam Buffer toggle"  "t" #'org-roam-buffer-toggle
       :desc "Org roam node find"  "f" #'org-roam-node-find
       :desc "Org roam node insert"  "i" #'org-roam-node-insert
       :desc "Org roam heading id create"  "h" #'org-id-get-create))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(map! :leader
      (:prefix ("e". "evaluate")
       :desc "Evaluate elisp in buffer"  "b" #'eval-buffer
       :desc "Evaluate defun"            "d" #'eval-defun
       :desc "Evaluate elisp expression" "e" #'eval-expression
       :desc "Evaluate last sexpression" "l" #'eval-last-sexp
       :desc "Evaluate elisp in region"  "r" #'eval-region))

(use-package! lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-eldoc-render-all t))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc--inline-pos 'bottom))

(use-package! typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'lsp)
  :custom
  (create-lockfiles nil))

(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'sk/rustic-mode-hook))

(defun sk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package! company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

(after! yasnippet
  (use-package! yasnippet
    :config
    (setq yas-snippet-dirs '("~/Documents/snippets"))
    (yas-global-mode t)))
