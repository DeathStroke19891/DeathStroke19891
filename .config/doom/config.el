(setq user-full-name "Sridhar D Kedlaya"
      user-mail-address "sridhar@sridharkedlaya.xyz")

(setq  x-meta-keysym 'super
       x-super-keysym 'meta)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

(setq doom-theme 'doom-monokai-machine)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "TerminessTTF Nerd Font" :size 14)
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

(after! org
  (setq org-ellipsis " ▼ ")
  (setq org-superstar-headline-bullets-list '("◉  " "●  " "○  " "◆  " "●  " "○  " "◆  "))
  (setq org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))) ; changes +/- symbols in item lists
  (setq org-log-done 'time)
  (setq org-hide-emphasis-markers t)
  (setq org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/")))
  (setq org-table-convert-region-max-lines 20000)
  (setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO"           ; A task that is ready to be tackled
             "BLOG"           ; Blog writing assignments
             "GYM"            ; Things to accomplish at the gym
             "PROJ"           ; A project that contains other tasks
             "VIDEO"          ; Video assignments
             "WAIT"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE"           ; Task has been completed
             "CANCELLED" )))) ; Task has been cancelled

(defun sk/org-font-setup ()
(with-eval-after-load 'org-faces
   (dolist (face
            '((org-level-1 1.7 "#78dce8" ultra-bold)
              (org-level-2 1.6 "#ab9df2" extra-bold)
              (org-level-3 1.5 "#a9dc76" bold)
              (org-level-4 1.4 "#fc9867" semi-bold)
              (org-level-5 1.3 "#ff6188" normal)
              (org-level-6 1.2 "#ffd866" normal)
              (org-level-7 1.1 "#78dce8" normal)
              (org-level-8 1.0 "#ab9df2" normal)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
  (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf")))
(sk/org-font-setup)

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
       :desc "Show graph"  "g" #'org-roam-mode-ui
       :desc "Capture to node"  "g" #'org-roam-capture
       :desc "Org roam node insert"  "i" #'org-roam-node-insert
       :desc "Org roam heading id create"  "h" #'org-id-get-create))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun sk/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'sk/insert-auto-tangle-tag)

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

(use-package! rjsx-mode
  :mode "\\.jsx\\'"
  :hook (rjsx-mode . lsp)
  :custom
  (create-lockfiles nil))

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
