(setq user-full-name "Sridhar D Kedlaya"
      user-mail-address "sridhar@sridharkedlaya.xyz")

(setq  x-meta-keysym 'super
       x-super-keysym 'meta)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

(setq doom-theme 'doom-sourcerer)

(defvar fancy-splash-image-directory
  (expand-file-name "~/.config/doom/misc/splash-images/" doom-private-dir)
  "Directory in which to look for splash image templates.")

(defvar fancy-splash-image-template
  (expand-file-name "emacs-e-template.svg" fancy-splash-image-directory)
  "Default template svg used for the splash image.
Colours are substituted as per `fancy-splash-template-colours'.")

(defvar fancy-splash-template-colours
  '(("#111112" :face default   :attr :foreground)
    ("#8b8c8d" :face shadow)
    ("#eeeeef" :face default   :attr :background)
    ("#e66100" :face highlight :attr :background)
    ("#1c71d8" :face font-lock-keyword-face)
    ("#f5c211" :face font-lock-type-face)
    ("#813d9c" :face font-lock-constant-face)
    ("#865e3c" :face font-lock-function-name-face)
    ("#2ec27e" :face font-lock-string-face)
    ("#c01c28" :face error)
    ("#000001" :face ansi-color-black)
    ("#ff0000" :face ansi-color-red)
    ("#ff00ff" :face ansi-color-magenta)
    ("#00ff00" :face ansi-color-green)
    ("#ffff00" :face ansi-color-yellow)
    ("#0000ff" :face ansi-color-blue)
    ("#00ffff" :face ansi-color-cyan)
    ("#fffffe" :face ansi-color-white))
   "Alist of colour-replacement plists.
Each plist is of the form (\"$placeholder\" :doom-color 'key :face 'face).
If the current theme is a doom theme :doom-color will be used,
otherwise the colour will be face foreground.")

(setq doom-font (font-spec :family "SpaceMono Nerd Font Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "SpaceMono Nerd Font" :size 13)
      doom-big-font (font-spec :family "SpaceMono Nerd Font Mono" :size 24))
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

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(setq peep-dired-cleanup-on-disable t)

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-open-extensions '(("gif" . "viewnior")
                              ("jpg" . "viewnior")
                              ("png" . "viewnior")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(add-hook 'find-file-hook 'recentf-save-list)
(add-hook 'find-file-hook 'flyspell-mode)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file"      "a" #'(lambda () (interactive) (find-file "~/Documents/org/agenda.org"))
       :desc "Edit doom config.org"  "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el"     "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))

(use-package! vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package! transpose-frame)

(after! org
  (setq org-ellipsis " ▼ ")
  (setq org-superstar-headline-bullets-list '("◉  " "●  " "○  " "◆  " "●  " "○  " "◆  "))
  (setq org-superstar-itembullet-alist '((?+ . ?✦) (?- . ?➤)))
  (setq org-log-done 'time)
  (setq org-hide-emphasis-markers t)
  (setq org-link-abbrev-alist
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/")))
  (setq org-table-convert-region-max-lines 20000)
  (setq org-todo-keywords
          '((sequence
             "TODO(t)"
             "Note(n)"
             "BLOG(b)"
             "|"
             "DONE(d)")
            (sequence
             "BACKLOG(b)"
             "PLAN(p)"
             "READY(r)"
             "ACITVE(a)"
             "REVIEW(v)"
             "WAIT(w@/!)"
             "HOLD(h)"
             "|"
             "COMPLETED(c)"
             "CANCELLED(k@)"
             )))) ; Task has been cancelled

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

(use-package! org
  :commands (org-agenda)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/Documents/org/agenda.org"
                           "~/Documents/org/Birthday.org")))

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

(use-package! org
  :commands (org-capture)
  :config
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Documents/org/agenda.org" "in the flow")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Documents/org/agenda.org" "inbox")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Documents/org/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

  (define-key global-map (kbd "C-c t")
    (lambda () (interactive) (org-capture nil "tt")))

(defun sk/org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

(use-package! org-roam
  :custom
  (org-roam-directory "~/Documents/orgRoam")
  (setq org-roam-completion-everywhere t)
  :bind ( :map org-mode-map (
                             "C-M-i" . completion-at-point ;; Trigger Completion at point
                             ))
  ;:hook (org-roam-mode . org-roam-ui-mode)
  :config
  (org-roam-db-autosync-enable)
  :custom
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (sk/org-roam-node-insert-immediate))


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
       :desc "Org roam node insert immediate"  "I" #'sk/org-roam-node-insert-immediate
       :desc "Org roam heading id create"  "h" #'org-id-get-create))

(use-package! org-roam
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("w" "word" plain
      "* Definition\n%?\n* Example\n1. "
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+tags: word\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n%?\n* Tasks\n** TODO Add initial tasks\n\n*Index\n\n* Dates\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: project")
      :unnarrowed t))))

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

(setq inferior-lisp-program "sbcl")

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

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

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
