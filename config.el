
(setq user-full-name "Štěpán Křivanec"
      user-mail-address "stepan.krivanec@gmail.com")

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(set-charset-priority 'unicode)

(setq org-catch-invisible-edits 'error) ;now I can't edit invisible text. C-c C-r (org-reveal) will display where the point is if it is buried in invisible text to allow editing again.

(setq org-use-speed-commands t)

; UTF-8 as the default coding systerm for all org files:
(setq org-export-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (calc . t)
   (latex . t)
   (java . t)
   (ruby . t)
   (scheme . t)
   (sh . t)
   (sqlite . t)
   (js . t)))

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(setq org-startup-indented t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
              helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
              helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
              helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
              helm-ff-file-name-history-use-recentf t)
          (helm-mode))
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-`" . helm-resume)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line))

(use-package guide-key
  :ensure t
  :init
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-c h"))
  (guide-key-mode 1))

(blink-cursor-mode -1)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq default-input-method "czech-qwerty")

(use-package cyberpunk-theme
  :ensure t
  :init
  (progn
    (load-theme 'cyberpunk t)
    (set-face-attribute `mode-line nil
                        :box nil)
    (set-face-attribute `mode-line-inactive nil
                        :box nil)))

(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  :ensure t)

(setq monokai-use-variable-pitch nil)

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "M-<f12>" 'switch-theme)
(bind-key "M-<f11>" 'disable-active-themes)

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(use-package smooth-scrolling
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?j ?k ?l ?u ?i ?o ?p))
  (ace-window-display-mode)
  :bind ("s-i" . ace-window))
