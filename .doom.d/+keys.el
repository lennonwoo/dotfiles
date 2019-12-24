;;; ~/.doom.d/keys.el -*- lexical-binding: t; -*-

(map!
 :i "C-b" #'backward-char
 :i "C-f" #'forward-char
 :i "C-h" #'delete-backward-char
 :i "C-d" #'delete-forward-char
 :i "C-k" #'kill-sexp
 :i "C-u" #'backward-kill-sexp
 :i "C-n" #'next-line
 :i "C-p" #'previous-line

 :i "C-v" #'evil-paste-after
 :i "C-z" #'undo

 :n "zC" #'evil-close-folds
 :n "zO" #'evil-open-folds

 :in "M-g" #'magit-status
 :in "M-p" #'my/dired-sidebar-show-sidebar
 :in "M-r" #'counsel-recentf
 :in "M-e" #'+eval/buffer
 :in "M-f" #'counsel-grep
 :in "M-l" #'counsel-locate
 :in "C-/" #'comment-line
 :in "C-<tab>" #'ace-window

 :v "C-r" #'evil-quick-replace
 :v "C-c" #'evil-yank


 ;; vscode port
 :n "C-p" #'zilongshanren/open-file-with-projectile-or-counsel-git
 :in "C-s" #'save-buffer

 ;; Window manager
 (:prefix "C-c"
   :in "k" #'evil-delete-buffer
   :in "w" #'+workspace/close-window-or-workspace
   :in "m" #'maximize-window
   :in "j" #'open-junk-file
   )

 ;; Awesome tab related
 :n "C-h" #'awesome-tab-backward-tab
 :n "C-l" #'awesome-tab-forward-tab
 :in "C-j" #'my-awesome-tab-switch-group
 :n "C-." nil
 :n "C-, C-," #'awesome-tab-move-current-tab-to-left
 :n "C-. C-." #'awesome-tab-move-current-tab-to-right
 :n "C-w" nil
 :n "C-w" #'my/kill-window
 :n "C-1" nil
 :n "C-1" #'awesome-tab-select-beg-tab
 :n "C-0" nil
 :n "C-0" #'awesome-tab-select-end-tab
 :n "M-e" #'awesome-tab-select-end-tab
 :n "M-w" #'awesome-tab-kill-other-buffers-in-current-group
 :in "M-1" nil
 :in "M-2" nil
 :in "M-3" nil
 :in "M-4" nil
 :in "M-5" nil
 :in "M-1" 'awesome-tab-select-visible-tab
 :in "M-2" 'awesome-tab-select-visible-tab
 :in "M-3" 'awesome-tab-select-visible-tab
 :in "M-4" 'awesome-tab-select-visible-tab
 :in "M-5" 'awesome-tab-select-visible-tab

 (:leader
   :n "SPC" #'ace-window
   :desc "Last buffer" :n "<tab>" #'evil-switch-to-windows-last-buffer
   (:desc "error" :prefix "e"
     :n "n" #'flycheck-next-error
     :n "p" #'flycheck-previous-error
     )
   (:prefix "f"
     :n "f" #'find-file
     :n "d" #'delete-current-buffer-file
     )
   (:prefix "p"
     :n "e" #'projectile-run-eshell
     :n "f" #'zilongshanren/open-file-with-projectile-or-counsel-git
     :n "s" #'counsel-rg
     )
   (:prefix "w"
     :n "/" #'evil-window-vsplit
     :n "-" #'evil-window-split
     )
   (:prefix "s"
     :n "u" #'lsp-ui-peek-find-references
     )
   (:prefix "l"
     :n "s" #'my/save-layout
     :n "l" #'my/load-layout
     :n "n" #'persp-add-new
     :n "d" #'persp-kill
     )
   )

 ;; lsp-ui mode customize
 (:after lsp-ui
   :map lsp-ui-peek-mode-map
   "C-j" #'lsp-ui-peek--select-next
   "C-k" #'lsp-ui-peek--select-prev
   "C-n" #'lsp-ui-peek--select-next-file
   "C-p" #'lsp-ui-peek--select-prev-file
   )

 (:after ivy
   :map ivy-minibuffer-map
   "<tab>" #'ivy-call-and-recenter
   "C-b"   #'backward-char
   "C-f"   #'forward-char
   "C-h"   #'delete-backward-char
   "C-d"   #'delete-forward-char
   "C-w"   #'backward-kill-word
   )

 (:after company
   (:map company-active-map
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-i"        #'company-complete-selection
     "RET"        nil
     [return]     nil
     "SPC"        nil))

 ;; org mode
 "<f1>" #'org-capture
 "<f2>" #'my/org-agenda-day-list
 "<f3>" #'org-todo-list
 "C-c c" #'org-capture
 (:after org
   (:map org-mode-map
     :in "M-h" #'org-promote-subtree
     :in "M-l" #'org-demote-subtree
     :i "C-h"  #'delete-backward-char
     :i "C-d"  #'delete-forward-char
     (:prefix "C-c"
       :in "a" #'org-archive-subtree
       :in "s" #'org-schedule
       :in "t" (lambda () (interactive) (org-todo "TODO"))
       :in "d" (lambda () (interactive) (org-todo "DONE"))
       :in "p" #'my/org-screenshot
       :in "c" #'my/org-insert-src-block
       :in "i" #'org-toggle-inline-images
       )
     )
   )

 (:after sly
   (:map sly-editing-mode-map
     (:prefix "C-c"
     :in "SPC" #'sly-compile-defun
     :in "d"   #'sly-documentation
     ))
   (:map sly-mrepl-mode-map
     :in "C-j" #'my-awesome-tab-switch-group
     (:prefix "C-c"
     :inv "C-z" #'evil-switch-to-windows-last-buffer
     )))
 )

(defun my/org-agenda-goto ()
  (interactive)
  (org-agenda-goto t)
  (delete-other-windows))

(defun my/org-agenda-todo ()
  (interactive)
  (org-agenda-todo "TODO"))

(defun my/org-agenda-done ()
  (interactive)
  (org-agenda-todo "DONE"))

(evil-define-key 'motion org-agenda-mode-map
  (kbd "<return>") 'my/org-agenda-goto
  (kbd "C-h") 'awesome-tab-backward-tab
  (kbd "C-l") 'awesome-tab-forward-tab
  (kbd "C-w") 'my/kill-window
  (kbd "M-1") 'awesome-tab-select-visible-tab
  (kbd "M-2") 'awesome-tab-select-visible-tab
  (kbd "M-3") 'awesome-tab-select-visible-tab
  (kbd "M-4") 'awesome-tab-select-visible-tab
  (kbd "M-5") 'awesome-tab-select-visible-tab
  "t" 'my/org-agenda-todo
  "d" 'my/org-agenda-done)
