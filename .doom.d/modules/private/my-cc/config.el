;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(defvar ccls-path-mappings [])

(defvar ccls-initial-blacklist [])

(defun my/ccls/enable ()
  (require 'ccls)
  (setq-local lsp-ui-sideline-show-symbol nil)
  (condition-case nil
      (progn
        (lsp-ccls-enable)
        (lsp-ui-mode))
    (user-error nil)))

(defun ccls/callee ()
  (interactive)
  (lsp-ui-peek-find-custom 'callee "$ccls/call" '(:callee t)))
(defun ccls/caller ()
  (interactive)
  (lsp-ui-peek-find-custom 'caller "$ccls/call"))
(defun ccls/vars (kind)
  (lsp-ui-peek-find-custom 'vars "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels)
  (lsp-ui-peek-find-custom 'base "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels)
  (lsp-ui-peek-find-custom 'derived "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind)
  (lsp-ui-peek-find-custom 'member "$ccls/member" `(:kind ,kind)))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 64))))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:excludeRole 32))))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 8))))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 16))))

;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
     (while (string-match pattern line i)
       (setq i (match-end 0))
       (add-face-text-property (match-beginning 0) (match-end 0) 'isearch t line)
       )
     line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; This deviated from the original in that it highlights pattern appeared in symbol
  (defun lsp--symbol-information-to-xref (pattern symbol)
   "Return a `xref-item' from SYMBOL information."
   (let* ((location (gethash "location" symbol))
          (uri (gethash "uri" location))
          (range (gethash "range" location))
          (start (gethash "start" range))
          (name (gethash "name" symbol)))
     (xref-make (format "[%s] %s"
                        (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                        (my/highlight-pattern-in-text (regexp-quote pattern) name))
                (xref-make-file-location (string-remove-prefix "file://" uri)
                                         (1+ (gethash "line" start))
                                         (gethash "character" start)))))

  (cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
    (let ((symbols (lsp--send-request (lsp--make-request
                                       "workspace/symbol"
                                       `(:query ,pattern)))))
      (mapcar (lambda (x) (lsp--symbol-information-to-xref pattern x)) symbols)))
  )

(after! cc-mode
  ;; https://github.com/radare/radare2
  (c-add-style
   "radare2"
   '((c-basic-offset . 4)
     (indent-tabs-mode . t)
     (c-auto-align-backslashes . nil)
     (c-offsets-alist
      (arglist-intro . ++)
      (arglist-cont . ++)
      (arglist-cont-nonempty . ++)
      (statement-cont . ++)
      )))
  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setq c-default-style "my-cc")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")
              ))

  (add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

  (map!
   :map (c-mode-map c++-mode-map)
   :n "gh" (λ! (ccls-navigate "U"))
   :n "gj" (λ! (ccls-navigate "R"))
   :n "gk" (λ! (ccls-navigate "L"))
   :n "gl" (λ! (ccls-navigate "D"))
   (:leader
     :n "=" #'clang-format-region
     )
   (:localleader
     :n "a" #'ccls/references-address
     :n "c" #'ccls/callers
     :n "f" #'ccls/references-not-call
     :n "lp" #'ccls-preprocess-file
     :n "lf" #'ccls-reload
     :n "m" #'ccls/references-macro
     :n "r" #'ccls/references-read
     :n "w" #'ccls/references-write
     :desc "breakpoint"
     :n "db" (lambda ()
               (interactive)
               (evil-open-above 1)
               (insert "volatile static int z=0;while(!z)asm(\"pause\");")
               (evil-normal-state))))
  )

(def-package! clang-format
  :commands (clang-format-region)
  )

(def-package! ccls
  :load-path "/usr/bin/ccls"
  :defer t
  :init (add-hook! (c-mode c++-mode objc-mode) #'my/ccls/enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq
   ccls-extra-init-params
   `(:clang (:pathMappings ,+ccls-path-mappings)
            :completion
            (:includeBlacklist
             ("^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
              "^/usr/(local/)?include/c\\+\\+/v1/"
              ))
            :index (:initialBlacklist ,+ccls-initial-blacklist :trackDependency 1)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set-company-backend! '(c-mode c++-mode objc-mode) 'company-lsp)
  )
