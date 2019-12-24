;;; ~/.doom.d/ui.el -*- lexical-binding: t; -*-

(require 'awesome-tab)
(require 'dired-sidebar)


(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*ana" name)
     (string-prefix-p "*Ido" name)
     (string-prefix-p "*doom" name)
     (string-prefix-p "*scratch" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*sly" name)
     (and (string-prefix-p "magit" name)
               (not (file-name-extension name)))
     )))

(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab wit h mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-prefix-p "*sly-mrepl" (buffer-name))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'dired-mode)
     "Dired")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

(awesome-tab-mode 1)
(column-number-mode)
(setq-default fill-column 120)
