;;; ~/.doom.d/funcs.el -*- lexical-binding: t; -*-

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun kill-ranger-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'ranger-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun my/load-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "lennonwoo")))

(defun my/save-layout ()
  (interactive)
  (progn
    (kill-ranger-buffers)
    (persp-save-state-to-file (concat persp-save-dir "lennonwoo")
                              )))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-used-p 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun evil-quick-replace (begin end)
  (interactive "r")
  (let ((selection (regexp-quote (buffer-substring-no-properties begin end))))
    (setq command-string (format "%%s /%s//g" selection))
    (minibuffer-with-setup-hook
        (lambda () (backward-char 2))
      (evil-ex command-string))))

(defun zilongshanren/git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

(defun zilongshanren/open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (zilongshanren/git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))

(defun my/awesome-next-tab (n)
  (if (= n 1)
      (message "DONE")
      (awesome-tab-forward-tab)
      (my/awesome-next-tab (- n 1))))

(defun my/choose-tab (n)
  (progn
    (awesome-tab-select-beg-tab)
    (my/awesome-next-tab n)))

(defun my/awesome-tab-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (let* ((current-group-name (cdr (awesome-tab-selected-tab (awesome-tab-current-tabset t))))
         (currentbuffer (current-buffer)))
    ;; Kill all buffers in current group.
    (awesome-tab-kill-buffer-match-rule
     (lambda (buffer) (equal buffer currentbuffer)))
    ))

(defun my/kill-window ()
  "Kill current buffer."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (doom-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (+workspace-current-name))
                     (delete-frame)
                   (+workspace/delete current-persp-name))))

              (t (my/awesome-tab-kill-current-buffer)))))))

(defun my/dired-sidebar-show-sidebar ()
  (interactive)
  (if (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-hide-sidebar)
    (dired-sidebar-show-sidebar)))

;; dirty hack
(defun persp--kill-buffer-query-function-foreign-check (persp buf)
  'kill)


(defun add-agenda-map-hook (key func)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (local-set-key (kbd key) func))))

(defun my-awesome-tab-switch-group ()
  "Switch tab groups using ido."
  (interactive)
  (let* ((tab-buffer-list (mapcar
                           #'(lambda (b)
                               (with-current-buffer b
                                 (list (current-buffer)
                                       (buffer-name)
                                       (funcall awesome-tab-buffer-groups-function))))
                           (funcall awesome-tab-buffer-list-function)))
         (groups (my-awesome-tab-get-other-groups))
         (group-name (ido-completing-read "Groups: " groups)))
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (car (car (cdr (cdr group)))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list))))

(defun my-awesome-tab-get-other-groups ()
  (interactive)
  (let* ((all-groups (awesome-tab-get-groups))
         (current-group (funcall awesome-tab-buffer-groups-function)))
    (delete (car current-group) all-groups)))

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (abbreviate-file-name (if (equal major-mode 'dired-mode)
                                            default-directory
                                          (buffer-file-name)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
