;;; ~/.doom.d/org.el -*- lexical-binding: t; -*-

(setq org-agenda-files '("~/Dropbox/org/gtd.org")
      org-default-notes-file "~/Dropbox/org/notes.org"
      org-agenda-todo-list-sublevels nil
      org-tags-match-list-sublevels nil
      ;; next three line for speeding up agenda view
      org-agenda-dim-blocked-tasks nil
      org-agenda-inhibit-startup nil
      org-agenda-use-tag-inheritance nil)

(setq org-capture-templates
      '(("a" "Arrange today's work" entry (file "~/Dropbox/org/gtd.org")
         "* TODO %?\nSCHEDULED:%t\nEntered on%U")
        ("m" "Monthly job" entry (file+headline "~/Dropbox/org/gtd.org" "Monthly")
         "* TODO %?\nEntered on%U")
        ("f" "Future's work" entry (file+headline "~/Dropbox/org/gtd.org" "Future")
         "* TODO %?\nEntered on%U")
        ("c" "Containers" entry (file+headline "~/Dropbox/org/gtd.org" "Containers")
         "* %?\nEntered on%U")
        ("C" "Containers with link" entry (file+headline "~/Dropbox/org/gtd.org" "Containers")
         "* %?\nEntered on%U\n %a")
        ("j" "journey" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on%U")
        ("J" "journey with link" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on%U\n %a")))

(defun my/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh""js""C++"
            "lisp" "org""ruby" "scheme" "sml")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((current-buffer-name (replace-regexp-in-string
                               "\\." "_" (file-name-nondirectory buffer-file-name)))
         (store-filename (concat current-buffer-name
                                 (format-time-string "_%Y%m%d_%H%M%S")
                                 ".png"))
         (assets-filepath (concat (file-name-nondirectory "")
                                  "assets/"
                                  store-filename)))
    (progn
      (unless (file-exists-p (file-name-directory assets-filepath))
        (make-directory (file-name-directory assets-filepath)))
      ;; 若为Linux,则进行截图
      (if (eq system-type 'gnu/linux)
          (call-process "maim" nil nil nil "-s" "-k" assets-filepath))
      (if (file-exists-p assets-filepath)
          (insert (concat "[[file:" assets-filepath "]]")))
      (org-display-inline-images))))

(defun my/org-agenda-day-list ()
  "show my org agenda as current day list"
  (interactive)
  (save-excursion
    (progn (org-agenda-list)
           (org-agenda-day-view)
           (my/kill-window)
           (while (not (string-equal (buffer-name) "*Org Agenda*"))
             (awesome-tab-forward-tab))
           )))
