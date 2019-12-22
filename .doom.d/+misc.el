;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-


(setq-default persp-save-dir "~/.doom.d/")
(setq-default frame-title-format
              '(:eval
                (format "%s %s%s"
                        "Doom"
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]"))
                        (if (buffer-modified-p)
                            "modified"
                          "")
                        )))
(setq racket-smart-open-bracket-enable nil)
