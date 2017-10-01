;; Put the following line in your ~/.emacs file
;;(load-file (expand-file-name "~/work/bzr_reps/eb/emacs_init.el")) <-- insert your path

;;****************************************************************************
;; Init slime
;;****************************************************************************
;; Use "M--M-x slime" to select implementation
(setq slime-lisp-implementations
      '((ccl ("/home/evgeny/bin/closure-cl"))
        (ecl ("/usr/local/bin/ecl"))))
(setq inferior-lisp-program "/usr/local/bin/ecl") ; <-- insert your path
(add-to-list 'load-path (expand-file-name "~/emacs/slime")) ; <-- insert your path
(add-to-list 'load-path (expand-file-name "~/emacs/slime/contrib")) ; <-- insert your path
(require 'slime)
(slime-setup '(slime-fancy))

(setq slime-net-coding-system 'utf-8-unix)
;; (setq common-lisp-hyperspec-root (expand-file-name
;;                                   "/usr/share/doc/hyperspec/")) ; <-- insert your path

;;****************************************************************************
;; Enable case convertions
;;****************************************************************************
(put 'downcase-region 'disabled 0)
(put 'upcase-region 'disabled 0)

;;****************************************************************************
;; Allow extra space at the end of the line
;;****************************************************************************
(setq-default fill-column 80)

;;****************************************************************************
;; Remove trailing white spaces and replace all tabs with spaces when saving.
;; WARNING: Can cause problems with makefiles
;;****************************************************************************
(add-hook 'before-save-hook
          '(lambda ()
             (delete-trailing-whitespace)
             (if (not (or (eq major-mode 'makefile-mode)
                          (eq major-mode 'makefile-gmake-mode)))
                 (untabify (point-min) (point-max)))))

;;****************************************************************************
;; Make lines wrap automatically in text mode.
;;****************************************************************************
(add-hook 'text-mode-hook
          '(lambda () (auto-fill-mode 1)))
(add-hook 'lisp-mode-hook
          '(lambda () (auto-fill-mode 1)))
(add-hook 'c-mode-hook
          '(lambda () (c++-mode) (auto-fill-mode 1)))
(add-hook 'c++-mode-hook
          '(lambda () (auto-fill-mode 1)))
(add-hook 'erlang-shell-mode-hook
          '(lambda () (font-lock-mode 1)))

;;****************************************************************************
;; Setup tab
;;****************************************************************************
(setq-default indent-tabs-mode 0)       ; never use tab character
(setq default-tab-width 2)              ; set tab width

;;****************************************************************************
;; Highlight Current Line
;;****************************************************************************
;; (global-hl-line-mode 1)
(global-hl-line-mode 0)

;;****************************************************************************
;; Show line-number in the mode line
;;****************************************************************************
(line-number-mode 1)

;;****************************************************************************
;; Show column-number in the mode line
;;****************************************************************************
(column-number-mode 1)

;;****************************************************************************
;; Collapse multiple spaces into single one.
;;****************************************************************************
(defun my-collapse-whitespace ()
  "Reduce all whitespace surrounding point to a single space."
  ;; @@ This seems to be quite buggy at the moment
  (interactive)
  (kill-region (progn (re-search-backward "[^ \t\r\n]")
                      (forward-char)
                      (point))
               (progn (re-search-forward "[^ \t\r\n]")
                      (backward-char)
                      (point)))
  (insert-char ?\  1))
(global-set-key "\C-cw" 'my-collapse-whitespace)

;;****************************************************************************
;; kill whole line
;;****************************************************************************
(defun my-kill-whole-line ()
  "Kill an entire line, including trailing newline"
  (interactive)
  (beginning-of-line)
  (kill-line 1))
(global-set-key "\C-ck" 'my-kill-whole-line)

;;****************************************************************************
;; goto line function C-c C-g
;;****************************************************************************
(global-set-key [ (control c) (control g) ] 'goto-line)

;;****************************************************************************
;; Comment-uncomment shorcut
;;****************************************************************************
(global-set-key "\C-c;" 'comment-or-uncomment-region)

;;****************************************************************************
;; display the current time
;;****************************************************************************
;; (display-time)

;;****************************************************************************
;; alias y to yes and n to no
;;****************************************************************************
(defalias 'yes-or-no-p 'y-or-n-p)

;;****************************************************************************
;; Don't make backup files
;;****************************************************************************
(setq make-backup-files 0 backup-inhibited t)

;;****************************************************************************
;; Erlang-mode
;;****************************************************************************
(if (file-directory-p "/usr/local/lib/erlang/lib/tools-2.6.10/emacs")
    (progn
      ;; OLD locally installed erlang
      (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.10/emacs"
                            load-path))              ; <-- insert your path
      (setq erlang-root-dir "/usr/local/lib/erlang")       ; <-- insert your path
      (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))) ; <-- insert your path
    (progn
      ;; NEW erlang installed by distribution
      (setq load-path (cons "/usr/lib/erlang/lib/tools-2.7/emacs"
                            load-path))              ; <-- insert your path
      (setq erlang-root-dir "/usr/lib/erlang")       ; <-- insert your path
      (setq exec-path (cons "/usr/lib/erlang/bin" exec-path)))) ; <-- insert your path

(require 'erlang-start)

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-sname" "emacs@localhost"
                                                    "+K" "true"))
            (imenu-add-to-menubar "imenu")))

;;****************************************************************************
;; Load Distel
;;****************************************************************************
(push (expand-file-name "~/emacs/distel/elisp") load-path) ; <-- insert your path
(require 'distel)
(distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;****************************************************************************
;; disable colors in emacs
;;****************************************************************************
(global-font-lock-mode 0)

;;****************************************************************************
;; Disable the splash screen
;;****************************************************************************
(setq inhibit-splash-screen 1)

;;****************************************************************************
;; Misc customizations
;;****************************************************************************
(transient-mark-mode t)                 ; highlight selected region
(column-number-mode t)                  ; show column number in status line
(show-paren-mode t)                     ; highlight matching paren

;;****************************************************************************
;; Load Geiser: Guile Scheme
;;****************************************************************************
;; (load "/home/evgeny/emacs/geiser/elisp/geiser.el") ; <-- insert your path


;;****************************************************************************
;; Ident whole buffer, bind it to 'C-x \'
;;****************************************************************************
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) 0)
    (untabify (point-min) (point-max))))

(global-set-key "\C-x\\" 'indent-buffer)
