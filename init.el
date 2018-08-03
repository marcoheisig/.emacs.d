" -*- coding: utf-8; no-byte-compile: t; -*-
#+TITLE: Marco Heisig's Emacs configuration
#+EMAIL: marco.heisig@fau.de
#+PROPERTY: header-args:emacs-lisp :results silent
#+OPTIONS: H:2

This is Marco Heisig's [[http://www.gnu.org/software/emacs/emacs.html][Emacs]] configuration. It is written in a Literal
Programming style and the [[http://www.orgmode.org][Org mode]] is used to manage the individual code
snippets.

* Introduction
This file is divided into several chapters. The first chapter, [[*Meta Configuration][Meta
Configuration]], describes how the configuration itself is loaded and how
missing functionality is obtained using the Emacs package manager. The
chapter [[*Minor Modes and Miscellaneous Utilities][Minor Modes and Miscellaneous Utilities]] enables and configures a
plethora of secondary features for an amazing Emacs experience. The third
chapter [[*Major Modes][Major Modes]] contains configuration sorted by the buffer type it
applies to, like the `c-mode' for operating on files in the C
language. Most human computer interaction is placed separately in the
chapter [[*User%20Interface][User Interface]]. Prominent features of this chapter are color
themes, key bindings, undo and redo, auto completion and the choice of
initial open buffers.

A word of warning -- this configuration file is heavily centered around the
[[https://www.emacswiki.org/emacs/Evil][Evil mode]]. Seasoned Emacs users might be surprised by the Vi-style
key bindings. The author had to switch the layout due to pinky finger
exhaustion. This is probably a sign of being unworthy, certainly not that
the default Emacs key bindings are cumbersome.

If you are not Marco Heisig and plan to use this configuration, some lines
should be adapted accordingly. As a starting point, you should adapt all lines
containing `Marco', `Heisig', `phone' or `crypt-key'.

A final remark -- this configuration is not optimized for load time. It is
therefore strongly recommended to use Emacs as a server, which is as simple
as using the following command to launch a session:

#+BEGIN_SRC sh :eval no
emacsclient -n -c -a ''
#+END_SRC

* Meta Configuration
This chapter deals with the nature of Emacs customization, hence the
`Meta'. It manages paths in the file system and utility functions for all other
chapters.

** Loading
These are magic incantations that make this file also a valid Emacs
`init.el' file. They are only interesting for seasoned Emacs Lisp hackers,
others may skip this section. For those curious how it is possible to
`load' this file from Emacs, it may be enlightening to inspect it in
`M-x emacs-lisp-mode'.

#+BEGIN_SRC emacs-lisp :eval no :export no :wrap ?"
(require 'cl-lib) ;; enable Common Lisp features

(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each
error is of the form (MARKER . MESSAGE).")

(defvar init.el-missing-packages '()
  "A list of packages that were demanded during initialization,
  but were not installed.")

(defvar init.el-missing-features '()
  "A list of features that were demanded during initialization,
  but could not be required.")

(defvar init.el-marker (make-marker)
  "Approximation to the currently executed position in
  init.el. Used to generate accurate error messages.")

(defun init.el-display-summary ()
  (if (not init.el-errors)
      (message "Initialization successful - happy hacking.")
    (message
     "There have been %d error(s) and %d missing package(s) during init:\n%s"
     (length init.el-errors)
     (length init.el-missing-packages)
     (mapconcat
      (lambda (init.el-error)
        (pcase-let ((`(,marker . ,msg) init.el-error))
          (format "Line %d: %s"
                  (save-excursion
                    (set-buffer (marker-buffer marker))
                    (line-number-at-pos (marker-position marker)))
                  msg)))
      init.el-errors
      "\n"))))

(defmacro init.el-with-error-handling (&rest body)
  "Useful during Emacs initialization. Catch and all errors and
add them to `init.el-errors'."
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         ,(macroexp-progn body)
       (error
        (push
         (cons (copy-marker init.el-marker)
               (error-message-string ,err))
         init.el-errors)))))

(defun init.el-execute-next-src-block ()
  "Execute the next emacs-lisp source code block. Return T if a
block was successfully executed and NIL if no block could be
found."
  (let ((block-executed? nil)
        (buffer (current-buffer))
        (src-regexp ;; copy paste from org mode's ob-core.el
         (concat
          ;; (1) indentation                 (2) lang
          "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
          ;; (3) switches
          "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
          ;; (4) header arguments
          "\\([^\n]*\\)\n"
          ;; (5) body
          "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    ;; scan for org babel source code blocks
    (while (and (not block-executed?)
                (re-search-forward src-regexp nil t))
      (let ((lang (match-string-no-properties 2))
            (switches (match-string-no-properties 3))
            (header-args (match-string-no-properties 4))
            (beg-body (match-beginning 5))
            (end-body (match-end 5))
            (end-block (match-end 0)))
        (when (and (string-equal lang "emacs-lisp")
                   ;; TODO relax the heuristics when to execute a src-block
                   (string-equal header-args "")
                   (string-equal switches ""))
          ;; a suitable source code block is found, execute it cautiously
          (goto-char beg-body)
          (save-restriction
            (narrow-to-region beg-body end-body)
            ;; execute each s-expression individually
            (while (scan-sexps (point) 1)
              (save-window-excursion
                (eval
                 (prog1 (read buffer)
                   (set-marker init.el-marker
                               (scan-sexps (point) -1)
                               buffer)))))
            (setf block-executed? t)))
        (goto-char end-block)))
    block-executed?))

(defun init ()
  "Traverse the initialization file and try to execute all its source
blocks. Any errors that occur are stored in `init.el-errors'."
  (interactive)
  (setq init.el-errors '())
  (setq init.el-missing-packages '())
  (setq init.el-missing-features '())

  ;; now execute all relevant org-src blocks
  (let ((inhibit-redisplay (not init-file-debug)) ;; less flickering
        (message-log-max init-file-debug)         ;; silence
        (inhibit-message (not init-file-debug)))  ;; more silence in Emacs 25+
    (save-window-excursion
      (find-file-existing user-init-file)
      (emacs-lisp-mode) ;; make forward-sexp etc. behave well
      (while (init.el-with-error-handling
              (init.el-execute-next-src-block)))
      (unless init-file-debug
        (kill-buffer "*Messages*")) ;; clear *Messages*
      (goto-char (point-min))
      (revert-buffer nil 1)
      (org-mode)))
  (when (interactive-p)
    (init.el-display-summary)))

(init)

;; make `load' skip the rest of this file
(setf load-read-function
      (lambda (&optional stream)
        (set-buffer stream)
        (goto-char (point-max))
        '(setf load-read-function nil)))
#+END_SRC

** Ensuring Packages, Features and Files
Traditionally Emacs loads extensions via the function `require', which
locates a suitable file containing the matching `provide' form. Those files
can either be placed manually in the `load-path' variable, or conveniently
installed with the [[info:Emacs#Package][Emacs package manager]]. The following functions ensure
the presence of certain packages, features and files. Errors are signaled
if items cannot be ensured.

#+BEGIN_SRC emacs-lisp
(cl-flet ((define-error (name message)
            (if (fboundp 'define-error)
                (define-error name message)
              (put name
                   'error-conditions
                   `(error ,name))
              (put name 'error-message message))))
  (define-error 'package-error "Missing package(s)")
  (define-error 'feature-error "Missing feature(s)"))

(defun ensure-packages (&rest required-packages)
  (let ((missing-packages
         (cl-remove-if #'package-installed-p
                       required-packages)))
    (when missing-packages
      (when (string-equal
             (file-name-nondirectory
              (buffer-file-name))
             "init.el")
        (setf init.el-missing-packages
              (cl-remove-duplicates
               (append missing-packages
                       init.el-missing-packages))))
      (signal 'package-error missing-packages))
    (mapc ; most packages need to be also required...
       (lambda (x)
         (ignore-errors ; ... some dont, ignore their absence
           (require x)))
       required-packages)))

(defun ensure-files (&rest filenames)
  (dolist (filename filenames)
    (make-directory (file-name-directory filename) t)
    (unless (file-exists-p filename)
      (write-region "" nil filename))))

(defun ensure-features (&rest required-features)
  (let ((missing-features
         (cl-remove-if
          (lambda (x)
            (require x nil t))
          required-features)))
    (when missing-features
      (signal 'feature-error missing-features))))

(defun install-missing-packages ()
  "Install all packages that were missing while loading the Emacs
configuraton. More precisely load all packages in
`init.el-missing-packages'."
  (interactive)
  (if init.el-missing-packages
    (when (or (not (called-interactively-p 'any))
              (let ((use-dialog-box nil))
                (yes-or-no-p
                 (format "Install missing packages: %s "
                         init.el-missing-packages))))
      (package-refresh-contents)
      (mapc #'package-install
            init.el-missing-packages))
    (message "No missing packages need to be installed.")))
#+END_SRC

** The Epilogue Hook
Some things are best run at the end of initialization, even after the
designated Emacs hooks `emacs-startup-hook' and `window-setup-hook'.

#+BEGIN_SRC emacs-lisp
(defvar init.el-epilogue-hook '()
  "Hook run after processing all Emacs initialization.")

(add-hook
 'window-setup-hook
 (lambda ()
   (run-at-time
    0.02 nil
    (lambda ()
      (run-hooks 'init.el-epilogue-hook)))))

(defmacro init.el-epilogue (&rest body)
  "Have the expressions in BODY evaluated briefly after all Emacs
initialization has finished."
  (when body
    `(add-hook 'init.el-epilogue-hook
               (lambda () ,@body)
               t)))
#+END_SRC

Now the epilogue hook can be used to display a nice little summary whether
the initialization was successful.

#+BEGIN_SRC emacs-lisp
(init.el-epilogue
 (init.el-display-summary))
#+END_SRC

** Customization
Emacs has a convenient interface for customization, that can be accessed by
the command `M-x customize'. This configuration does not use the customization
facility and performs all its actions via Emacs Lisp code.

In order to avoid interference with custom set variables, the customize
information is stored in another independent file.

#+BEGIN_SRC emacs-lisp
(let ((filename "~/.emacs.d/custom.el"))
  (ensure-files filename)
  (setf custom-file filename))

(load custom-file)
#+END_SRC

** The Emacs Package Manager

#+BEGIN_SRC emacs-lisp
(make-directory "~/.emacs.d/elisp/" t)
(make-directory "~/.emacs.d/elpa/" t)

(defun update-load-path ()
  (save-excursion
    (let ((default-directory "~/.emacs.d/elpa/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
    (let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

(setf package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(update-load-path)

(package-initialize)
#+END_SRC

* Minor Modes and Miscellaneous Utilities
[[info:Emacs#Minor%20Modes][Minor Modes]] add a variety of secondary features to currently edited
buffers. Any number of minor modes can be active at a time.
** Undo Tree Mode
While the default Emacs undo mechanism never forgets past modifications,
moving to an old state requires moving through the whole history in
between. For multiple branches of history, this leads to longer and longer
undo chains. The undo tree mode brings speed and clarity in this respect by
showing the true nature of the undo history as a tree with suitable
navigation commands.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'undo-tree 'evil)
(setf undo-tree-visualizer-timestamps t)
(setf undo-tree-visualizer-diff t)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
#+END_SRC

** Flyspell
Flyspell adds spellchecking to Emacs.

#+BEGIN_SRC emacs-lisp
(ensure-features 'flyspell)

(setf flyspell-issue-message-flag nil)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
#+END_SRC

** The Evil Mode
The [[info:evil][Evil Mode]] is the most sophisticated Vi emulation for Emacs. This
section shows how to set it up.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'evil)
(setf evil-want-C-w-in-emacs-state t)
(setf evil-echo-state nil)
(setf evil-cjk-emacs-word-boundary t)
(setf evil-want-C-i-jump nil)
(setf evil-want-C-w-delete nil)

(defun enable-evil-motion-state ()
  "Useful for major mode hooks to evable evil motion state
unconditionally."
  (evil-motion-state 1))

(setf evil-emacs-state-tag " E")
(setf evil-normal-state-tag " N")
(setf evil-insert-state-tag " I")
(setf evil-motion-state-tag " M")
(setf evil-multiedit-insert-state-tag "∀I")
(setf evil-multiedit-state-tag "∀N")
(setf evil-operator-state-tag " O")
(setf evil-visual-state-tag " V")
(evil-mode 1)

;; retain Emacs semantics of M-.
(define-key evil-normal-state-map (kbd "M-.") (kbd "\\ M-."))

(add-hook 'help-mode-hook 'enable-evil-motion-state)
(add-hook 'w3m-mode-hook 'enable-evil-motion-state)
(add-hook 'package-menu-mode-hook 'enable-evil-motion-state)
(add-hook 'occur-mode-hook 'enable-evil-motion-state)
#+END_SRC

** Recording Emacs sessions with Camcorder
Some Emacs features are best explained with a short demonstration
video. The camcorder package allows to record Emacs sessions in many video
formats.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'camcorder)
(define-key global-map (kbd "<f12>") 'camcorder-mode)
#+END_SRC

** The Insidious Big Brother Database for Emacs
BBDB is a great address database written in Emacs Lisp.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'bbdb)
(setf bbdb-default-country "Germany"
      bbdb-file "~/.emacs.d/bbdb"
      bbdb-gui t)
(bbdb-initialize)
#+END_SRC

** Automatic Text Completion with Company
When enabled, Company displays possible completion candidates for
individual words. This is particularly useful in programming modes, where
the completions include defined functions and variables.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'company)
(setf company-idle-delay 0.02)
(setf company-minimum-prefix-length 1)

(defun enable-company-mode ()
  (company-mode 1))

(add-hook 'prog-mode-hook 'enable-company-mode)
(add-hook 'org-mode-hook 'enable-company-mode)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)
#+END_SRC

** Multiple Cursors
A convenient feature, especially when it comes to renaming multiple
occurrences of a variable in source code. In its simplest form, it suffices
to mark a word and press `R' to edit all its occurrences at the same time.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'evil-multiedit 'iedit)
(define-key evil-visual-state-map "R"
  'evil-multiedit-match-all)

(define-key evil-normal-state-map (kbd "M-d")
  'evil-multiedit-match-and-next)
(define-key evil-visual-state-map (kbd "M-d")
  'evil-multiedit-match-and-next)

(define-key evil-normal-state-map (kbd "M-D")
  'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D")
  'evil-multiedit-match-and-prev)

(define-key evil-visual-state-map (kbd "C-M-D")
  'evil-multiedit-restore)

(define-key evil-multiedit-state-map (kbd "RET")
  'evil-multiedit-toggle-or-restrict-region)

(define-key evil-visual-state-map (kbd "RET")
  'evil-multiedit-toggle-or-restrict-region)

(define-key evil-multiedit-state-map (kbd "C-n")
  'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-p")
  'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "C-n")
  'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "C-p")
  'evil-multiedit-prev)
#+END_SRC

** Openwith mode - Open certain buffers with external tools
Despite the best attempts of the Emacs community, Emacs can not (yet) open
all file types gracefully. The openwith mode compensates for this by
opening files with certain extensions in external programs. It is important
to adapt the variable `openwith-associations' to suit ones personal
preferences.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'openwith)
(openwith-mode)
(setf openwith-associations
      ;; note: no openwith-opening of .ps files or imaxima misbehaves
      '(("\\.\\(?:dvi\\|pdf\\|ps\\.gz\\|djvu\\)\\'"
         "evince" (file))
        ("\\.jar\\'"
         "java -jar" (file))
        ("\\.\\(?:odt\\|fodt\\|uot\\|docx\\|docx\\)\\'"
         "libreoffice" (file))
        ("\\.xcf\\'"
         "gimp" (file))
        ("\\.\\(?:mp3\\|wav\\|mp4\\|mpe?g\\|avi\\|flac\\|ogg\\|wma\\|m4a\\|mkv\\)\\'"
         "vlc" (file))))
#+END_SRC

** Incremental Completion with Helm
#+BEGIN_SRC emacs-lisp
(ensure-packages 'helm)
(ensure-features 'helm-config)
(setq helm-candidate-number-limit 100)

(setf helm-display-header-line nil
      helm-prevent-escaping-from-minibuffer t
      helm-split-window-in-side-p t
      helm-quick-update t
      helm-always-two-windows t
      helm-echo-input-in-header-line t
      helm-imenu-execute-action-at-once-if-one nil
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)

(setf helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-buffers-fuzzy-matching t)

(helm-autoresize-mode -1)
(helm-mode 1)
#+END_SRC

** Regular Expression Building
A mode for interactively building regular expressions and viewing their
effect on the selected buffer. Mostly made obsolete by the Evil mode search
and replace facility, but sometimes useful for complex regular expressions
with multiple grouping constructs.

#+BEGIN_SRC emacs-lisp
(ensure-features 're-builder)
(setf reb-re-syntax 'string)
#+END_SRC

** Bibliographic References with Reftex

#+BEGIN_SRC emacs-lisp
(ensure-packages 'reftex)
(defun org-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
         (setf TeX-master t)
         (turn-on-reftex)
         (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
         ;; This also changes any call to org-citation!
         (reftex-set-cite-format
          '((?c . "\\cite{%l}")
            (?t . "\\citet{%l}") ; natbib inline text
            (?i . "\\citep{%l}") ; natbib with parens
            )))))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)
#+END_SRC

** Image viewing with Emacs
Emacs can open images but does not re-scale them to fit to the buffer. The
`image+' library scales pictures accordingly.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'image+)
(imagex-global-sticky-mode)
(imagex-auto-adjust-mode)
#+END_SRC

** Handy abbreviations with Abbrev
General Unicode characters can be inserted via `C-x 8 RET', but this is too
cumbersome for many use cases. The abbrev mode allows this somewhat quicker
input method. Remember that the expansion of abbreviations can be canceled
by typing `C-q' before finishing the word.

#+BEGIN_SRC emacs-lisp
;;(ensure-packages 'org)
;;(ensure-features 'org-entities)
;; (let ((table ()))
;;   (mapc
;;    (lambda (x)
;;      (when (listp x)
;;        (let ((name (car x))
;;              (utf-8 (nth 6 x)))
;;          (when (and (= 1 (length utf-8))
;;                     (multibyte-string-p utf-8))
;;            (push (list name utf-8) table)))))
;;    org-entities))

(define-abbrev-table 'lisp-mode-abbrev-table
  '(("alpha" "α")
    ("beta" "β")))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
#+END_SRC

** Gracefully manage matching Parentheses with Smartparens

#+BEGIN_SRC emacs-lisp
(ensure-packages 'smartparens 'evil-smartparens)

(require 'smartparens-config)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(smartparens-global-mode t)
(show-smartparens-global-mode nil)

(add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
#+END_SRC

** Key Chord Mode
#+BEGIN_SRC emacs-lisp
(ensure-packages 'key-chord)
(key-chord-mode 1)
(setf key-chord-two-keys-delay 0.05)
(setf key-chord-one-key-delay 0.14)
#+END_SRC

* Major Modes
** The Org Mode
If Emacs is the place a programmer lives when using his computer, the [[info:org][Org mode]]
is likely to be his living room. At its core it is a mode for writing
structured plain text, but its many extensions allow it to blur the line
between organizing, note taking and programming in amazing ways.

*** Basics
#+BEGIN_SRC emacs-lisp
(ensure-packages 'org)
(require 'org)
(setf org-export-backends '(ascii html icalendar latex beamer odt))
(setf org-adapt-indentation nil)
(setf org-agenda-window-setup (quote current-window))
(setf org-catch-invisible-edits 'show)
(setf org-default-notes-file "~/.emacs.d/org/notes.org")
(setf org-directory "~/.emacs.d/org")
(setf org-pretty-entities t)
(setf org-highlight-latex-and-related '(latex))
(setf org-pretty-entities-include-sub-superscripts nil)
(setf org-return-follows-link t)
(setf org-special-ctrl-a/e nil)
(setf org-block-begin-line t)
(setf org-export-with-timestamps t)
(setf org-export-date-timestamp-format "%Y-%m-%d")
(setq-default org-tag-alist
              '(("crypt" . ?c)
                ("drill" . ?d)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ce" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook 'org-indent-mode)
#+END_SRC

*** Organizing with Org Agenda

#+BEGIN_SRC emacs-lisp
(ensure-files "~/.emacs.d/org/agenda-files.org"
              "~/.emacs.d/org/cal.org"
              "~/.emacs.d/org/notes.org"
              "~/.emacs.d/org/quotes.org")

(setf org-agenda-compact-blocks nil)
(setf org-agenda-files "~/.emacs.d/org/agenda-files.org")
(setf org-agenda-include-diary t)
(setf org-agenda-span 'week)
(setf org-agenda-sticky nil)
(setq
 org-capture-templates
 '(("t" "Task - needs to be done" entry
    (file+headline "~/.emacs.d/org/cal.org" "incoming")
    "* TODO %?
               Entered on %U")
   ("d" "Deadline - task with deadline" entry
    (file+headline "~/.emacs.d/org/cal.org" "incoming")
    "* TODO %?
               DEADLINE: %^t
               Entered on %U")
   ("e" "Event - something that happens at a specified date" entry
    (file+headline "~/.emacs.d/org/cal.org" "incoming")
    "* APPT %?
               When: %^t
               Entered on %U")
   ("n" "Note - capture some info" entry
    (file+headline "~/.emacs.d/org/notes.org" "incoming")
    "* %?
               Entered on %U")
   ("z" "Zitat - Capture noteworthy statements" entry
    (file+headline "~/.emacs.d/org/quotes.org" "Zitate")
    "* %^{Who said it} %U
               \"%?\"")))
#+END_SRC

*** Exporting Org mode buffers

#+BEGIN_SRC emacs-lisp
(ensure-packages 'cdlatex 'org 'htmlize)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(pushnew '("pdf" . "evince %s") org-file-apps)

(setf org-latex-create-formula-image-program 'imagemagick)
(setf org-latex-listings 'minted)
(add-to-list 'org-latex-default-packages-alist '("" "minted"))
(setf org-latex-minted-options
      '(("frame" "single")
        ("framesep" "6pt")
        ("encoding" "utf8")
        ("mathescape" "true")
        ("fontsize" "\\footnotesize")))
(setq
 org-latex-pdf-process
 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "bibtex $(basename %b)"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq
 LaTeX-command-style
 '((""
    "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
#+END_SRC

*** Managing source code with Org Babel

#+BEGIN_SRC emacs-lisp
(ensure-packages 'org 'gnuplot)
(setf org-edit-src-content-indentation 0)
(setf org-src-preserve-indentation nil)
(setf org-src-tab-acts-natively t)
(setf org-src-window-setup 'other-window)
(setq-default org-export-babel-evaluate 'inline-only)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))
#+END_SRC

*** Encrypting parts of a buffer with Org Crypt
#+BEGIN_SRC emacs-lisp
(ensure-features 'org-crypt)
(setf auto-save-default nil)
(org-crypt-use-before-save-magic)
(setf org-tags-exclude-from-inheritance '("crypt"))
(setf org-crypt-key "05369722")
#+END_SRC

*** Beautiful Presentations with Org Reveal

#+BEGIN_SRC emacs-lisp
(ensure-packages 'ox-reveal)
(setq org-reveal-root "~/userdata/proj/reveal.js")
#+END_SRC

*** Efficient Learning with Org drill
Org drill is an amazing tool to learn new facts. In a first step, one creates
drill cards, which are nothing more than org sub trees with some meta data and the
`:drill:' tag. Afterwards the command `org-drill' will start a sophisticated
drill session.

#+BEGIN_SRC emacs-lisp
;(ensure-packages 'org)
;(ensure-features 'org-drill)
;; prevent drill hints from ruining Latex formulas
;(setf org-drill-hint-separator "||HINT||")
#+END_SRC

Below is the helpful bug fix for a org-drill redisplay issue.

#+BEGIN_QUOTE
Thanks!  I can reproduce your issue with a relatively fresh Emacs 25 and
org from git.

AFAICS the window gets blanked sometimes in function
org-toggle-latex-fragment of org.el in line

--8<---------------cut here---------------start------------->8---
;; Work around a bug that doesn't restore window's start
;; when widening back the buffer.
(set-window-start nil window-start)
--8<---------------cut here---------------end--------------->8---

A workaround would be to comment out just this line.

I don't know if this is a reliable fix though.
#+END_QUOTE

** Latex Editing with Auctex
Auctex is by far the best Latex editing environment on the planet, only
surpassed by the Org mode Latex export facility and `cdlatex'.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'auctex 'company-auctex)
(ensure-features 'latex)
(setq-default TeX-PDF-mode t)
(company-auctex-init)
;; Use Okular as the primary PDF viewer
(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular")
(pushnew '(output-pdf "Okular") TeX-view-program-selection
         :key #'cadr
         :test #'string=)
#+END_SRC

** EIRC
#+BEGIN_SRC emacs-lisp
(setf erc-nick "heisig")
(setf erc-port 6667)
(setf erc-server "chat.freenode.net")

(add-hook 'erc-mode-hook 'tweak-erc)
#+END_SRC

** Directory Browsing with Dired
Dired is the directory browser in Emacs. There are numerous little tweaks
to enhance the dired usability. One of them is simply to activate dired+.
Another one is to enable recursive copies and enable
`dired-dwim-target'. The latter allows to copy and move whole folders
between adjacent [[info:Emacs#Windows][Emacs windows]].

#+BEGIN_SRC emacs-lisp
(ensure-features 'dired)
(setf dired-dwim-target t
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      dired-listing-switches "-ahl"
      dired-auto-revert-buffer t
      wdired-allow-to-change-permissions 'advanced)
#+END_SRC

#+BEGIN_SRC emacs-lisp
(ensure-packages 'dired+)
(global-dired-hide-details-mode 1)
#+END_SRC

Dired narrow is a handy tool to filter the files in a dired buffer.
#+BEGIN_SRC emacs-lisp
(ensure-packages 'dired-narrow)
(define-key dired-mode-map (kbd "C-x /") 'dired-narrow)
#+END_SRC

Sometimes one wishes to perform quick conversions from one file type to another,
preferably for all currently marked files in a Dired buffer. The function
`dired-convert' attempts to perform such a conversion with several
heuristics. The command is strictly more useful than `dired-compress' and is
therefore bound to the key `Z' instead.

#+BEGIN_SRC emacs-lisp
(defgroup dired-convert nil
  "Quick and dirty file conversions/extractions."
  :prefix "dired-convert-"
  :group 'dired)

(makunbound 'dired-convert-extraction-associations)
(defcustom dired-convert-extraction-associations
  '(("\\.\\(?:gz\\|tgz\\|bz2\\|xz\\|zip\\|rar\\|7z\\)\\'"
     "7z" "x" src)
    ("\\.tar\\'"
     "tar" "xpf" src))
  "Associations of file patterns to external programs that
`dired-convert' uses to detect how to extract a file."
  :type '(alist
          :key-type (regexp :tag "Pattern")
          :value-type (repeat
                       (choice string
                               (const src)
                               (const dst)))))

(makunbound 'dired-convert-conversion-associations)
(defcustom dired-convert-conversion-associations
  '(("\\.\\(?:mp3\\|ogg\\|wma\\|flac\\|wav\\|m4a\\)\\'"
     "ffmpeg" "-i" src "-ab" "192K" "-vn" dst)
    ("\\.\\(?:flv\\|mov\\|mp4\\|webm\\|mkv\\)\\'"
     "ffmpeg" "-i" src "-ab" "192K" dst)
    ("\\.\\(?:png\\|tga\\|bmp\\|jpeg\\|jpg\\|gif\\|svg\\)\\'"
     "convert" src dst)
    ("\\.tar.gz\\'"
     "tar" "czvpf" dst src)
    ("\\.tar.xz\\'"
     "tar" "cJvpf" dst src)
    ("\\.tar.bz2\\'"
     "tar" "cjvpf" dst src)
    ("\\.\\(?:gz\\|tgz\\|bz2\\|xz\\|zip\\|rar\\|z\\)\\'"
     "7z" "a" dst src)
    ("\\.tar\\'"
     "tar" "cpf" dst src))
  "Associations of file patterns to external programs that
`dired-convert' uses to convert a file to the specified
extension."
  :type '(alist
          :key-type (regexp :tag "Pattern")
          :value-type (repeat
                       (choice string
                               (const src)
                               (const dst)))))

(defun dired-convert (filenames target-extension)
  "Try to perform a quick and dirty conversion of all files
specified by the list of strings FILENAMES to the format
specified by TARGET-EXTENSION. A TARGET-EXTENSION of \"\" issues
an extraction instead.

If used interactively, FILENAMES defaults to the list of marked
files or the file under the cursor if the former is empty."
  (interactive
   (list (or (dired-get-marked-files 'no-dir)
             (dired-get-filename 'no-dir)
             (list (read-file-name "File to convert: ")))
         (read-string "Desired extension (empty = extract): ")))
  (cl-flet
      ((cmdize
        (src dst elements)
        (when (null elements)
          (error "Do not know how to operate on %s\n" src))
        (mapconcat
         (lambda (x)
           (shell-quote-argument
            (pcase x
              ((pred stringp) x)
              (`src src)
              (`dst dst)
              (_ (error "Invalid element %s" x)))))
         elements " ")))
    (let ((ex (string-equal target-extension "")))
      (when (yes-or-no-p
             (if ex (format "Extract the files %S?" filenames)
               (format "Convert the files %S to %s?"
                       filenames target-extension)))
        (dolist (src filenames)
          (let ((dst (concat (file-name-directory src)
                             (file-name-base src)
                             "." target-extension)))
            (let ((key (if ex src dst))
                  (assocs (if ex dired-convert-extraction-associations
                            dired-convert-conversion-associations)))
              (let ((cmd (cmdize
                          src dst
                          (cdr
                           (cl-find-if
                            (lambda (x)
                              (string-match x key))
                            assocs :key #'car)))))
                (message "%s" cmd) ;; be verbose
                (shell-command cmd))))
          ;; make newly created files appear in dired immediately
          (revert-buffer nil t)
          (redisplay))))))

(define-key dired-mode-map (kbd "Z") 'dired-convert)
#+END_SRC

** The C Langugage Family
[[info:ccmode#Top][cc-mode]] is a GNU Emacs mode for editing files containing C, C++,
Objective-C, Java, CORBA IDL (and the variants PSDL and CIDL), Pike and
AWK code.

#+BEGIN_SRC emacs-lisp
(ensure-features 'cc-mode)
(add-to-list 'auto-mode-alist `("\\.cl\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.frag\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.vert\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.jad\\'" . java-mode))
(setf c-basic-offset 4
      c-hanging-braces-alist (quote set-from-style)
      c-offsets-alist (quote ((innamespace . 0))))
#+END_SRC

The language C++ is the first to my knowledge where Emacs stutters with maximal
syntax highlighting. Unfortunately it is not possible to change the ridiculously
complex syntax of C++ (current standardization efforts are even going in the
opposite direction), so as an alternative the font lock decoration level is
lowered.

#+BEGIN_SRC emacs-lisp
(setf font-lock-maximum-decoration (quote ((c++-mode . 2) (t . t))))
#+END_SRC

** Common Lisp
One of the best programming languages on the planet - and thanks to the
effort of many great programmers, it is as of 2016 more pleasant to use
than ever. The [[https://www.common-lisp.net][Common Lisp website]] is a good start for all those who want
to start learning this programming language. Because as Eric S. Raymond put
it:

#+BEGIN_QUOTE
Lisp is worth learning for the profound enlightenment experience you will
have when you finally get it; that experience will make you a better
programmer for the rest of your days, even if you never actually use Lisp
itself a lot.
#+END_QUOTE

#+BEGIN_SRC emacs-lisp
(ensure-packages 'slime 'slime-company)
(setf inferior-lisp-program "sbcl")
(slime-setup
 '(slime-fancy
   slime-sbcl-exts
   slime-cl-indent
   slime-sprof
   slime-asdf
   slime-fancy-inspector
   slime-company
   slime-autodoc))

(put 'make-instance 'common-lisp-indent-function 1)
(put 'change-class 'common-lisp-indent-function 2)
(put 'reinitialize-instance 'common-lisp-indent-function 1)
(put 'define-package 'common-lisp-indent-function 1)
(put 'dx-flet 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
(put 'dx-let 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'dx-let* 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))

(defun tweak-slime-repl ()
  (setf tab-always-indent t) ; prevent the annoying default completion
  (slime-company-maybe-enable)) ; activate a sane completion

(add-hook 'slime-repl-mode-hook 'tweak-slime-repl)

(defun start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; start SLIME automatically when visiting a file
(add-hook 'slime-mode-hook 'start-slime)

(global-set-key (kbd "C-c s") 'slime-selector)

(define-key slime-mode-map (kbd "C-c m") 'slime-macroexpand-1)
(define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
(define-key slime-mode-map (kbd "C-c d") 'slime-disassemble-symbol)

(define-key slime-repl-mode-map (kbd "C-c m") 'slime-macroexpand-1)
(define-key slime-repl-mode-map (kbd "C-c i") 'slime-inspect)
(define-key slime-repl-mode-map (kbd "C-c d") 'slime-disassemble-symbol)
#+END_SRC

Furthermore, make the Common Lisp Hyperspec (CLHS) accessible withing Emacs.

#+BEGIN_SRC emacs-lisp
(ensure-features 'w3m)

(setf w3m-default-display-inline-images t)

(setf common-lisp-hyperspec-root "~/userdata/literature/cs/lisp/CLHS/")

(defun use-w3m (fn &rest args)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (apply fn args)))

(advice-add 'common-lisp-hyperspec :around 'use-w3m)
(advice-add 'common-lisp-hyperspec-lookup-reader-macro :around 'use-w3m)
(advice-add 'common-lisp-hyperspec-format :around 'use-w3m)

(global-set-key (kbd "C-c h h") 'common-lisp-hyperspec)
(global-set-key (kbd "C-c h r") 'common-lisp-hyperspec-lookup-reader-macro)
(global-set-key (kbd "C-c h f") 'common-lisp-hyperspec-format)
(global-set-key (kbd "C-c h g") 'common-lisp-hyperspec-glossary-term)
#+END_SRC

** Emacs Lisp
The language Emacs Lisp is a fine blend of Maclisp, Common Lisp and some
language for editing text. Unsurprisingly Emacs is well suited for editing
Emacs Lisp. The only worthwhile addition provided here is a simple Macro
stepper called `macroexpand-point'.

#+BEGIN_SRC emacs-lisp
(define-derived-mode emacs-lisp-macroexpand-mode emacs-lisp-mode
  "Macro Expansion"
  "Major mode for displaying Emacs Lisp macro expansions."
  (setf buffer-read-only t))

(define-key emacs-lisp-mode-map
  (kbd "C-c m") 'macroexpand-point)

(defun macroexpand-point (arg)
  "Apply `macroexpand' to the S-expression at point and show
the result in a temporary buffer. If already in such a buffer,
expand the expression in place.

With a prefix argument, perform `macroexpand-all' instead."
  (interactive "P")
  (let ((bufname "*emacs-lisp-macroexpansion*")
        (bounds (bounds-of-thing-at-point 'sexp))
        (expand (if arg #'macroexpand-all #'macroexpand)))
    (unless bounds
      (error "No S-expression at point."))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (expansion
            (funcall expand
                     (first
                      (read-from-string
                       (buffer-substring beg end))))))
      (if (eq major-mode 'emacs-lisp-macroexpand-mode)
          (let ((inhibit-read-only t)
                (full-sexp
                 (car (read-from-string
                       (concat
                        (buffer-substring (point-min) beg)
                        (prin1-to-string expansion)
                        (buffer-substring end (point-max)))))))
            (delete-region (point-min) (point-max))
            (save-excursion
              (pp full-sexp (current-buffer)))
            (goto-char beg))
        (let ((temp-buffer-show-hook '(emacs-lisp-macroexpand-mode)))
          (with-output-to-temp-buffer bufname
            (pp expansion)))))))
#+END_SRC

** Maxima
#+BEGIN_SRC emacs-lisp
(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(ensure-features 'maxima)
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist `("\\.ma[cx]\\'" . maxima-mode))

(ensure-features 'imaxima)
;; This is a little bugfix, otherwise imaxima decided the equation color
;; was NIL and would fail
(setf imaxima-equation-color "#DCDCCC")

(setf imaxima-use-maxima-mode-flag t)
(setf imaxima-latex-preamble "
\\usepackage{concrete}
\\usepackage{euler}
")
(setf imaxima-scale-factor 1.4)
#+END_SRC

** Scheme Programming
#+BEGIN_SRC emacs-lisp
(ensure-packages 'geiser)
(setf geiser-default-implementation "guile")
(setf scheme-program-name "guile")
(add-to-list 'auto-mode-alist `("\\.sc\\'". scheme-mode))
#+END_SRC

** Octave like languages
There is a whole family of programming environments for applied mathematics
with Octave-like syntax.
#+BEGIN_SRC emacs-lisp
(ensure-features 'octave)
(add-to-list 'auto-mode-alist `("\\.sci\\'". octave-mode))
(add-to-list 'auto-mode-alist `("\\.m\\'". octave-mode))
#+END_SRC

** Scala
#+BEGIN_SRC emacs-lisp
(ensure-packages 'scala-mode)
(add-to-list 'auto-mode-alist `("\\.scala\\'". scala-mode))
#+END_SRC

** Gnus - More than an Email program
#+BEGIN_SRC emacs-lisp
(setq gnus-select-method
      '(nnimap "FAUMail"
               (nnimap-address "groupware.fau.de")
               (nnimap-stream starttls)))

(setq smtpmail-smtp-server "groupware.fau.de"
      smtpmail-smtp-service 587)
#+END_SRC

** Magit
#+BEGIN_SRC emacs-lisp
(ensure-packages 'magit 'evil-magit)
(setf evil-magit-state 'motion)
#+END_SRC

* User Interface
This chapter is concerned with the interaction between Emacs and human
beings. The aim is to provide a very distraction free environment for the
experienced Emacs user.

** Trivial Modifications
First of all there are dozens of minor modifications that deserve no
individual explanation beyond the hint that `C-h v' and `C-h f' explain an
Emacs variable and function, respectively.

#+BEGIN_SRC emacs-lisp
(setf user-full-name "Marco Heisig")
(setf user-mail-address "marco.heisig@fau.de")
(setf frame-title-format "%b - Emacs")
(setf large-file-warning-threshold (* 1000 1000 400))
(setf read-file-name-completion-ignore-case t)
(setf read-buffer-completion-ignore-case t)
(setf visible-bell nil)
(setf ring-bell-function (lambda ())) ; AKA do nothing
(prefer-coding-system 'utf-8)
(setf save-abbrevs nil)
(column-number-mode 1)
(setf echo-keystrokes 0.01)
(setq-default fill-column 75)
(setq-default truncate-lines t)
(setq-default initial-major-mode 'org-mode)
(setq-default major-mode 'org-mode)
(setf require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setf indicate-buffer-boundaries nil)
(setf fringe-mode 4)
(setq-default indicate-empty-lines t)
(setf initial-scratch-message nil)
(setf pop-up-frames nil)
(setf inhibit-startup-screen t)
(setf ispell-dictionary "american")
(advice-add 'display-startup-echo-area-message :override #'ignore)
#+END_SRC

Emacs wizards have memorized all their commands and have no need for visual
guidance.

#+BEGIN_SRC emacs-lisp
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
#+END_SRC

Emacs disables several interactive functions by default, because they would
confuse the unsuspecting user even more than the average Emacs function. A
user of this configuration should familiarize himself with those commands or
leave them disabled.

#+BEGIN_SRC emacs-lisp
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; (put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
#+END_SRC

By default, there are two major annoyances in Emacs. First, killing a
buffer with an attached process asks for confirmation every single
time. Second, a prompt for interactive confirmation requires the user to type
`y e s RET' instead of a simple `y'. The next snippet fixes both these issues.

#+BEGIN_SRC emacs-lisp
(setf kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defalias 'yes-or-no-p 'y-or-n-p)
#+END_SRC

Now some customization of the highlighting of matching parentheses.

#+BEGIN_SRC emacs-lisp
(setf show-paren-delay 0
      blink-matching-paren nil
      show-paren-style 'parenthesis)
(show-paren-mode)
#+END_SRC

Visually indicate the two major sins in programming: tabs and trailing
white space.

#+BEGIN_SRC emacs-lisp
(setf whitespace-style '(face trailing tab-mark))
(global-whitespace-mode 1)
#+END_SRC

Another thing that has probably scared away thousands of potential Emacs
users -- the automatic backup files. As soon as an Emacs user gets his
hands on a file, it clobbers the directory with a backup file with a
tilde. This configuration disables auto saving by default. If autosaving is
activated, it places the files in a temporary directory. There are two
reasons for turning off auto saving by default: The first one is that if a
file really matters, one should use a versioning tool like Git and a
hardware backup solution. The second, more mundane one is that the
org-crypt mode does not work well with auto-save.

#+BEGIN_SRC emacs-lisp
(setf auto-save-default nil
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist (quote (("." . "~/.emacs.d/saves")))
      backup-inhibited nil)
#+END_SRC

Emacs supports a ridiculously sophisticated mechanism to figure out where
to display new buffers. The following setup tries to keep the number of
open windows small.

#+BEGIN_SRC emacs-lisp
(setf split-height-threshold 100)
(setf split-width-threshold 160)
#+END_SRC

** Encryption
#+BEGIN_SRC emacs-lisp
(ensure-features 'epa-file)
(epa-file-enable)

(defun truly-random-letter ()
  (let ((letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?"))
    ;; note: letters has 64 characters, or 6 bits of information
    (assert (= 64 (length letters)))
    (elt letters
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (call-process "head" "/dev/urandom" (current-buffer) nil "-c1")
           (mod (get-byte 1) 64)))))

(defun insert-new-password (length)
  (interactive "nlength: ")
  (insert
   (apply #'string (loop repeat length collect (truly-random-letter)))))
#+END_SRC

** Color Theme Enhancements
In Emacs terminology, rendering attributes of each character are called
[[info:Elisp#faces][Faces]]. Traditionally a color theme defines the appearance of each
face. However hardly any color theme is exhaustive and sets colors like
`rainbow-delimiters-depth-9-face' or `org-block-begin-line'. The following
code derives sane values for such faces automatically. As a result, one can
load any color theme and get a consistent experience.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'rainbow-delimiters 'org 'dired+)

(defun respec-face (face &rest arguments)
  (when (member face (face-list))
    (face-spec-reset-face face)
    (apply #'set-face-attribute face nil arguments)))

(defun hsl (color-name)
  (apply #'color-rgb-to-hsl (color-name-to-rgb color-name)))

(defun hue-clamp (value)
  (- value (floor value)))

(defun merge-hsl (wh1 ws1 wl1 color1 wh2 ws2 wl2 color2)
  (cl-multiple-value-bind (h1 s1 l1) (hsl color1)
    (cl-multiple-value-bind (h2 s2 l2) (hsl color2)
      (apply
       #'color-rgb-to-hex
       (color-hsl-to-rgb
        (hue-clamp   (+ (* wh1 h1) (* wh2 h2)))
        (color-clamp (+ (* ws1 s1) (* ws2 s2)))
        (color-clamp (+ (* wl1 l1) (* wl2 l2))))))))

(defun derive-faces (&rest args)
  (redraw-display) ;; make theme change actually happen
  (let* ((default-bg (face-background 'default))
         (default-fg (face-foreground 'default))
         (string-fg (face-foreground
                     'font-lock-string-face
                     nil 'default))
         (comment-fg (face-foreground
                      'font-lock-comment-face
                      nil 'default))
         (block-bg (merge-hsl
                    1.0 1.0 0.9 default-bg
                    0.0 0.0 0.1 default-fg)))

    ;; Derive suitable colors for org-blocks
    (respec-face 'org-block-begin-line
                 :background block-bg
                 :foreground comment-fg)
    (respec-face 'org-block-end-line
                 :inherit 'org-block-begin-line)
    (respec-face 'org-block
                 :background (merge-hsl
                              0.0 0.3 0.3 default-bg
                              1.0 0.7 0.7 block-bg)
                 :inherit 'unspecified)

    ;; Make rainbow-delimiters actually a rainbow
    (dotimes (i 0) ;; disable
      (let ((face
             (intern
              (format "rainbow-delimiters-depth-%d-face" (+ i 1)))))
        (respec-face face :foreground
                     (merge-hsl
                      1.0 1.0 1.0 default-fg
                      (+ 0.5 (* i 0.166)) 0.8 0.0 "#ff00ff"))))

    ;; dired+ faces
    (respec-face 'diredp-compressed-file-suffix :inherit 'font-lock-comment-face)
    (respec-face 'diredp-file-suffix :inherit 'font-lock-comment-face)
    (respec-face 'diredp-date-time :inherit 'font-lock-comment-face)
    (respec-face 'diredp-dir-heading :inherit 'org-block-begin-line)
    (respec-face 'diredp-dir-name :inherit 'dired-directory)
    (respec-face 'diredp-symlink :inherit 'font-lock-constant-face)
    (respec-face 'diredp-file-name :inherit 'default)
    (respec-face 'diredp-ignored-file-name :inherit 'font-lock-comment-face)
    (respec-face 'diredp-number :inherit 'font-lock-constant-face)
    (respec-face 'diredp-flag-mark :inherit 'highlight)
    (respec-face 'diredp-flag-mark-line :inherit 'highlight)
    (respec-face 'diredp-deletion :inherit 'warning)
    (respec-face 'diredp-deletion-file-name :inherit 'warning)
    (respec-face 'diredp-dir-priv :inherit 'dired-directory)
    (respec-face 'diredp-read-priv :inherit 'rainbow-delimiters-depth-1-face)
    (respec-face 'diredp-write-priv :inherit 'rainbow-delimiters-depth-2-face)
    (respec-face 'diredp-exec-priv :inherit 'rainbow-delimiters-depth-3-face)
    (respec-face 'diredp-link-priv :inherit 'default)
    (respec-face 'diredp-rare-priv :inherit 'default)

    ;; regexp-grouping constructs should have the same color as the
    ;; `font-lock-string-face', but different emphasis.
    (respec-face 'font-lock-regexp-grouping-backslash
                 :foreground (merge-hsl
                              0.0 0.5 0.5 default-bg
                              1.0 0.5 0.5 string-fg))

    (respec-face 'font-lock-regexp-grouping-construct
                 :foreground (merge-hsl
                              0.0 0.5 0.75 default-bg
                              1.0 0.5 0.75 string-fg)
                 :weight 'bold)))

;; run derive-faces after every usage of `load-theme'
(advice-add 'load-theme :after #'derive-faces)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(defun disable-show-paren-mode ()
  (show-paren-mode -1))

(add-hook 'rainbow-delimiters-mode-hook 'disable-show-paren-mode)
#+END_SRC

** The Color Theme and Mode Line
This chapter deals with the visual appearance of Emacs. Interested readers
might want to read the section [[info:Elisp#Display][Display]] of the Emacs Lisp manual.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'doom-themes 'spaceline)
(ensure-features 'spaceline-config)
(require 'airline-themes) ;; I currently don't use the ELPA version

(setf doom-themes-enable-bold nil)
(setf powerline-default-separator 'wave)
(set-face-attribute 'button nil :inherit 'link)
(airline-themes-set-modeline)

(defun init.el-load-themes ()
  (load-theme 'doom-nord t)
  (doom-themes-org-config)
  (load-theme 'airline-nord t)
  (custom-theme-set-faces
   'airline-nord
   `(minibuffer-prompt ((t (:foreground nil :background nil :inherit 'default))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (init.el-load-themes))))
  (init.el-load-themes))
#+END_SRC

Org mode presents two options for rendering source code blocks. The default
one is to use the `org-block' face. The other one can be activated by
setting `org-src-fontify-natively' to a non-nil value and displays the code
according to the major mode of its language. The approach here is a hybrid
solution that first uses native fontification, but the applies a different
background to illustrate the block structure.

#+BEGIN_SRC emacs-lisp
(setf org-src-fontify-natively t)

(defun org-src-fontification--after (lang start end)
  (let ((pos start) next)
    (while (and (setq next (next-single-property-change pos 'face))
                (<= next end))
      ;; org-src occasionally places invalid faces of nil in the text
      ;; properties, so I have to remove them before calling
      ;; `add-face-text-property'.
      (unless (get-text-property pos 'face)
        (remove-text-properties pos next '(face nil)))
      (add-face-text-property
       pos next 'org-block)
      (setq pos next))))

(advice-add 'org-src-font-lock-fontify-block
            :after #'org-src-fontification--after)
#+END_SRC

** Convenience Utilities
This section contains some commands that help Emacs do "the right thing" in
a given context.

The function `save-if-appropriate' executes a bunch of heuristics to
determine whether now is a good moment to save the current buffer. Actually
these heuristics work so good, that one can bind this command to the very
frequently used `evil-normal-state-entry-hook'.

#+BEGIN_SRC emacs-lisp
(defun save-if-appropriate ()
  "Save the buffer unless saving is unpleasant or meaningless."
  (let ((appropriate
         (and
          (buffer-modified-p) ; there should be modifications
          (buffer-file-name)  ; there should be a file to save to
          (file-exists-p (buffer-file-name))
          (case major-mode
            ;; no org-crypt tags should be present
            (org-mode (not (org-map-entries t "+crypt" 'file)))
            (otherwise t)))))
    (when appropriate
      (save-buffer))))

(add-hook 'evil-insert-state-exit-hook 'save-if-appropriate)
#+END_SRC

Another frequent operation is to `leave-somehow', depending on the context.

#+BEGIN_SRC emacs-lisp
(defun leave-somehow (prefix)
  (interactive "P")
  (save-if-appropriate)
  (let ((buffer (current-buffer)))
    (cond
     ((eq major-mode 'Info-mode)
      (Info-up))
     ((eq major-mode 'help-mode)
      (quit-window))
     ((eq major-mode 'wdired-mode)
      (evil-change-to-previous-state)
      (wdired-finish-edit))
     ((not (eq evil-state 'normal))
      (case evil-state
        (multiedit (evil-multiedit-abort))
        (otherwise (evil-change-to-previous-state))))
     ((minibufferp buffer)
      (minibuffer-keyboard-quit))
     (org-src-mode
      (org-edit-src-exit))
     ((eq major-mode 'dired-mode)
      (dired-up-directory))
     ((buffer-file-name) (find-file "."))
     (t
      (previous-buffer)))
    (when prefix
      (kill-buffer buffer))))
#+END_SRC

** Key Bindings
There are different types of key bindings. Most of them are related to the
name of the underlying function, e.g. `C-x v' for version control, or the
key `b' for buffer related activities. But there are also bindings that
depend only on the physical position of the keys on the keyboard. This is
the case for vim-style motion keys and some key chords. However a single
keyboard can be used with different layouts, like QWERTY or the German Neo
layout. This configuration permits to select different positional bindings
to accommodate for different keyboard layouts.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'key-chord 'evil)

(defvar qwerty-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (key-chord-define map "df" 'evil-window-next)
      (key-chord-define map "jk" 'leave-somehow))))

(defvar neo-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (key-chord-define map "ae" 'evil-window-next)
      (key-chord-define map "nr" 'leave-somehow))))

(define-minor-mode qwerty-mode
  "Enable QWERY specific key chords."
  :global t
  (when qwerty-mode (neo-mode -1)))

(define-minor-mode neo-mode
  "Enable Neo specific key chords."
  :global t
  (when neo-mode (qwerty-mode -1)))

;;; Use the Neo layout, but only on Kinesis keyboards.
(defun detect-kinesis-keyboards ()
  (let ((kinesis-keyboards-p nil))
    (with-temp-buffer
      (with-demoted-errors "Error running xinput: %S"
        (call-process "xinput" nil t))
      (goto-char (point-min))
      (while (re-search-forward "Kinesis Advantage2 Keyboard.+id=\\([0-9]+\\)" nil t)
        (let ((id (match-string 1)))
          (setf kinesis-keyboards-p t)
          (with-demoted-errors "Error running setxkbmap: %S"
            (call-process "setxkbmap" nil nil nil "-device" id "de" "neo"))))
      (if kinesis-keyboards-p
          (neo-mode 1)
        (qwerty-mode 1)))))

(detect-kinesis-keyboards)
#+END_SRC

#+BEGIN_SRC emacs-lisp
(define-key Info-mode-map "n" 'evil-search-next)
(define-key Info-mode-map "N" 'evil-search-previous)
(define-key Info-mode-map "b" 'Info-history-back)
(define-key Info-mode-map "w" 'Info-history-forward)

(define-key dired-mode-map "n" 'evil-search-next)
(define-key dired-mode-map "N" 'evil-search-previous)
(define-key dired-mode-map "a" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map "i" 'wdired-change-to-wdired-mode)

(define-key global-map (kbd "C-c o") 'occur)
(define-key global-map (kbd "C-x g") 'magit-status)
#+END_SRC

** Initial Buffers
A collection of buffers that should be opened as soon as Emacs is
started. The list contains mostly directories.

#+BEGIN_SRC emacs-lisp
(save-excursion
  (find-file-existing "~/.emacs.d/")
  (find-file "~/userdata/*" t)
  (find-file "~/userdata/proj/*" t)
  (find-file "~/Downloads" t))
(setf initial-buffer-choice "~/userdata")
#+END_SRC

** Cleanup the Mode Line with Diminish
There are some modes that are so omnipresent that they deserve no special
mention in the [[info:Emacs#Mode%20Line][Mode Line]]. The small package `diminish' gets rid of them.

#+BEGIN_SRC emacs-lisp
(ensure-packages 'diminish)

(let ((mode-line-bloat
       '(flyspell-mode
         flyspell-prog-mode
         smartparens-mode
         evil-smartparens-mode
         global-whitespace-mode
         company-mode
         rainbow-mode
         yas-minor-mode
         abbrev-mode
         auto-revert-mode
         undo-tree-mode
         grab-and-drag-mode
         reftex-mode
         helm-mode
         org-cdlatex-mode
         org-indent-mode
         eldoc-mode)))
  (dolist (mode mode-line-bloat)
    (ignore-errors (diminish mode))))
#+END_SRC

* Possible Improvements
A list of things that could be improved in this Emacs configuration
- set up and use Gnus
- apply org-drill bug fix
- integrate home-folder restructuring and Linux package setup directly into
  this file
- write command 'higitus-figitus' to pack my home folder
- write command 'sutigif-sutigih' to unpack again
