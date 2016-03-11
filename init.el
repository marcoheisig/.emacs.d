" -*- mode: org; coding: utf-8; no-byte-compile: t; -*-
#+TITLE: Marco Heisig's Emacs configuration
#+EMAIL: marco.heisig@fau.de
#+PROPERTY: header-args:emacs-lisp :results value silent
#+OPTIONS: H:2

This is Marco Heisig's [[http://www.gnu.org/software/emacs/emacs.html][Emacs]] configuration. It is written in a Literal
Programming style and the [[http://www.orgmode.org][Org mode]] is used to manage the individual code
snippets.

* Introduction
This file is divided into several chapters. The first chapter, [[*Meta Configuration][Meta
Configuration]], describes how the configuration itself is loaded and how
missing functionality is obtained with the Emacs package manager. The
chapter [[*Minor Modes][Minor Modes]] enables and configures a plethora of secondary features
for an amazing Emacs experience. The third chapter [[*Major Modes][Major Modes]] contains
configuration sorted by the buffer type it applies to, like the `c-mode'
for operating on files in the C Language. Most human computer interaction
is placed separately in the chapter [[*User%20Interface][User Interface]]. Prominent features of
this chapter are color themes, keybindings, undo and redo, auto completion
and the choice of initial open buffers.

A word of warning -- this configuration file is heavily centered around the
[[https://www.emacswiki.org/emacs/Evil][Evil mode]]. Seasoned Emacs users might be surprised by the Vi-style
keybindings. The author had to switch the layout due to pinky finger
exhaustion. This is probably a sign of being unworthy, certainly not that
the default Emacs keybindings are cumbersome.

If you are not Marco Heisig and plan to use this configuration, some lines
should be adapted accordingly. As a helpful starting point, all lines that
should definitely be reviewed are those containing `marco', `heisig',
`phone' or `crypt-key'.

A final remark -- this configuration is not optimized for load time. It is
therefore strongly recommended to use Emacs as a server, which is as simple
as using the following command to launch a session:

#+BEGIN_SRC sh :eval no
emacsclient -n -c -a ''
#+END_SRC

* Meta Configuration
This chapter deals with the nature of Emacs customization, hence the
`Meta'. It manages paths in the filesystem and utility functions for all other
chapters.

** Loading
These are magic incantations that make this file also a valid Emacs
`init.el' file. They are only interesting for seasoned Emacs Lisp hackers,
others may skip this section. For those curious how it is possible to
`load' this file from Emacs, it may be enlightening to inspect it using
`M-x emacs-lisp-mode'.

#+BEGIN_SRC emacs-lisp :eval no :export no :wrap ?"
(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each
error is of the form (LINE ERRORMESSAGE).")

(defvar init.el-missing-packages '()
  "A list of packages that were demanded during initialization,
  but were not installed.")

(defvar init.el-missing-features '()
  "A list of features that were demanded during initialization,
  but could not be required.")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")

(defmacro with-buckled-seatbelts (&rest body)
  "Useful during Emacs initialization. Catch and all errors and
add them to `init.el-errors'."
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         ,(macroexp-progn body)
       (error
        (push
         (cons init.el-line
               (error-message-string,err))
         init.el-errors)))))

(package-initialize) ;; in particular, load org
(require 'cl-lib) ;; enable Common Lisp features

;; now execute all relevant org-src blocks
(find-file-existing load-file-name)
(let ((org-confirm-babel-evaluate nil)
      (inhibit-redisplay t) ;; less flickering
      (message-log-max nil) ;; silence
      (inhibit-message t)) ;; more silence in Emacs 25+
  (org-babel-map-executables nil
    (unless (looking-at org-babel-lob-one-liner-regexp)
      (setf init.el-line (line-number-at-pos))
      (with-buckled-seatbelts
       (org-babel-execute-src-block)))))
(kill-buffer)

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
the presence of certain packages, features and files. Errors are signalled
when something cannot be ensured.

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

(defun ensure-packages* (&rest required-packages)
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
    (write-region "" nil filename)))

(defun ensure-features* (&rest required-features)
  (let ((missing-features
         (cl-remove-if
          (lambda (x)
            (require x nil t))
          required-features)))
    (when missing-features
      (signal 'feature-error missing-features))))

(defmacro ensure-packages (&rest required-packages)
  `(ensure-packages*
    ,@(mapcar (lambda (x) `',x)
              required-packages)))

(defmacro ensure-features (&rest required-features)
  `(ensure-features*
    ,@(mapcar (lambda (x) `',x)
              required-features)))
#+END_SRC

** The Epilogue Hook
Some things are best run at the end of initialization, even after the
designated Emacs hooks `emacs-startup-hook' and `window-setup-hook'.

#+BEGIN_SRC emacs-lisp
(defvar setup-epilogue-hook '()
  "Hook run after processing all Emacs initialization.")

(add-hook
 'window-setup-hook
 (lambda ()
   (run-at-time
    0.02 nil
    (lambda ()
      (run-hooks 'setup-epilogue-hook)))))

(defmacro init.el-epilogue (&rest body)
  "Have the expressions in BODY evaluated briefly after all Emacs
initialization has finished."
  (when body
    `(add-hook 'setup-epilogue-hook
               (lambda () ,@body)
               t)))
#+END_SRC

** The Emacs Package Manager
Most Emacs utilities can nowadays be obtained via the Emacs package
manager. Other packages can be installed by placing them in the load path. The
following snippet makes all code in the folders `elpa' and `elisp' accessible to
Emacs. It seems the most useful package archives (as of 2016) for Emacs are
Melpa and the Org mode archives.

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
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(update-load-path)
(package-initialize)
#+END_SRC

** Customization
Emacs has a convenient interface for customization, that can be accessed by
the command `M-x customize'. This configuration does not use the customization
facility and performs all its actions via Emacs Lisp code.

In order to avoid interference with custom set variables, the customize
information is stored in another independent file.

#+BEGIN_SRC emacs-lisp
(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)
#+END_SRC

** Enhancing Color Themes
In Emacs terminology, rendering attributes of each character are called
[[info:Elisp#faces][Faces]]. Traditionally a color theme defines the appearance of each
face. However hardly any color theme is exhaustive and sets colors like
`rainbow-delimiters-depth-9-face' or `org-block-begin-line'. The following
code derives sane values for such faces automatically. As a result, one can
load any color theme and get a consistent experience.

#+BEGIN_SRC emacs-lisp
(ensure-packages rainbow-delimiters org dired+)

(defun derive-faces (&rest args)
  (cl-labels
      ((hsl (color-name)
            (apply #'color-rgb-to-hsl
                   (color-name-to-rgb color-name)))
       (hue-clamp (value)
                  (- value (floor value)))
       (merge-hsl
        (wh1 ws1 wl1 color1 wh2 ws2 wl2 color2)
        (cl-multiple-value-bind (h1 s1 l1) (hsl color1)
          (cl-multiple-value-bind (h2 s2 l2) (hsl color2)
            (apply
             #'color-rgb-to-hex
             (color-hsl-to-rgb
              (hue-clamp   (+ (* wh1 h1) (* wh2 h2)))
              (color-clamp (+ (* ws1 s1) (* ws2 s2)))
              (color-clamp (+ (* wl1 l1) (* wl2 l2))))))))
       (respec (face &rest arguments)
               (when (member face (face-list))
                 (face-spec-reset-face face)
                 (apply #'set-face-attribute
                        face nil
                        arguments))))
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
      (respec 'org-block-begin-line
              :background block-bg
              :foreground comment-fg)
      (respec 'org-block-end-line
              :inherit 'org-block-begin-line)
      (respec 'org-block
              :background (merge-hsl
                           0.0 0.3 0.3 default-bg
                           1.0 0.7 0.7 block-bg)
              :inherit 'unspecified)

      ;; Make rainbow-delimiters actually a rainbow
      (dotimes (i 9)
        (let ((face
               (intern
                (format "rainbow-delimiters-depth-%d-face"
                        (+ i 1)))))
          (respec face
                  :foreground
                  (merge-hsl
                   1.0 1.0 1.0 default-fg
                   (+ 0.5 (* i 0.166)) 0.8 0.0 "#ff00ff"))))

      ;; dired+ faces
      (respec 'diredp-compressed-file-suffix :inherit 'font-lock-comment-face)
      (respec 'diredp-file-suffix :inherit 'font-lock-comment-face)
      (respec 'diredp-date-time :inherit 'font-lock-comment-face)
      (respec 'diredp-dir-heading :inherit 'org-block-begin-line)
      (respec 'diredp-dir-name :inherit 'dired-directory)
      (respec 'diredp-symlink :inherit 'font-lock-constant-face)
      (respec 'diredp-file-name :inherit 'default)
      (respec 'diredp-ignored-file-name :inherit 'font-lock-comment-face)
      (respec 'diredp-number :inherit 'font-lock-constant-face)
      (respec 'diredp-flag-mark :inherit 'highlight)
      (respec 'diredp-flag-mark-line :inherit 'highlight)
      (respec 'diredp-deletion :inherit 'warning)
      (respec 'diredp-deletion-file-name :inherit 'warning)
      (respec 'diredp-dir-priv :inherit 'dired-directory)
      (respec 'diredp-read-priv :inherit 'rainbow-delimiters-depth-1-face)
      (respec 'diredp-write-priv :inherit 'rainbow-delimiters-depth-2-face)
      (respec 'diredp-exec-priv :inherit 'rainbow-delimiters-depth-3-face)
      (respec 'diredp-link-priv :inherit 'default)
      (respec 'diredp-rare-priv :inherit 'default)

      ;; regexp-grouping constructs should have the same color as the
      ;; `font-lock-string-face', but different emphasis.
      (respec 'font-lock-regexp-grouping-backslash
              :foreground (merge-hsl
                           0.0 0.5 0.5 default-bg
                           1.0 0.5 0.5 string-fg))

      (respec 'font-lock-regexp-grouping-construct
              :foreground (merge-hsl
                           0.0 0.5 0.75 default-bg
                           1.0 0.5 0.75 string-fg)
              :weight 'bold)

      ;; run derive-faces after every usage of `load-theme'
      (advice-add 'load-theme :after #'derive-faces))))

(init.el-epilogue
 (derive-faces))
#+END_SRC

* Minor Modes
[[info:Emacs#Minor%20Modes][Minor Modes]] add a variety of secondary features to currently edited
buffers. Any numberr of minor modes can be active at a time.
** Undo Tree Mode
While the default Emacs undo mechanism never forgets past modifications,
moving to an old state requires moving through the whole history in
between. For multiple branches of history, this leads to longer and longer
undo chains. The undo tree mode brings speed and clarity in this respect by
showing the true nature of the undo history as a tree with suitable
navigation commands.

#+BEGIN_SRC emacs-lisp
(ensure-packages undo-tree)
(setf undo-tree-visualizer-timestamps t)
(setf undo-tree-visualizer-diff t)
#+END_SRC

** The Evil Mode
The [[info:evil][Evil Mode]] is the most sophisticated Vi emulation for Emacs, and this
section describes how to set it up.

#+BEGIN_SRC emacs-lisp
(ensure-packages evil)
(setf evil-want-C-w-in-emacs-state t)
(setf evil-echo-state nil)
(setf evil-cjk-emacs-word-boundary t)
(setf evil-want-C-i-jump nil)
(setf evil-want-C-w-delete nil)

(defun enable-evil-motion-state ()
  "Useful for major mode hooks to evable evil motion state
unconditionally."
  (evil-motion-state 1))

(setf evil-emacs-state-tag " Ⓔ ")
(setf evil-normal-state-tag " Ⓝ ")
(setf evil-insert-state-tag " Ⓘ ")
(setf evil-motion-state-tag " Ⓜ ")
(setf evil-multiedit-insert-state-tag " ∀Ⓘ ")
(setf evil-multiedit-state-tag " ∀Ⓝ ")
(setf evil-operator-state-tag " Ⓞ ")
(setf evil-visual-state-tag " Ⓥ ")
(evil-mode 1)
#+END_SRC

** Recording Emacs sessions with Camcorder
Some Emacs features are better explained with a short demonstration video
than many words. The camcorder package allows to record Emacs sessions in
many video formats.

#+BEGIN_SRC emacs-lisp
(ensure-packages camcorder)
(define-key global-map (kbd "<f12>")
  'camcorder-mode)
#+END_SRC

** Monitoring the Battery
Emacs provides a robust battery monitoring facility. This config adds a
unicode battery Symbol and the charge percentage to the mode line.

#+BEGIN_SRC emacs-lisp
(ensure-packages battery)
(setf battery-mode-line-format "🔋 %p%%%%")
(setf battery-update-interval 5)
(display-battery-mode 1)
#+END_SRC

** The Insidious Big Brother Database for Emacs
BBDB is a great address database written im Emacs Lisp.

#+BEGIN_SRC emacs-lisp
(ensure-packages bbdb)
(setf bbdb-default-country "Germany"
      bbdb-file "~/.emacs.d/bbdb"
      bbdb-gui t
      bbdb-north-american-phone-numbers-p nil)
(bbdb-initialize)
#+END_SRC

** Automatical Text Completion with Company
When enabled, Company displays possible completion candidates for
individual words. This is particularly useful in programming modes, where
the completions include defined functions and variables.

#+BEGIN_SRC emacs-lisp
(ensure-packages company)
(setf company-idle-delay 0.02)
(setf company-minimum-prefix-length 1)

(defun enable-company-mode ()
  (company-mode 1))

(add-hook 'lisp-mode-hook 'enable-company-mode)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))
#+END_SRC

** Multiple Cursors
A convenient feature, especially when it comes to renaming multiple
occurences of a variable in source code. In its simplest form, it suffices
to mark a word and press `R' to edit all its occurences at the same time.

#+BEGIN_SRC emacs-lisp
(ensure-packages evil-multiedit iedit)
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
(ensure-packages openwith)
(openwith-mode)
(setf openwith-associations
      ;; note: no openwith-opening of .ps files or imaxima misbehaves
      '(("\\.\\(?:dvi\\|pdf\\|ps.gz\\|djvu\\)\\'"
         "evince" (file))
        ("\\.jar\\'"
         "java -jar" (file))
        ("\\.\\(?:odt\\|odt\\|fodt\\|uot\\|docx\\|docx\\)\\'"
         "libreoffice" (file))
        ("\\.xcf\\'"
         "gimp" (file))
        ("\\.\\(?:mp3\\|wav\\|mp4\\|mpe?g\\|avi\\|flac\\|ogg\\|wma\\|m4a\\|mkv\\)\\'"
         "vlc" (file))))
#+END_SRC

** Incremental Completion with Helm
#+BEGIN_SRC emacs-lisp
(ensure-packages helm)
(ensure-features helm-config)
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
(helm-mode -1)
#+END_SRC

** Regular Expression Building
A mode for interactively building regular expressions and viewing their
effect on the selected buffer. Mostly made obsolete by the Evil mode search
and replace facility, but sometimes useful for complex regular expressions
with multiple grouping constructs.

#+BEGIN_SRC emacs-lisp
(ensure-features re-builder)
(setf reb-re-syntax 'read)
#+END_SRC

** Bibliographic References with Reftex

#+BEGIN_SRC emacs-lisp
(ensure-packages reftex)
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
(define-key org-mode-map (kbd "C-c r") 'reftex-citation)
(add-hook 'org-mode-hook 'org-mode-reftex-setup)
(add-hook 'org-mode-hook 'visual-line-mode)
#+END_SRC

** Image viewing with Emacs
Emacs can open images but does not rescale them to fit to the buffer. The
`image+' library scales pictures accordingly.

#+BEGIN_SRC emacs-lisp
(ensure-packages image+)
(imagex-global-sticky-mode)
(imagex-auto-adjust-mode)
#+END_SRC

** Gracefully manage matching Parentheses with Paredit
Paredit is what separates happy Lisp programmers from those crying about
superfluous parentheses.

#+BEGIN_SRC emacs-lisp
(ensure-packages paredit evil-paredit)
(defun enable-evil-paredit-mode ()
  (evil-paredit-mode 1))
#+END_SRC

* Major Modes
** The Org Mode
If Emacs is the place a programmer lives when using his computer, the [[info:org][Org mode]]
is likely to be his living room. At its core it is a mode for writing
structured plaintext, but its many extensions allow it to blur the line
between organizing, note taking and programming in amazing ways.

*** Basics
#+BEGIN_SRC emacs-lisp
(ensure-packages org-plus-contrib)
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
(setcdr (assoc-string "\\.pdf\\'" org-file-apps) "evince %s")
(setq-default org-tag-alist
              '(("crypt" . ?c)
                ("drill" . ?d)))
(add-hook 'org-mode-hook 'org-indent-mode)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ce" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
#+END_SRC

Finally there is this little hack for full fontification of long Org mode
buffers. It is not clear (as of 2016) whether this is still an issue.

#+BEGIN_SRC emacs-lisp
(setf jit-lock-chunk-size 10000)
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
(ensure-packages cdlatex org htmlize)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setf org-latex-create-formula-image-program 'imagemagick)
(setf org-format-latex-options
      (plist-put org-format-latex-options :scale 1.6))
(setf org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setf org-latex-minted-options
      '(("frame" "single") ("framesep" "6pt")
        ("mathescape" "true") ("fontsize" "\\footnotesize")))
(setq
 org-latex-pdf-process
 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "bibtex $(basename %b)"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq
 LaTeX-command-style
 '((""
    "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
#+END_SRC

*** Managing source code with Org Babel

#+BEGIN_SRC emacs-lisp
(ensure-packages org)
(setf org-edit-src-content-indentation 0)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((asymptote . t)
   (awk . t)
   (calc . t)
   (C . t)
   (clojure . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . t)
   (java . t)
   (js . t)
   (latex . t)
   (lisp . t)
   (lilypond . t)
   (maxima . t)
   (ocaml . t)
   (octave . t)
   (org . t)
   (perl . t)
   (python . t)
   (ruby . t)
   (scheme . t)
   (screen . t)
   (sh . t)))
(setf org-src-preserve-indentation nil)
(setf org-src-tab-acts-natively t)
(setf org-src-window-setup 'other-window)
(setq-default org-export-babel-evaluate 'inline-only)
#+END_SRC

*** Encrypting parts of a buffer with Org Crypt
#+BEGIN_SRC emacs-lisp
(ensure-features org-crypt)
(setf auto-save-default nil)
(org-crypt-use-before-save-magic)
(setf org-tags-exclude-from-inheritance '("crypt"))
(setf org-crypt-key "05369722")
#+END_SRC

*** Beautiful Presentations with Org Reveal

#+BEGIN_SRC emacs-lisp
(ensure-packages ox-reveal)
(setq
 org-reveal-root
 "file:///home/marco/.emacs.d/elpa/ox-reveal-2015022.2306/reveal.js")
#+END_SRC

*** Efficient Learning with Org drill
Org drill is an amazing tool to learn new facts. In a first step, one creates
drill cards, which are nothing more than org subtrees with some metadata and the
`:drill:' tag. Afterwards the command `org-drill' will start a sophisticated
drill session.

#+BEGIN_SRC emacs-lisp
(ensure-packages org-plus-contrib)
(ensure-features org-drill)
;; prevent drill hints from ruining Latex formulas
(setf org-drill-hint-separator "||HINT||")
#+END_SRC

** Latex Editing with Auctex
Auctex is by far the best Latex editing environment on the planet, only
surpassed by the Org mode Latex export facility and `cdlatex'.

#+BEGIN_SRC emacs-lisp
(ensure-packages auctex company-auctex)
(ensure-features latex)
(setq-default TeX-PDF-mode t)
(company-auctex-init)
#+END_SRC

** Directory Browsing with Dired
Dired is the directory browser in Emacs. There are numerous little tweaks
to enhance the dired usability. One of them is simply to activate dired+.
Another one is to enable recursive copies and enable
`dired-dwim-target'. The latter allowes to copy and move whole folders
between adjacent [[info:Emacs#Windows][Emacs windows]].

#+BEGIN_SRC emacs-lisp
(ensure-features dired)
(setf dired-dwim-target t
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      dired-listing-switches "-ahl"
      dired-auto-revert-buffer t
      wdired-allow-to-change-permissions 'advanced)
#+END_SRC

#+BEGIN_SRC emacs-lisp
(ensure-packages dired+)
(global-dired-hide-details-mode 1)
#+END_SRC

Dired narrow is a handy tool to filter the files in a dired buffer.
#+BEGIN_SRC emacs-lisp
(ensure-packages dired-narrow)
(define-key dired-mode-map (kbd "C-x /") 'dired-narrow)
#+END_SRC

Sometimes one wishes to perform quick conversions from one file type to another,
preferably for all currently marked files in a dired buffer. The function
`dired-convert' attempts to perform such a conversion with several
heuristics. The command is strictly more useful than `dired-compress' and is
therefore bound to the key `Z' instead.

#+BEGIN_SRC emacs-lisp
(defgroup dired-convert nil
  "Quick and dirty file conversions/extractions."
  :prefix "dired-convert-"
  :group 'dired)

(defcustom dired-convert-extraction-associations
  '(("\\.\\(?:gz\\|tgz\\|bz2\\|xz\\|zip\\|rar\\)\\'"
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

(defcustom dired-convert-conversion-associations
  '(("\\.\\(?:mp3\\|ogg\\|wma\\|flac\\|wav\\|m4a\\)\\'"
     "ffmpeg" "-i" src "-ab" "192K" "-vn" dst)
    ("\\.\\(?:flv\\|mov\\|mp4\\|webm\\|mkv\\)\\'"
     "ffmpeg" "-i" src "-ab" "192K" dst)
    ("\\.\\(?:png\\|tga\\|bmp\\|jpeg\\|jpg\\|gif\\)\\'"
     "convert" src dst)
    ("\\.\\(?:gz\\|tgz\\|bz2\\|xz\\|zip\\|rar\\|z\\)\\'"
     "7z" "a" dst src))
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
                (message cmd) ;; be verbose
                (shell-command cmd))))
          ;; make newly created files appear in dired immediately
          (revert-buffer)
          (redisplay))))))

(define-key dired-mode-map (kbd "Z") 'dired-convert)
#+END_SRC

** The C Langugage Family
[[info:ccmode#Top][cc-mode]] is a GNU Emacs mode for editing files containing C, C++,
Objective-C, Java, CORBA IDL (and the variants PSDL and CIDL), Pike and
AWK code.

#+BEGIN_SRC emacs-lisp
(ensure-features cc-mode)
(add-to-list 'auto-mode-alist `("\\.cl\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.frag\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.vert\\'" . c-mode))
(add-to-list 'auto-mode-alist `("\\.jad\\'" . java-mode))
(setf c-basic-offset 4
      c-hanging-braces-alist (quote set-from-style)
      c-offsets-alist (quote ((innamespace . 0))))
#+END_SRC

The Language C++ is the first to my knowledge where Emacs stutters with maximal
syntax highlighting. Unfortunately it is not possible to change the ridiculously
complex syntax of C++ (current standardization efforts are even going in the
opposite direction), so as an alternative the font lock decoration level is
lowered.

#+BEGIN_SRC emacs-lisp
(setf font-lock-maximum-decoration (quote ((c++-mode . 2) (t . t))))
#+END_SRC

** Common Lisp
The best programming language on the planet.

#+BEGIN_SRC emacs-lisp
(ensure-packages slime slime-company rainbow-delimiters evil-paredit paredit)
(setf inferior-lisp-program "sbcl")
(slime-setup
 '(slime-fancy
   slime-sbcl-exts
   slime-cl-indent
   slime-sprof
   slime-asdf
   slime-fancy-inspector
   slime-company
   ;;slime-fuzzy
   slime-autodoc))

(setq
 common-lisp-hyperspec-root
 "file:/home/marco/userdata/literature/cs/lisp/Common Lisp Hyperspec/")
(global-set-key "\C-cs" 'slime-selector)
(global-set-key "\C-ch" 'common-lisp-hyperspec)

(define-key slime-mode-map
  (kbd "C-c m") 'slime-macroexpand-1)

(add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-mode-hook 'enable-paredit-mode)
(add-hook 'slime-mode-hook 'enable-evil-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

;; workaround for paredit on the slime REPL
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook
          'override-slime-repl-bindings-with-paredit)

'override-slime-repl-bindings-with-paredit
#+END_SRC

** Emacs Lisp
The language Emacs Lisp is a fine blend of Maclisp, Common Lisp and some
language for editing text. Unsurprisingly Emacs is well suited for editing
Emacs Lisp. The only worthwile addition provided here is a simple Macro
stepper called `macroexpand-point'.

#+BEGIN_SRC emacs-lisp
  (define-derived-mode emacs-lisp-macroexpand-mode emacs-lisp-mode
    "Macro Expansion"
    "Major mode for displaying Emacs Lisp macro expansions."
    (setf buffer-read-only t))

  (define-key emacs-lisp-mode-map
    (kbd "C-c m") 'macroexpand-point)

  (define-key org-mode-map
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

  (ensure-packages paredit evil-paredit company rainbow-mode rainbow-delimiters)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-evil-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
#+END_SRC

** Maxima
#+BEGIN_SRC emacs-lisp
(ensure-features maxima)
(add-to-list 'auto-mode-alist `("\\.mac\\'" . maxima-mode))

(ensure-features imaxima)
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
(ensure-packages geiser paredit evil-paredit company rainbow-delimiters)
(setf geiser-default-implementation "guile")
(setf scheme-program-name "guile")
(add-to-list 'auto-mode-alist `("\\.sc\\'". scheme-mode))
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-evil-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-company-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
#+END_SRC

** Octave like languages
There is a whole family of programming toolkits for applied mathematics,
all with similar syntax as Octave.
#+BEGIN_SRC emacs-lisp
(ensure-features octave)
(add-to-list 'auto-mode-alist `("\\.sci\\'". octave-mode))
(add-to-list 'auto-mode-alist `("\\.m\\'". octave-mode))
#+END_SRC

** Proof General
Proof General is an Emacs frontend for various Theorem Provers.
#+BEGIN_SRC emacs-lisp
(load-file "~/.emacs.d/elisp/ProofGeneral-4.2/generic/proof-site.el")

;; show-paren-mode does not interact well with the Proof General
(add-hook 'proof-ready-for-assistant-hook
          (lambda () (show-paren-mode 0)))
#+END_SRC

** EAP - Music Without Jolts
#+BEGIN_SRC emacs-lisp
(ensure-features eap)
(make-directory "~/.emacs.d/eap-playlists" t)
(make-directory "~/userdata/music" t)

(setf eap-music-library
      "~/userdata/music")
(setf eap-playlist-library
      "~/.emacs.d/eap-playlists")

(setf eap-volume-mute 0.00)

(setf eap-volume-fade-out-flag nil)

(add-hook 'kill-buffer-hook
          'eap-always-kill-buffer-cleanly)
(add-hook 'kill-buffer-hook
          'eap-always-kill-buffer-cleanly)
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
(setf browse-url-browser-function 'browse-url-chromium)
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
#+END_SRC

Emacs wizards have memoized all their commands and have no need for visual
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

There are two major annoyances in an uncofigured Emacs. First, killing a
buffer with an attached process asks for confirmation every single
time. Second, a prompt for interactive confirmation requires the user to type
`y e s RET' instead of a simple `y'. The next code fixes both these issues.

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
whitespace.

#+BEGIN_SRC emacs-lisp
(setf whitespace-style '(face trailing tab-mark))
(global-whitespace-mode)
#+END_SRC

Another thing that has probably scared away thousands of potential Emacs
users -- the automatic backup files. As soon as an Emacs user gets his
hands on a file, it clobbers the directory with a backup file with a
tilde. This configuration disables auto saving by default. If autosaving is
activated, it places the files in a temporary directory. There are two
reasons for turning off auto saving by default: The first one is that if a
file really matters, one should use a versioning tool like Git and some
hardware backup solution. The second, more profane one is that the
org-crypt mode does not work well with auto-save.

#+BEGIN_SRC emacs-lisp
(setf auto-save-default nil
      auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
      backup-directory-alist (quote (("." . "~/.emacs.d/saves")))
      backup-inhibited nil)
#+END_SRC

** The Color Theme and Mode Line
This chapter deals with the visual appearance of Emacs. Interested readers
might want to read the section [[info:Elisp#Display][Display]] of the Emacs Lisp manual.

#+BEGIN_SRC emacs-lisp
(ensure-packages zenburn-theme)
(load-theme 'zenburn t)
(set-face-attribute 'button nil :inherit 'link)

(ensure-packages spacemacs-theme)
;; The spacemacs theme tries to detect whether to give the full color
;; theme or a restricted version. This fails for the Emacs server, so I
;; disable this check
(defun always-true (&rest args) t)

(defun load-spacemacs-theme (&optional frame)
  (advice-add 'true-color-p :around #'always-true)
  (load-theme 'spacemacs-dark t)
  (advice-remove 'true-color-p #'always-true))
#+END_SRC

The package `powerline' and its derivative `spaceline' make the Emacs mode
line more beautiful.

#+BEGIN_SRC emacs-lisp
(ensure-packages spaceline)
(ensure-features spaceline-config)
(setf powerline-default-separator 'wave)
(spaceline-spacemacs-theme)
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

** Keybindings
The Evil keybindings are great, but there are some cases where they fall
short. The first annoyance is the use of the `ESC' key to quit a mode. A
useful alternative to `ESC' that is fully on a keyboards home row is to use a
key chord, that is two keys pressed at almost the same time. The most
convenient key chord is `jk'.

#+BEGIN_SRC emacs-lisp
(ensure-packages key-chord)
(key-chord-mode 1)
(setf key-chord-two-keys-delay 0.05)
(setf key-chord-one-key-delay 0.14)

(key-chord-define global-map
                  "jk" 'quit-window)

(key-chord-define minibuffer-local-map
                  "jk" 'minibuffer-keyboard-quit)

(key-chord-define minibuffer-local-ns-map
                  "jk" 'minibuffer-keyboard-quit)

(key-chord-define minibuffer-local-completion-map
                  "jk" 'minibuffer-keyboard-quit)

(key-chord-define minibuffer-local-must-match-map
                  "jk" 'minibuffer-keyboard-quit)

(key-chord-define minibuffer-local-isearch-map
                  "jk" 'minibuffer-keyboard-quit)

(key-chord-define evil-multiedit-state-map
                  "jk" 'evil-multiedit-abort)

(key-chord-define evil-multiedit-insert-state-map
                  "jk" 'evil-multiedit-state)

(key-chord-define evil-visual-state-map
                  "jk" 'evil-change-to-previous-state)

(key-chord-define evil-insert-state-map
                  "jk" 'evil-normal-state)

(key-chord-define evil-replace-state-map
                  "jk" 'evil-normal-state)
#+END_SRC

The `jk' keystroke can do even more -- when Evil is already in normal mode,
leave the current buffer somehow.

#+BEGIN_SRC emacs-lisp
(defun leave-buffer (prefix)
  (interactive "P")
  (cond
   (org-src-mode
    (org-edit-src-exit))
   ((eq major-mode 'dired-mode)
    (when prefix (quit-window t))
    (dired-up-directory))
   ((buffer-file-name)
    (when prefix (quit-window t))
    (find-file "."))
   (t
    (quit-window prefix))))

(key-chord-define evil-normal-state-map "jk" 'leave-buffer)
#+END_SRC

No bind the key `U' to the awesome Undo-Tree mode, `SPC' to the Ace Jump Word
command that queries for a character and jumps to a matching word and `C-c o' to
the `occur' command.

#+BEGIN_SRC emacs-lisp
(ensure-packages ace-jump-mode undo-tree)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-c o") 'occur)
#+END_SRC

Now a little gem -- `save-if-appropriate'. This function executes a bunch of
heuristics to determine whether now is a good moment to save the current
buffer. Actually these heuristics work so good, that one can bind this command
to the very frequently used `evil-normal-state-entry-hook'.

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

There are still some Emacs modes around that do not play well with Evil mode. It
would be more convenient if every mode would follow the following rules:
- The keys `hjkl' move the cursor
- The key chord `jk' moves upward in some hirarchy or closes a temporary buffer
- The keys `/', `n' and `N' can be used for searching
- The keys `b', `B', `w', `W', `e', `E', `t' and `T' produce some meaningful
  forward and backward motion.

Now come some humble attempts to make Emacs even more evil.

#+BEGIN_SRC emacs-lisp
(key-chord-define Info-mode-map "jk" 'Info-up)
(define-key Info-mode-map "n" 'evil-search-next)
(define-key Info-mode-map "N" 'evil-search-previous)
(define-key Info-mode-map "b" 'Info-history-back)
(define-key Info-mode-map "w" 'Info-history-forward)

(define-key dired-mode-map "n" 'evil-search-next)
(define-key dired-mode-map "N" 'evil-search-previous)

(add-hook 'help-mode-hook 'enable-evil-motion-state)
(add-hook 'package-menu-mode-hook 'enable-evil-motion-state)
#+END_SRC

** Initial Buffers
A collection of buffers that should be opened as soon as Emacs is
started. The list contains mostly directories.

#+BEGIN_SRC emacs-lisp
(save-excursion
  (find-file-existing "~/.emacs.d/")
  (find-file "~/userdata/gaming/*" t)
  (find-file "~/userdata/*" t)
  (find-file "~/userdata/proj/*" t)
  (find-file "~/userdata/events/*" t))
(setf initial-buffer-choice "~/userdata")
#+END_SRC

** Cleanup the Mode Line with Diminish
There are some modes that are so omnipresent that they deserve no special
mention in the [[info:Emacs#Mode%20Line][Mode Line]]. The small package `diminish' gets rid of them.

#+BEGIN_SRC emacs-lisp
(ensure-packages diminish)
(diminish 'paredit-mode)
(diminish 'evil-paredit-mode)
(diminish 'global-whitespace-mode)
(diminish 'company-mode)
(diminish 'rainbow-mode)
(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'grab-and-drag-mode)
(diminish 'reftex-mode)
(diminish 'org-cdlatex-mode)
(diminish 'org-indent-mode)
(diminish 'eldoc-mode)
#+END_SRC

** Finalization
After all initialization is complete, display a nice summary whether the
init file was loaded successfully and if not, what went wrong.

#+BEGIN_SRC emacs-lisp
(setf inhibit-startup-screen t)
(advice-add 'display-startup-echo-area-message
            :override #'ignore)

(init.el-epilogue
 ;; apply powerline and drop spam
 (kill-buffer "*Messages*")
 (cond
  (init.el-missing-packages
   (let ((use-dialog-box nil))
     (when (yes-or-no-p
            (format
             "Install missing packages: %s "
             init.el-missing-packages))
       (mapc #'package-install
             init.el-missing-packages)))
   (message
    "Missing packages are installed, please restart Emacs."))
  (init.el-errors
    (message
     "There have been %d error(s) during init:\n%s"
     (length init.el-errors)
     (mapconcat
      (lambda (init.el-error)
        (pcase-let ((`(,line . ,msg) init.el-error))
          (format "Lines %d+: %s" line msg)))
      init.el-errors
      "\n")))
  (t
   (message "Initialization successful - happy hacking."))))

'done
#+END_SRC

* Possible Improvements
A list of things that could be improved in this Emacs config
*** TODO The `M-.' is shadowed by `evil-repeat-pop-next', but should jump to a Lisp definition.
Probably also some other Evil features could be added in SLIME
*** TODO configure and use doc-view mode
Probably impossible to make doc-view mode as good as evince, but worth a
try. Maybe I should wait for Emacs Xwidgets support...
*** TODO proofgeneral and show-paren
For magic reasons, it is not possible to activate show-paren-mode in
proofgeneral.
*** TODO enable company in more modes
There are many modes that would profit from company completion and do not
at the moment.
*** TODO review auto saving
Probably it would be useful to re-enable auto-save in some way
*** TODO borrow spacemacs config
Especially the major mode setup and helm
*** TODO whitespace mode not enabled everywhere
*** TODO add dedicated command `jk-magic'
*** TODO derive faces once the first graphical Emacsclient starts
*** TODO set up and use Gnus
