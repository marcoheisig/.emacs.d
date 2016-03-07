" -*- mode: org; coding: utf-8; no-byte-compile: t; -*-
#+TITLE: Marco Heisig's Emacs configuration
#+EMAIL: marco.heisig@fau.de
#+PROPERTY: header-args:emacs-lisp :results value silent
#+OPTIONS: H:2

This is Marco Heisig's [[http://www.gnu.org/software/emacs/emacs.html][Emacs]] configuration. It is written in a Literal
Programming style and the [[http://www.orgmode.org][Org mode]] is used to manage the individual code
snippets.

* Introduction
** Making this file loadable
The first few lines of this file contain magic incantations that make it
also a valid Emacs `init.el' file. They are only interesting for seasoned
Emacs Lisp hackers, and are therefore not exported when formatting this
document with the Org mode.

#+BEGIN_SRC emacs-lisp :eval no :export no :wrap ?"
(defvar init.el-errors '()
  "A list of errors that occured during initialization. Each
error is of the form (LINE ERROR &rest ARGS).")

(defvar init.el-line 0
  "Approximation to the currently executed line in this file.")

(defmacro with-buckled-seatbelts (&rest body)
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         ,(macroexp-progn body)
       (error
        (push (cons init.el-line ,err)
              init.el-errors)))))

(package-initialize)
(find-file-existing load-file-name)
(let ((org-confirm-babel-evaluate nil)
      (inhibit-redisplay t)) ; less flickering
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

** Introduction
The rest of this configuration file is divided into several chapters. The
first chapter, [[*Meta Configuration][Meta Configuration]], describes how the configuration itself
is loaded and how missing functionality is obtained via the Emacs package
manager. The chapter [[*Minor Modes][Minor Modes]] enables and configures a plethora of
secondary features for an amazing Emacs experience. The third chapter [[*Major Modes][Major
Modes]] contains configuration sorted by the buffer type it applies to, like
the `c-mode' for operating on files in the C Language. Many of those Major
Modes also enable several Minor Modes. Most human computer interaction is
placed separately in the chapter [[*User%20Interface][User Interface]]. Prominent features of this
chapter are color themes, keybindings, undo and redo, auto completion and
the choice of initial open buffers.

A word of warning -- this configuration file is heavily centered around the
[[https://www.emacswiki.org/emacs/Evil][Evil mode]]. Seasoned Emacs users might be surprised by the Vi-style
keybindings. The author had to switch the layout due to pinky finger
exhaustion. This is probably a sign of being unworthy, certainly not that
the default Emacs keybindings are cumbersome.

If you are not Marco Heisig and plan to use this configuration, some lines
should be adapted accordingly. As a helpful starting point, all lines that
should definitely be reviewed are listed by the following command (assuming
you are reading this file from Emacs)

#+BEGIN_SRC emacs-lisp :eval no
;; place cursor after the final `)' and press C-x C-e to evaluate
(occur "\\(marco\\|heisig\\|org-crypt-key\\|country\\|phone\\)")
#+END_SRC

A final remark -- this configuration is not optimized for load time. It is
therefore strongly recommended to use Emacs as a server, which is as simple
as using the following command to launch a session:

#+BEGIN_SRC sh :eval no
emacsclient -n -c -a ""
#+END_SRC

* Meta Configuration
This chapter deals with the nature of Emacs customization, hence the
`Meta'. It manages paths in the filesystem and utility functions for all other
chapters.

In particular, some utility functions will be presented that facilitate the
notation in the later chapters. It would not be Emacs if the most important
utilities were not already in place. The following code enables the [[info:CL][CL]]
package that provides Common Lisp compatibility form Emacs.

#+BEGIN_SRC emacs-lisp
(require 'cl-lib)
#+END_SRC

** The `setup' macro
The macro described here is similar to the `use-package' macro provided by
John Wiegley, but deliberatily simpler to fit into this configuration
file. It can ensure the presence of a package, apply configuration and
handle all errors that arise.

#+BEGIN_SRC emacs-lisp
(defun setup-ensure (package arg)
  (when (and (not (ignore-errors (require package)))
             arg)
    (cl-typecase arg
     (string
      (save-excursion
        (find-file-existing "~/.emacs.d/elisp/")
        (shell-command arg)
        (normal-top-level-add-subdirs-to-load-path)
        (require package)))
     (t
      (package-install package)))))

(defun setup-mode-alist (mode regexps)
  (dolist (regexp regexps)
    (add-to-list 'auto-mode-alist
                 `(,regexp . ,mode))))

(defmacro setup (name &rest args)
  "Configure and load a single Emacs package or item. Usage:

  (setup package-name
     (:keyword expr ...) ...)

:init    EXPR ...   -- Code to run before PACKAGE-NAME has been loaded
:config  EXPR ...   -- Code to run after PACKAGE-NAME has been loaded
:mode    REGEXP ... -- Add (REGEXP . PACKAGE-NAME) to `auto-mode-alist'
:ensure  BOOLEAN    -- Loads the package using `package-install' if necessary
:ensure  STRING     -- Execute STRING with `shell-command' in ~/.emacs.d/elisp/
"
  (with-buckled-seatbelts
    (cl-assert (cl-every (lambda (x) (keywordp (car x))) args) t)
    (cl-assert (symbolp name))
    (let ((init-forms   (cdr (assoc :init args)))
          (config-forms (cdr (assoc :config args)))
          (ensure-p     (cadr (assoc :ensure args)))
          (mode-regexps (cdr (assoc :mode args))))
      `(with-buckled-seatbelts
         ,@init-forms
         (setup-ensure ',name ',ensure-p)
         (setup-mode-alist ',name ',mode-regexps)
         ,@config-forms
         ',name))))

(put 'setup 'lisp-indent-function 'defun)
#+END_SRC

** The Emacs Package Manager
Most Emacs utilities can nowadays be obtained via the Emacs package
manager. Other packages can be installed by placing them in the load path. The
following snippet makes all code in the folders `elpa' and `elisp' accessible to
Emacs. It seems the most useful package archives (as of 2016) for Emacs are
Melpa and the Org mode archives.

#+BEGIN_SRC emacs-lisp
(setup package
  (:config
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
   (package-initialize)))
#+END_SRC

** Customization
Emacs has a convenient interface for customization, that can be accessed by
the command `M-x customize'. This configuration does not use the customization
facility and performs all its actions via Emacs Lisp code.

In order to avoid interference with custom set variables, the customize
information is stored in another independent file.

#+BEGIN_SRC emacs-lisp
(setup custom
  (:config
   (setf custom-file "~/.emacs.d/custom.el")
   (load custom-file)))
#+END_SRC

** Working with color themes
In Emacs terminology, rendering attributes of each character are called
[[info:Elisp#faces][Faces]]. Traditionally a color theme defines the appearance of each
face. However hardly any color theme is exhaustive and sets colors like
`rainbow-delimiters-depth-9-face' or `org-block-begin-line'. The following
code derives sane values for such faces automatically. As a result, one can
load any color theme and get a consistent experience.

#+BEGIN_SRC emacs-lisp
(setup derived-faces
  (:config

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

         (advice-add 'load-theme :after #'derive-faces))))

   (add-hook 'emacs-startup-hook
             (lambda ()
               ;; wait for face initialization
               (run-at-time 0.1 nil #'derive-faces)))))
#+END_SRC

* Minor Modes
The bulk of features within Emacs is provided by different [[info:Elisp#Modes][Modes]]. This chapter
will describe the use and customization of each individual mode.
** Undo Tree Mode
While the default Emacs undo mechanism never forgets past modifications,
moving to an old state requires moving through the whole history in
between. For multiple branches of history, this leads to longer and longer
undo chains. The undo tree mode brings speed and clarity in this respect by
showing the true nature of the undo history as a tree with suitable
navigation commands.
#+BEGIN_SRC emacs-lisp
(setup undo-tree
  (:ensure t)
  (:config
   (setf undo-tree-visualizer-timestamps t)))
#+END_SRC

** Rainbow delimiters
Many programming languages gain additional clarity if pairs of matching
delimiters are coloured differently. The rainbow delimiters mode does just
that.
#+BEGIN_SRC emacs-lisp
(setup rainbow-delimiters
  (:ensure t))
#+END_SRC

** Color highlighting with Rainbow Mode
#+BEGIN_SRC emacs-lisp
(setup rainbow-mode
  (:ensure t))
#+END_SRC

** The Evil Mode
The [[info:evil][Evil Mode]] is the most sophisticated Vi emulation for Emacs, and this
section describes how to set it up.

#+BEGIN_SRC emacs-lisp
(setup ace-jump-mode
  (:ensure t))

(setup evil
  (:ensure t)
  (:config
   (evil-mode 1)
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
   (setf evil-visual-state-tag " Ⓥ ")))
#+END_SRC

** Recording Emacs sessions with Camcorder
Some Emacs features are better explained with a short demonstration video
than many words. The camcorder package allows to record Emacs sessions in
many video formats.
#+BEGIN_SRC emacs-lisp
(setup camcorder
  (:ensure t)
  (:config
   (define-key global-map (kbd "<f12>")
     'camcorder-mode)))
#+END_SRC

** Monitoring the Battery
#+BEGIN_SRC emacs-lisp
(setup battery
  (:ensure t)
  (:config
   (setf battery-mode-line-format "🔋 %p%%%%")
   (setf battery-update-interval 5)
   (display-battery-mode 1)))
#+END_SRC

** The Insidious Big Brother Database for Emacs
#+BEGIN_SRC emacs-lisp
(setup bbdb
  (:ensure t)
  (:config
   (setf bbdb-default-country "Germany"
         bbdb-file "~/.emacs.d/bbdb"
         bbdb-gui t
         bbdb-north-american-phone-numbers-p nil)
   (bbdb-initialize)))
#+END_SRC

** Automatical Text Completion with Company
#+BEGIN_SRC emacs-lisp
(setup company-auctex
  (:ensure t))

(setup company
  (:ensure t)
  (:config
   (setf company-idle-delay 0.02)
   (setf company-minimum-prefix-length 1)
   (company-auctex-init)

   (defun enable-company-mode ()
     (company-mode 1))

   (add-hook 'lisp-mode-hook 'enable-company-mode)

   (defun indent-or-complete ()
     (interactive)
     (if (looking-at "\\_>")
         (company-complete-common)
       (indent-according-to-mode)))))
#+END_SRC

** Multiple Cursors
A convenient feature, especially when it comes to renaming multiple
occurences of a variable in source code. In its simplest form, it suffices
to mark a word and press `R' to edit all its occurences at the same time.
#+BEGIN_SRC emacs-lisp
(setup iedit
  (:ensure t))

(setup evil-multiedit
  (:ensure t)
  (:config
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
     'evil-multiedit-prev)))
#+END_SRC

** Openwith mode - Open certain buffers with external tools
Despite the best attempts of the Emacs community, Emacs can not (yet) open
all file types gracefully. The openwith mode compensates for this by
opening files with certain extensions in external programs. It is important
to adapt the variable `openwith-associations' to suit ones personal
preferences.
#+BEGIN_SRC emacs-lisp
(setup openwith
  (:ensure t)
  (:config
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
            "vlc" (file))))))
#+END_SRC

** Regular Expression Building
A mode for interactively building regular expressions and viewing their
effect on the selected buffer. Mostly made obsolete by the Evil mode search
and replace facility, but sometimes useful for complex regular expressions
with multiple grouping constructs.
#+BEGIN_SRC emacs-lisp
(setup re-builder
  (:config
   (setf reb-re-syntax 'read)))
#+END_SRC

** Bibliographic References with Reftex
#+BEGIN_SRC emacs-lisp
(setup reftex
  (:ensure t)
  (:config
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
   (add-hook 'org-mode-hook 'org-mode-reftex-setup)))
#+END_SRC

** Image viewing with Emacs
Emacs can open images but does not rescale them to fit to the buffer. The
`image+' library scales pictures accordingly.
#+BEGIN_SRC emacs-lisp
(setup image+
  (:ensure t)
  (:config
   (imagex-global-sticky-mode)
   (imagex-auto-adjust-mode)))
#+END_SRC

** Gracefully manage matching Parentheses with Paredit
#+BEGIN_SRC emacs-lisp
(setup paredit
  (:ensure t))

(setup evil-paredit
  (:ensure t)
  (:config
   (defun enable-evil-paredit-mode ()
     (evil-paredit-mode 1))))
#+END_SRC

* Major Modes
** The Org Mode
If Emacs is the place a programmer lives when using his computer, the [[info:org][Org mode]]
is likely to be his living room. At its core it is a mode for writing
structured plaintext, but its many extensions allow it to blur the line
between organizing, note taking and programming in amazing ways.

*** Basics
#+BEGIN_SRC emacs-lisp
(setup org-plus-contrib
  (:ensure t)
  (:init
   (setf org-export-backends '(ascii html icalendar latex beamer odt)))
  (:config
   (setf org-adapt-indentation nil)
   (setf org-agenda-window-setup (quote current-window))
   (setf org-catch-invisible-edits 'show)
   (setf org-default-notes-file "~/.emacs.d/org/notes.org")
   (setf org-directory "~/.emacs.d/org")
   (setf org-pretty-entities t)
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
   (global-set-key "\C-ca" 'org-agenda)))
#+END_SRC

Finally there is this little hack for full fontification of long Org mode
buffers. It is not clear (as of 2016) whether this is still an issue.

#+BEGIN_SRC emacs-lisp
(setup org-fontification-hack
  (:config
   (setf jit-lock-chunk-size 10000)))
#+END_SRC

*** Organizing with Org Agenda
#+BEGIN_SRC emacs-lisp
(setup org-agenda
  (:config
   (make-directory "~/.emacs.d/eap-playlists" t)

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
               \"%?\"")))))
#+END_SRC

*** Generating LaTeX from Org mode buffers
#+BEGIN_SRC emacs-lisp
(setup cdlatex
  (:ensure t))

(setup org-plus-contrib
  (:ensure t)
  (:config
   (setf org-latex-create-formula-image-program 'imagemagick)
   (setf org-format-latex-options
         (plist-put org-format-latex-options :scale 1.6))
   (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
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
       "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))))
#+END_SRC

*** Managing source code with Org Babel
#+BEGIN_SRC emacs-lisp
(setup org-plus-contrib
  (:ensure t)
  (:config
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
   (setq-default org-export-babel-evaluate 'inline-only)))
#+END_SRC

The org-src minor mode allows to edit a single source code block in a
separate buffer. It is convenient to have the same evil state in both
buffers.

#+BEGIN_SRC emacs-lisp
(setup org-src
  (:config
   (defun org-src-get-parent-state ()
     (save-excursion
       (set-buffer (marker-buffer org-src--beg-marker))
       evil-state))

   (defun org-src-sync-evil-state ()
     (when org-src-mode
         (evil-change-state (org-src-get-parent-state))))

   (add-hook 'org-src-mode-hook 'org-src-sync-evil-state)))
#+END_SRC

*** Encrypting parts of a buffer with Org Crypt
#+BEGIN_SRC emacs-lisp
(setup org-crypt
  (:config
   (require 'org-crypt)
   (setf auto-save-default nil)
   (org-crypt-use-before-save-magic)
   (setf org-tags-exclude-from-inheritance '("crypt"))
   (setf org-crypt-key "05369722")))
#+END_SRC

*** Beautiful Presentations with Org Reveal
#+BEGIN_SRC emacs-lisp
(setup ox-reveal
  (:ensure t)
  (:config
   (setq
    org-reveal-root
    "file:///home/marco/.emacs.d/elpa/ox-reveal-2015022.2306/reveal.js")))
#+END_SRC

*** Efficient Learning with Org drill
Org drill is an amazing tool to learn new facts. In a first step, one creates
drill cards, which are nothing more than org subtrees with some metadata and the
`:drill:' tag. Afterwards the command `org-drill' will start a sophisticated
drill session.
#+BEGIN_SRC emacs-lisp
(setup org-drill
  (:config
   ;; prevent drill hints from ruining Latex formulas
   (setf org-drill-hint-separator "||HINT||")))
#+END_SRC

** Latex Editing with Auctex
#+BEGIN_SRC emacs-lisp
(setup auctex
  (:ensure t))

(setup latex
  (:config
   (setq-default TeX-PDF-mode t)))
#+END_SRC

** Directory Browsing with Dired
There are numerous little tweaks to enhance the dired usability. One of them is
simply to activate dired+, another one is to enable recursive copies and enable
`dired-dwim-target'. The latter allowes to copy and move whole folders between
adjacent [[info:Emacs#Windows][Emacs windows]].
#+BEGIN_SRC emacs-lisp
(setup dired
  (:config
   (setf dired-dwim-target t
         dired-recursive-copies 'top
         dired-listing-switches "-ahl"
         dired-auto-revert-buffer t
         wdired-allow-to-change-permissions 'advanced)))
#+END_SRC

#+BEGIN_SRC emacs-lisp
(setup dired+
  (:ensure t)
  (:config
   (global-dired-hide-details-mode 1)))
#+END_SRC

Dired narrow is a handy tool to filter the files in a dired buffer.
#+BEGIN_SRC emacs-lisp
(setup dired-narrow
  (:ensure t)
  (:config
   (define-key dired-mode-map (kbd "C-x /") 'dired-narrow)))
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
(setup c-mode
  (:mode "\\.cl\\'" "\\.frag\\'" "\\.vert\\'")
  (:config
   (setf c-basic-offset 4
         c-hanging-braces-alist (quote set-from-style)
         c-offsets-alist (quote ((innamespace . 0))))))

(setup java-mode
  (:mode "\\.jad\\'"))
#+END_SRC

The Language C++ is the first to my knowledge where Emacs stutters with maximal
syntax highlighting. Unfortunately it is not possible to change the ridiculously
complex syntax of C++ (current standardization efforts are even going in the
opposite direction), so as an alternative the font lock decoration level is
lowered.

#+BEGIN_SRC emacs-lisp
(setup c++-font-lock
  (:config
   (setf font-lock-maximum-decoration (quote ((c++-mode . 2) (t . t))))))
#+END_SRC

** Common Lisp
#+BEGIN_SRC emacs-lisp
(setup slime-company
  (:ensure t))

(setup slime
  (:ensure t)
  (:config
   (setf inferior-lisp-program "sbcl")
   (slime-setup
    '(slime-fancy
      slime-cl-indent
      slime-sprof
      slime-asdf
      slime-fancy-inspector
      slime-company
      ;;slime-fuzzy
      slime-autodoc
      ))
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

   'override-slime-repl-bindings-with-paredit))
#+END_SRC

** Emacs Lisp
The language Emacs Lisp is a fine blend of Maclisp, Common Lisp and some
language for editing text. Unsurprisingly Emacs is well suited for editing
Emacs Lisp. The only worthwile addition provided here is a simple Macro
stepper called `macroexpand-point'.
#+BEGIN_SRC emacs-lisp
(setup emacs-lisp
  (:config

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

   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
   (add-hook 'emacs-lisp-mode-hook 'enable-evil-paredit-mode)
   (add-hook 'emacs-lisp-mode-hook 'enable-company-mode)
   (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
   (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))
#+END_SRC

** Maxima
#+BEGIN_SRC emacs-lisp
(setup maxima-mode
  (:mode "\\.mac\\'"))

(setup imaxima
  (:config
   ;; This is a little bugfix, otherwise imaxima decided the equation color
   ;; was NIL and would fail
   (setf imaxima-equation-color "#DCDCCC")

   (setf imaxima-use-maxima-mode-flag t)
   (setf imaxima-latex-preamble "
\\usepackage{concrete}
\\usepackage{euler}
")
   (setf imaxima-scale-factor 1.4)))
#+END_SRC

** Scheme Programming
#+BEGIN_SRC emacs-lisp
(setup geiser
  (:ensure t)
  (:config
   (setf geiser-default-implementation "guile")))

(setup scheme-mode
  (:mode "\\.sc\\'")
  (:config
   (setf scheme-program-name "guile")
   (add-hook 'scheme-mode-hook 'enable-paredit-mode)
   (add-hook 'scheme-mode-hook 'enable-evil-paredit-mode)
   (add-hook 'scheme-mode-hook 'enable-company-mode)
   (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)))
#+END_SRC

** Octave like languages
There is a whole family of programming toolkits for applied mathematics,
all with similar syntax as Octave.
#+BEGIN_SRC emacs-lisp
(setup octave-mode
  (:mode "\\.sci\\'" "\\.m\\'"))
#+END_SRC

** Proof General
Proof General is an Emacs frontend for various Theorem Provers.
#+BEGIN_SRC emacs-lisp
(setup proofgeneral
  (:config
   (load-file "~/.emacs.d/elisp/ProofGeneral-4.2/generic/proof-site.el")

   ;; show-paren-mode does not interact well with the Proof General
   (add-hook 'proof-ready-for-assistant-hook
             (lambda () (show-paren-mode 0)))))
#+END_SRC

** EAP - Music Without Jolts
#+BEGIN_SRC emacs-lisp
(setup eap
  (:ensure "svn co http://svn.gna.org/svn/eap/trunk eap")
  (:config
   (make-directory "~/.emacs.d/eap-playlists" t)

   (setf eap-music-library
         "~/userdata/music")
   (setf eap-playlist-library
         "~/.emacs.d/eap-playlists")

   (setf eap-volume-mute 0.00)

   (setf eap-volume-fade-out-flag nil)

   (add-hook 'kill-buffer-hook
             'eap-always-kill-buffer-cleanly)
   (add-hook 'kill-buffer-hook
             'eap-always-kill-buffer-cleanly)))
#+END_SRC

* User Interface
This chapter is concerned with the interaction between Emacs and human
beings. The aim is to provide a very distraction free environment for the
experienced Emacs user.

** Simple Modifications
First of all there are dozens of minor modifications that deserve no
individual explanation beyond the hint that `C-h v' and `C-h f' explain an
Emacs variable and function, respectively.

#+BEGIN_SRC emacs-lisp
(setup simple-modifications
  (:config
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
   (setf pop-up-frames nil)))
#+END_SRC

Emacs wizards have memoized all their commands and have no need for visual
guidance.

#+BEGIN_SRC emacs-lisp
(setup window-layout
  (:config
   (tooltip-mode -1)
   (tool-bar-mode -1)
   (menu-bar-mode -1)
   (scroll-bar-mode -1)))
#+END_SRC

Emacs disables several interactive functions by default, because they would
confuse the unsuspecting user even more than the average Emacs function. A
user of this configuration should familiarize himself with those commands or
leave them disabled.

#+BEGIN_SRC emacs-lisp
(setup advanced-commands
  (:config
   (put 'narrow-to-page 'disabled nil)
   (put 'narrow-to-region 'disabled nil)
   (put 'upcase-region 'disabled nil)
   (put 'downcase-region 'disabled nil)
   ;; (put 'set-goal-column 'disabled nil)
   (put 'dired-find-alternate-file 'disabled nil)
   (put 'erase-buffer 'disabled nil)
   (put 'scroll-left 'disabled nil)
   (put 'scroll-right 'disabled nil)))
#+END_SRC

There are two major annoyances in an uncofigured Emacs. First, killing a
buffer with an attached process asks for confirmation every single
time. Second, a prompt for interactive confirmation requires the user to type
`y e s RET' instead of a simple `y'. The next code fixes both these issues.

#+BEGIN_SRC emacs-lisp
(setup annoyances
  (:config
   (setf kill-buffer-query-functions
         (remq 'process-kill-buffer-query-function
               kill-buffer-query-functions))

   (defalias 'yes-or-no-p 'y-or-n-p)))
#+END_SRC

Now some customization of the hilighting of matching parentheses.

#+BEGIN_SRC emacs-lisp
(setup paren
  (:config
   (setf show-paren-delay 0
         blink-matching-paren nil
         show-paren-style 'parenthesis)
   (show-paren-mode)))
#+END_SRC

Visually indicate the two major sins in programming: tabs and trailing
whitespace.

#+BEGIN_SRC emacs-lisp
(setup whitespace
  (:config
   (setf whitespace-style '(face trailing tab-mark))
   (global-whitespace-mode)))
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
(setup auto-save
  (:config
   (setf auto-save-default nil
         auto-save-list-file-prefix "~/.emacs.d/auto-save/save-"
         backup-directory-alist (quote (("." . "~/.emacs.d/saves")))
         backup-inhibited nil)))
#+END_SRC

** The Color Theme and Modeline
This chapter deals with the visual appearance of Emacs. Interested readers
might want to read the section [[info:Elisp#Display][Display]] of the Emacs Lisp manual.

The preferred color themes of Marco Heisig.

#+BEGIN_SRC emacs-lisp
(setup zenburn-theme
  (:ensure t)
  (:config
   (load-theme 'zenburn t)
   (set-face-attribute 'button nil :inherit 'link)))

(setup spacemacs-theme
  (:ensure t)
  (:config
   ;; The spacemacs theme tries to detect whether to give the full color
   ;; theme or a restricted version. This fails for the Emacs server, so I
   ;; disable this check
   (defun always-true (&rest args) t)

   (defun load-spacemacs-theme (&optional frame)
     (advice-add 'true-color-p :around #'always-true)
     (load-theme 'spacemacs-dark t)
     (advice-remove 'true-color-p #'always-true))

   ;(load-spacemacs-theme)
   ))
#+END_SRC

The package `powerline' and its derivative `spaceline' make the Emacs mode
line more beautiful.

#+BEGIN_SRC emacs-lisp
(setup spaceline
  (:ensure t)
  (:config
   (require 'spaceline-config)
   (setf powerline-default-separator 'wave)
   (spaceline-spacemacs-theme)))
#+END_SRC

Org mode presents two options for rendering source code blocks. The default
one is to use the `org-block' face. The other one can be activated by
setting `org-src-fontify-natively' to a non-nil value and displays the code
according to the major mode of its language. The approach here is a hybrid
solution that first uses native fontification, but the applies a different
background to illustrate the block structure.

#+BEGIN_SRC emacs-lisp
(setup org-src-fontification
  (:config
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
               :after #'org-src-fontification--after)))
#+END_SRC

** Keybindings
The Evil keybindings are great, but there are some cases where they fall
short. The first annoyance is the use of the `ESC' key to quit a mode. A
useful alternative to `ESC' that is fully on a keyboards home row is to use a
key chord, that is two keys pressed at almost the same time. The most
convenient key chord is `jk'.

#+BEGIN_SRC emacs-lisp
(setup key-chord
  (:ensure t)
  (:config
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
                     "jk" 'evil-normal-state)))
#+END_SRC

The `jk' keystroke can do even more -- when Evil is already in normal mode,
leave the current buffer somehow.

#+BEGIN_SRC emacs-lisp
(setup leave-buffer
  (:config
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

   (key-chord-define evil-normal-state-map "jk" 'leave-buffer)))
#+END_SRC

No bind the key `U' to the awesome Undo-Tree mode, `SPC' to the Ace Jump Word
command that queries for a character and jumps to a matching word and `C-c o' to
the `occur' command.

#+BEGIN_SRC emacs-lisp
(setup evil-basic-keybindings
  (:config
   (define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
   (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
   (define-key evil-normal-state-map (kbd "C-c o") 'occur)))
#+END_SRC

Now a little gem -- `save-if-appropriate'. This function executes a bunch of
heuristics to determine whether now is a good moment to save the current
buffer. Actually these heuristics work so good, that one can bind this command
to the very frequently used `evil-normal-state-entry-hook'.

#+BEGIN_SRC emacs-lisp
(setup save-if-appropriate
  (:config
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

   (add-hook 'evil-insert-state-exit-hook 'save-if-appropriate)))
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
(setup evil-keybindings
  (:config
   (key-chord-define Info-mode-map "jk" 'Info-up)
   (define-key Info-mode-map "n" 'evil-search-next)
   (define-key Info-mode-map "N" 'evil-search-previous)
   (define-key Info-mode-map "b" 'Info-history-back)
   (define-key Info-mode-map "w" 'Info-history-forward)

   (define-key dired-mode-map "n" 'evil-search-next)
   (define-key dired-mode-map "N" 'evil-search-previous)

   (add-hook 'help-mode-hook 'enable-evil-motion-state)
   (add-hook 'package-menu-mode-hook 'enable-evil-motion-state)))
#+END_SRC

** Initial Buffers
A collection of buffers that should be opened as soon as Emacs is
started. The list contains mostly directories.

#+BEGIN_SRC emacs-lisp
(setup initial-buffers
  (:config
   (save-excursion
     (find-file-existing "~/.emacs.d/")
     (find-file "~/userdata/gaming/*" t)
     (find-file "~/userdata/*" t)
     (find-file "~/userdata/proj/*" t)
     (find-file "~/userdata/events/*" t))
   (setf initial-buffer-choice "~/userdata")))
#+END_SRC

** Cleanup the Mode Line with Diminish
There are some modes that are so omnipresent that they deserve no special
mention in the [[info:Emacs#Mode%20Line][Mode Line]]. The small package `diminish' gets rid of them.
#+BEGIN_SRC emacs-lisp
(setup diminish
  (:ensure t)
  (:config
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
   (diminish 'eldoc-mode)))
#+END_SRC

** Finalization
After all initialization is complete, display a nice summary whether the
init file was loaded successfully and if not, what went wrong.

#+BEGIN_SRC emacs-lisp
(setf inhibit-startup-screen t)
(advice-add 'display-startup-echo-area-message
            :override #'ignore)

(defun summarize-initialization ()
  (let ((errors (length init.el-errors)))
    (if (= 0 errors)
        (message "Initialization successful - happy hacking.")
      (message
       "There have been %d errors during init:\n%s"
       (length init.el-errors)
       (mapconcat
        (lambda (init.el-error)
          (pcase-let ((`(,line ,err ,rest) init.el-error))
            (format "Lines %d+: %s %s" line err rest)))
        init.el-errors
        "\n")))))

;; display the summary after the spam from the emacs server
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-at-time
             0.1 nil
             #'summarize-initialization)))

'done
#+END_SRC

* Possible Improvements
A list of things that could be improved in this Emacs config
*** TODO The powerline is not activated in the *Messages* buffer
Probably because the *Messages* buffer is created before powerline.
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
