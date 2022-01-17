;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Francis Chan"
      user-mail-address "jackychany321@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Sarasa Fixed HC" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Sarasa Fixed HC" :size 13 :weight 'light)
      doom-big-font nil
      ;; doom-big-font nil
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Delete duplicated history
(setq history-delete-duplicates t)

;; fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; tranparency NOTE It makes the text transparent too
(set-frame-parameter (selected-frame) 'alpha '(100 85))
;; (add-to-list 'default-frame-alist '(alpha 100 10))

(setq tab-width 4)

(setq! enable-local-variables t)

(setq! scroll-lock-mode t)

(setq! +format-on-save-enabled-modes '(haskell-mode nix-mode))

;; scratch buffer default `org-mode'
(setq! doom-scratch-initial-major-mode 'org-mode)

;; vterm use fish
(after! vterm (setq! vterm-shell "~/.nix-profile/bin/fish"))

;; NOTE: https://www.reddit.com/r/emacs/comments/b7rsxu/behold_orgbabelexecutetypescript/
(defun org-babel-execute:typescript (body params)
  "babel execute typescript"
  (let* ((tmp-ts-file (org-babel-temp-file "scripts" ".ts"))
         (tmp-js-file (concat (substring tmp-ts-file 0 -2) "js"))
         (strict (if (assq :strict params) "--strict " ""))
         (cmd (concat "tsc " strict (shell-quote-argument tmp-ts-file))))
    (with-temp-file tmp-ts-file (insert body))
    (with-temp-buffer
      (if (eq (call-process-shell-command cmd nil t) 0)
          (progn
            (insert-file-contents tmp-js-file)
            (if (assq :js params)
                (buffer-string)
              (require 'ob-js)
              (org-babel-execute:js (buffer-string) params)))
        (buffer-string)))))

;; NOTE: https://github.com/emacs-lsp/lsp-mode/issues/2842#issuecomment-870807018
(defmacro fanshi/org-babel-edit-lsp-prep (lang ext)
  `(defun ,(intern (concat "org-babel-edit-prep:" lang))  (babel-info)
     "prep for `org-edit-special' using lsp for some lang"
     (let* ((tmp-src-file (org-babel-temp-file ,(concat lang "-src-edit-") ,(concat "." ext))))
       (with-temp-file tmp-src-file (insert (cadr babel-info)))
       (setq-local buffer-file-name tmp-src-file)
       (lsp)
       ;; NOTE: when lookup doc using lsp, it use pop-up.
       ;; NOTE: when we kill the popped up look-up, whole edit session gone because it was a pop-up
       ;; NOTE: so change the window to non-pop up, and window it more convenient than pop than code editing too.
       (+popup/raise (selected-window)))
     ))

(defvar fanshi/org-babel-edit-lsp-langs '(("haskell" . "hs") ("typescript" . "ts") ("js" . "js") ("python" . "py") ("rust" . "rs"))
  "alist for langs that will use lsp when `org-edit-special', key is the lang, and value is lang's file extension")

;; NOTE: defun org-babel-edit-prep:lang for each lang in `fanshi/org-babel-edit-lsp-langs'
(mapc (lambda (x) (eval `(fanshi/org-babel-edit-lsp-prep ,(car x) ,(cdr x)))) fanshi/org-babel-edit-lsp-langs)

;; NOTE: in `fanshi/org-babel-edit-lsp-prep', the `org-edit-special' pop-up it promoted, and after `org-edit-src-exit', we got one duplicate pop-up shaped window.
;; NOTE: this advice automatically delete that window for better ux
(advice-add 'org-edit-src-exit :around (lambda (f &rest args)
                                         (if-let* ((babel-info org-src--babel-info)
                                                   (lang (car babel-info))
                                                   (babel-lsp-lang (assoc lang fanshi/org-babel-edit-lsp-langs)))
                                             (progn (apply f args) (evil-window-delete))
                                           (apply f args))))

;; NOTE when async
;; executing Typescript code block...
;; error in process sentinel: async-handle-result: Cannot open load file: No such file or directory, ob-typescript
;; maybe related? https://github.com/hlissner/doom-emacs/issues/2198
;; TEMP FIXME
(setq ob-async-no-async-languages-alist '("typescript"))

(use-package! lsp-haskell
  :after lsp-mode
  :preface (add-hook 'haskell-mode-local-vars-hook #'lsp!)
  (setq! lsp-haskell-server-path "haskell-language-server")
  )

(setq-hook! haskell-mode +format-with-lsp t)

;; haskell templates
(after! haskell-mode
  (setq! haskell-auto-insert-module-format-string
         (concat haskell-auto-insert-module-format-string
                 "main :: IO ()\n"
                 "main = do\n"
                 "  putText \"Surprise Motherfucker!\"")))

(after! elfeed
  (setq! elfeed-feeds
         '(("http://feeds.feedburner.com/incodeblog" blog haskell)
           ("https://noonker.github.io/index.xml" blog tech)
           ("https://mollermara.com/rss.xml" emacs blog stat)
           ("https://notxor.nueva-actitud.org/rss.xml" es blog)
           ("http://pragmaticemacs.com/feed/" blog emacs)
           ("https://blog.thomasheartman.com/rss.xml" blog emacs haskell)
           ("https://medium.com/feed/@mojia" en blog)
           ("https://mac-ra.com/feed/atom/" jp blog)
           ("https://www.parsonsmatt.org/feed.xml" en haskell blog)
           ("https://www.fosskers.ca/jp/rss" jp haskell blog)
           ("https://www.fosskers.ca/en/rss" en haskell blog)
           ("https://lexi-lambda.github.io/feeds/all.atom.xml" en haskell blog)
           ("https://kseo.github.io/atom.xml" en haskell blog)
           ("https://sandymaguire.me/atom.xml" en blog)
           ("https://reasonablypolymorphic.com/atom.xml" en haskell blog)
           ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXf8jlTSP9kp6g4ROCfgvbQ" youtube )
           ;; ("https://dev.to/bradparker" en haskell blog)
           ("https://www.williamyaoh.com/feed.atom" en haskell blog)
           ;; ("https://www.reddit.com/r/haskell/top/.rss?t=week" top haskell reddit)
           ;; ("https://www.reddit.com/r/hongkong/top/.rss?t=week" top hongkong reddit)
           )
         elfeed-search-filter "+unread @1-month-ago"))

;; org
(after! org
  (setq! org-hide-emphasis-markers t)
  ;; https://explog.in/notes/writingsetup.html
  ;; (setq! org-adapt-indentation nil)
  ;; (setq! org-indent-indentation-per-level 1)
  (setq! org-todo-keywords '((sequence "TODO(t!)"
                                       "WAIT(w@/!)"
                                       "|"
                                       "DONE(d!)"
                                       "CANCELED(k@)")
                             (sequence "TOREAD(r!)"
                                       "SCAN(!)"
                                       "READING(!)"
                                       "|"
                                       "DONE(d!)"
                                       "CANCELED(k@)")
                             (sequence "PROJ(p!)"
                                       "|"
                                       "DONE(d!)"
                                       "CANCELED(k@)")
                             (sequence "INBOX(i!)" "|" )
                             (sequence "[ ](T!)" "[-](S!)" "[?](W@/!)" "|" "[X](D!)")
                             ))
  (setq! org-log-into-drawer t)
  ;; remove doom's journal template
  ;; (setq! org-capture-templates
  ;;        (seq-filter (lambda (x) (not (or (string-match (car x) "j")  (string-match (car x) "n") (string-match (car x) "t")))) org-capture-templates))
  (pushnew! org-todo-keyword-faces
            '("TOREAD" org-todo)
            '("SCAN" +org-todo-active)
            '("READING" +org-todo-active)
            '("INBOX" org-todo)
            )

  (setq! org-capture-templates '(("i" "inbox" entry (file "inbox.org")
                                  "* INBOX %^{heading}\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i%?\n %a")
                                 ("c" "start clock for")
                                 ("ct" "sudden task with clock" entry (file+olp "projects.org" "fanshi" "Tasks")
                                  "* TODO %^{Title}\n %i%?\n"
                                  :clock-in t
                                  :clock-keep t
                                  :immediate-finish t
                                  )
                                 ("cl" "sudden reading with clock" entry (file+olp "projects.org" "fanshi" "Tasks")
                                  "* READING %(org-web-tools--org-link-for-url)\n %i%?\n"
                                  :clock-in t
                                  :clock-keep t
                                  :immediate-finish t
                                  )
                                 ;; ("c" "clock" entry (function org-journal-find-location)
                                 ;;  "* %(format-time-string  org-journal-time-format) %^{Title}\n%a"
                                 ;;  :clock-in t
                                 ;;  :clock-keep t
                                 ;;  :immediate-finish t
                                 ;;  )
                                 ;; TODO set up meeting cpature
                                 ;; ("m" "meeting" entry (file "inbox.org")
                                 ;;  "* MEETING with %^{who}\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i%?\n %U")
                                 ;; TODO set up email
                                 ;; ("e" "email" entry (file+headline ,(concat org-directory "emails.org") "Emails")
                                 ;;  "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
                                 ("l" "link" entry (file "inbox.org")
                                  "* INBOX %(org-web-tools--org-link-for-url)\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :immediate-finish t)
                                 ;; TODO set up org protocol
                                 ;; ("c" "org-protocol-capture" entry (file ,(concat org-directory "inbox.org"))
                                 ;;  "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
                                 ;; NOTE Seems capture to journal is not right, every capture should go to inbox first, unless you are more then a capture, and hence not a capture
                                 ;; ("j" "Journal" entry (function org-journal-find-location)
                                 ;;  "* %(format-time-string  org-journal-time-format) %^{Title}\n%i%?\n%a")
                                 ("p" "Templates for projects")
                                 ("pt" "Project-local todo" entry
                                  (file+headline +org-capture-project-todo-file "Tasks")
                                  "* [] %?\n%i\n%a" :prepend t)
                                 ("pn" "Project-local notes" entry
                                  (file+headline +org-capture-project-notes-file "Notes")
                                  "* %U %?\n%i\n%a" :prepend t)
                                 ("pc" "Project-local changelog" entry
                                  (file+headline +org-capture-project-changelog-file "Unreleased")
                                  "* %U %?\n%i\n%a" :prepend t)
                                 ("o" "Centralized templates for projects")
                                 ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                 ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                 ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
                                 ))
  (defun fanshi/org-todo-trigger (change-plist) ""
         (when (equal (plist-get change-plist :type) 'todo-state-change)
           (when (equal (plist-get change-plist :from) "INBOX")
             (if (equal (plist-get change-plist :to) "TOREAD")
                 (let ((org-refile-targets '(("~/org/read.org" . (:level . 1))))) (org-refile))
               (when (equal (plist-get change-plist :to) "TODO")
                 (let ((org-refile-targets '(("~/org/projects.org" . (:level . 2))))) (org-refile))
                 )))
           ))
  (setq! org-trigger-hook 'fanshi/org-todo-trigger)
  ;; (setq! org-refile-allow-creating-parent-nodes "confirm")
  )

(setq! fanshi/org-roam-directory "~/org/roam/")

;; (after! org-journal
;;   (setq! org-journal-dir (concat fanshi/org-roam-directory "journal/"))
;;   (setq! org-journal-enable-agenda-integration t)

;;   (setq! org-journal-carryover-items nil)

;;   ;; NOTE no need auto close I think
;;   ;; close after save hook
;;   ;; (add-hook! org-journal-mode :append (add-hook! 'after-save-hook :local 'kill-buffer-and-window))

;;   ;; highlight time string with org-date face
;;   (font-lock-add-keywords 'org-journal-mode '(("\\(\\*\\)\\(\\*\\) .*\\([0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.+\\)"
;;                                                (1 'org-hide t)
;;                                                (2 'org-level-2 t)
;;                                                (3 'org-date t)
;;                                                (4 'org-level-2 t)
;;                                                )))
;;   ;; org capture

;;   ;; helper function
;;   (defun org-journal-find-location ()
;;     ;; Open today's journal, but specify a non-nil prefix argument in order to
;;     ;; inhibit inserting the heading; org-capture will insert the heading.
;;     (org-journal-new-entry t)
;;     ;; Position point on the journal's top-level heading so that org-capture
;;     ;; will add the new entry as a child entry.
;;     (goto-char (point-min))))

;; (after! org-roam (setq! org-roam-directory fanshi/org-roam-directory))

;; (after! org-noter
;;   (defun fanshi/noter-capture-note ()
;;     (interactive)
;;     (call-interactively #'org-noter-insert-precise-note)
;;     (insert "#+ATTR_ORG: :width 500 ")
;;     (call-interactively #'org-download-screenshot)
;;     )
;;   (setq! org-noter-notes-search-path (list (concat fanshi/org-roam-directory "notes/"))
;;          org-noter-doc-split-fraction '(0.57 0.43)))

;; set up langtool path
(after! langtool (setq! langtool-bin "languagetool-commandline"))

;; set up plantuml-mode
(after! plantuml-mode (setq! plantuml-default-exec-mode 'executable))

;; TEMP keywords
(after! hl-todo (pushnew! hl-todo-keyword-faces '("TEMP" 'warning 'bold)))

(after! org-agenda
  ;;  for clock
  (setq!
   ;; org-agenda-start-with-clockreport-mode t
   ;; org-agenda-files (seq-filter (lambda (x) (not (string-match-p "\\.#.*\\.org$" x)))
   ;;                              (append (directory-files (concat fanshi/org-roam-directory "notes/") 'FUll "\\.org$")
   ;;                                      (directory-files org-directory 'FULL "\\.org$")
   ;;                                      ))
   ;; org-agenda-start-with-log-mode t
   org-clock-report-include-clocking-task t
   org-agenda-clockreport-parameter-plist(quote (:link t :maxlevel 4 :fileskip0 t :compact t :narrow 80))
   )
  (setq! org-agenda-skip-scheduled-if-done t
         org-agenda-skip-deadline-if-done t
         org-agenda-include-deadlines t
         org-agenda-block-separator 9472
         org-deadline-warning-days 60
         org-agenda-compact-blocks t
         ;; org-agenda-breadcrumbs-separator " > "
         org-agenda-breadcrumbs-separator " / "
         ;; org-agenda-breadcrumbs-separator "->"
         org-agenda-span 'day
         org-agenda-start-day nil ;; i.e. today
         org-agenda-start-on-weekday nil
         org-agenda-current-time-string "⬲ NOW -- NOW --"
         org-agenda-prefix-format '(
                                    ;; (agenda . " %-3i %18s  %?-12t %-25b ")
                                    ;; (agenda . " %-3i %18s  %?-12t %-25b ")
                                    ;; (agenda . " %-3i %-45b %18s  %?-12t")
                                    (agenda . " %-3i %-25b %18s  %?-12t")
                                    (todo . " %-3i                     ")
                                    (tags . " %i %-12:c")
                                    (search . " %i %-12:c"))
         org-agenda-format-date (lambda (date) (concat "\n"
                                                       (make-string (window-width) 9472)
                                                       "\n"
                                                       (org-agenda-format-date-aligned date)))))

;; NOTE: ls does not support --dired; see ‘dired-use-ls-dired’ for more details.
;; NOTE: https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(after! dired (setq dired-use-ls-dired nil))

;; org-super-agenda
(setq! fanshi/private-agenda
       '((:name "Clocked Today 📰📰📰" :log t)
         (:name "Calendar 📅📅📅" :time-grid t :and (:scheduled today :not (:habit t) ))
         (:name "Deadlines Just Aren't Real To Me Until I'm Staring One In The Face 🚨🚨🚨" :deadline today :order 2)
         (:name "What Is Dead May Never Die 🚣🚣🚣" :deadline past :order 3)
         (:name "Defuse The Bomb 💣💣💣" :deadline future :deadline today :order 4)
         (:name "Déjà Vu 🔁🔁🔁" :and (:habit t :not (:scheduled future))) ;; 🧟🧟🧟
         ;; (:name "Meetings"
         ;;  :and (:todo "MEETING" :scheduled future)
         ;;  :order 8)
         (:name "Should Be Nothing" :order 99)
         ;; (:discard (:anything t))
         ))
(setq! fanshi/private-alltodo
       '((:discard (:scheduled today :deadline t))
         (:name "Important 💎💎💎" :tag "Payment" :priority "A" :order 2) ;;🚔🚔🚔
         (:name "Inbox 📬📬📬" :todo "INBOX" :order 3)
         (:discard (:habit t))
         (:name "Peek Into Future 🔮🔮🔮" :scheduled future :order 4)
         (:name "Watching 📺📺📺" :and (:todo "READING" :tag "TV") :order 6)
         (:name "Reading 📚📚📚" :todo "READING" :order 7)
         (:name "Quick Picks 🚀🚀🚀" :and (:effort< "0:30" :todo "TODO") :order 8)
         (:name "Others 🏝🏝🏝" :and (:priority "B" :not (:file-path "projects")) :order 20)
         (:name "Should Be Nothing" :not (:file-path "projects" :file-path "read") :order 99)
         ;; (:name "Optional 🧧🧧🧧" :and (:priority "C" :not (:file-path "projects")) :order 90)
         ;; (:name "waht 🧧🧧🧧" :todo "TOREAD" :order 90)
         (:name "Camping 🏕🏕🏕" :todo "WAITING" :order 9) ; Set order of this section 💎💎💎
         (:discard (:not (:file-path "projects")))
         (:auto-outline-path t :order 5)
         ;; (:name "Projects" :file-path "project" :order 5)
         ;; (:discard (:anything t))
         ))

(setq! fanshi/alltodo (cons '(:discard (:tag "Private")) fanshi/private-alltodo ))
(setq! fanshi/agenda (cons '(:discard (:tag "Private")) fanshi/private-agenda ))

(use-package! org-super-agenda
  :after org-agenda
  ;; :defer-incrementally org-roam org-journal
  :init
  (setq
   ;; org-super-agenda-header-separator (make-string (window-width) 9473)
   ;; org-super-agenda-header-separator "\n"
   org-super-agenda-header-map (make-sparse-keymap) ;; https://github.com/alphapapa/org-super-agenda/issues/50#issuecomment-446272744
   )
  (setq org-agenda-custom-commands '(
                                     ;; ("w" "WEEKLY REVIEW"
                                     ;;  ((todo "DONE"
                                     ;;         ((org-agenda-overriding-header "DONE!")
                                     ;;   (todo "CANCELLED"
                                     ;;         ((org-agenda-overriding-header "CANCELLED")))
                                     ;;   (todo "TODO"
                                     ;;         ((org-agenda-overriding-header "TODO Items (without time attached)")
                                     ;;          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))))
                                     ;;   (todo "WAITING"
                                     ;;         ((org-agenda-overriding-header "WAIT: Items on hold (without time attached)")
                                     ;;          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))))))
                                     ("a" "Agenda"
                                      ((agenda "" ((org-super-agenda-groups fanshi/agenda)))
                                       (alltodo "" ((org-agenda-overriding-header (concat
                                                                                   "\n"
                                                                                   (make-string (window-width) 9472) ;; ፨ lag, sad
                                                                                   ))
                                                    (org-super-agenda-groups fanshi/alltodo)))))
                                     ("d" "Agenda"
                                      ((agenda "" ((org-super-agenda-groups fanshi/private-agenda)))
                                       (alltodo "" ((org-agenda-overriding-header (concat
                                                                                   "\n"
                                                                                   (make-string (window-width) 9472)
                                                                                   ))
                                                    (org-super-agenda-groups fanshi/private-alltodo)))))))
  :config
  (org-super-agenda-mode))

;; (use-package! citeproc-org
;;   :after org
;;   :config
;;   (citeproc-org-setup))

;; (use-package! org-roam-bibtex
;;   :after (org-roam)
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :config
;;   (setq orb-preformat-keywords
;;         '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;   (setq orb-templates
;;         `(("r" "ref" plain (function org-roam-capture--get-point)
;;            ""
;;            :file-name "lit/${slug}"
;;            :head ,(concat
;;                    "#+setupfile: ./hugo_setup.org\n"
;;                    "#+title: ${=key=}: ${title}\n"
;;                    "#+roam_key: ${ref}\n\n"
;;                    "* ${title}\n"
;;                    "  :PROPERTIES:\n"
;;                    "  :Custom_ID: ${=key=}\n"
;;                    "  :URL: ${url}\n"
;;                    "  :AUTHOR: ${author-or-editor}\n"
;;                    "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;                    "  :NOTER_PAGE: \n"
;;                    "  :END:\n")
;;            :unnarrowed t))))

;; (use-package! bibtex-completion
;;   :defer t
;;   :config
;;   (setq bibtex-completion-notes-path (concat fanshi/org-roam-directory "notes/")
;;         ;; bibtex-completion-bibliography "~/.org/braindump/org/biblio.bib"
;;         bibtex-completion-pdf-field "file"
;;         bibtex-completion-notes-template-multiple-files
;;         (concat
;;          "#+title: ${title}\n"
;;          "#+roam_key: cite:${=key=}\n"
;;          "* TODO Notes\n"
;;          ":PROPERTIES:\n"
;;          ":Custom_ID: ${=key=}\n"
;;          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
;;          ":AUTHOR: ${author-abbrev}\n"
;;          ":JOURNAL: ${journaltitle}\n"
;;          ":DATE: ${date}\n"
;;          ":YEAR: ${year}\n"
;;          ":DOI: ${doi}\n"
;;          ":URL: ${url}\n"
;;          ":END:\n\n"
;;          )))

;; dired-narrow
(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map :n "/" #'dired-narrow-fuzzy))

;; lpy
(use-package! lpy
  :hook (python-mode . lpy-mode)
  :init (setq lispy-override-python-binary "python"))

(use-package! lispy
  :init (setq lispy-compat '(edebug cider)))

(use-package! org-web-tools
  :commands (org-web-tools--org-link-for-url)
  ;; :after-call org-capture
  )

(use-package! gif-screencast
  :commands (gif-screencast-start-or-stop)
  :init (setq gif-screencast-args '("-x")
              gif-screencast-capture-format "ppm"
              gif-screencast-cropping-program "" ;; NOTE diable cropping, seems its only crop part of the emacs screen fro some reason
              )
  :bind ("<f12>" . gif-screencast-start-or-stop))

(use-package! nov :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

;; (use-package! vuiet :defer)

(use-package! keycast
  :defer
  :config (define-minor-mode keycast-mode
            "Show current command and its key binding in the mode line."
            :global t
            (if keycast-mode
                (add-hook 'pre-command-hook 'keycast-mode-line-update t) (remove-hook 'pre-command-hook 'keycast-mode-line-update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast))
  )
;; (use-package! calfw)

(use-package! pdf-view
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  ;; :hook (pdf-tools-enabled . hide-mode-line-mode)
  :config
  (setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))

;; jest
(use-package! jest :hook (js2-mode . jest-minor-mode))

(use-package! sbt-mode :disabled)

(use-package! ammonite-term-repl
  :after scala-mode
  :config (progn
            (setq ammonite-term-repl-auto-config-mill-project nil)
            (setq ammonite-term-repl-auto-detect-predef-file nil)
            ;; (setq ammonite-term-repl-program-args '("-s" "--no-default-predef"))
            (set-repl-handler! 'scala-mode #'run-ammonite :persist t)))

(push '("\\.sc\\'" . scala-mode) auto-mode-alist)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
