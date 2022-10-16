;; -*- lexical-binding: t -*-

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
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; macro to set var with secret
(defmacro fanshi/setq-secret (var)
  `(setq ,var (auth-source-pass-get (symbol-name ',var) "OrgConfig")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(fanshi/setq-secret org-directory)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Delete duplicated history
(setq history-delete-duplicates t)

;; fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; tranparency NOTE It makes the text transparent too
;; (set-frame-parameter (selected-frame) 'alpha '(100 85))
;; (add-to-list 'default-frame-alist '(alpha 100 10))

(setq tab-width 4)

(setq! enable-local-variables t)

(setq! scroll-lock-mode t)

;; scratch buffer default `org-mode'
(setq! doom-scratch-initial-major-mode 'org-mode)

;; use 'vterm' with 'fish'
(after! vterm (setq! vterm-shell "~/.nix-profile/bin/fish"))

(setq! +format-on-save-enabled-modes '(haskell-mode nix-mode))

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

;; NOTE when async
;; executing Typescript code block...
;; error in process sentinel: async-handle-result: Cannot open load file: No such file or directory, ob-typescript
;; maybe related? https://github.com/hlissner/doom-emacs/issues/2198
;; TEMP FIXME
(setq ob-async-no-async-languages-alist '("typescript"))

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
       ;; FIXME: use set-popup-rule! ?
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

(after! haskell-mode
  (setq! haskell-auto-insert-module-format-string
         (concat haskell-auto-insert-module-format-string
                 "main :: IO ()\n"
                 "main = do\n"
                 "  putText \"Surprise Motherfucker!\"")))

(after! lsp-haskell
  (setq! lsp-haskell-server-path "haskell-language-server")
  (setq! lsp-haskell-floskell-on nil)
  (setq! lsp-haskell-fourmolu-on nil)
  (setq! lsp-haskell-brittany-on nil)
  (setq! lsp-haskell-stylish-haskell-on nil)
  (setq-hook! haskell-mode +format-with-lsp t))

(after! lsp-mode
  (setq! lsp-file-watch-ignored-directories
         (append lsp-file-watch-ignored-directories '("[/\\\\]materialized\\'"
                                                      ;; NOTE: we don't set up lsp for nix, so probably fine for now
                                                      "[/\\\\]nix\\'"
                                                      "[/\\\\]spec\\'"
                                                      "[/\\\\]golden\\'"
                                                      "[/\\\\]\\.postgres\\'"))))

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

(after! org
  (setq! org-archive-location "archive/%s_archive::"
         org-hide-emphasis-markers t
         ;; https://explog.in/notes/writingsetup.html
         ;; org-adapt-indentation nil
         ;; org-indent-indentation-per-level 1
         org-complete-tags-always-offer-all-agenda-tags t
         org-log-into-drawer t
         org-log-reschedule "note"
         org-log-redeadline "note"))

(after! org
  (setq! org-priority-default 67
         org-priority-lowest 69
         org-priority-faces '((65 . error) (66 . warning) (67 . warning) (68 . success) (69 . success))))
;; org-priority-faces

(after! org (fanshi/setq-secret org-crypt-key))

(after! org
  (setq! org-todo-keywords '((sequence "TODO(t!)"
                                       "NEXT(n!)"
                                       "WAIT(w@)"
                                       "HOLD(h@)"
                                       "SOMEDAY(s!)"
                                       "IDEA(i!)"
                                       "|"
                                       "DONE(d!)"
                                       "KILL(k@)")
                             (sequence "TOPLAN(p!)"
                                       ;; "BRAINSTORM(b!)"
                                       "PROJ(P!)"
                                       "HOLD(h@)"
                                       "SOMEDAY(s!)"
                                       "|"
                                       "DONE(d!)"
                                       "KILL(k@)")
                             ;; (sequence "[ ](T!)" "[-](S!)" "[?](W@/!)" "|" "[X](D!)")
                             ))
  (pushnew! org-todo-keyword-faces
            '("IDEA" org-todo)
            '("TOREAD" org-todo)
            '("TOPLAN" org-todo)
            '("DEAL" org-todo)
            '("PICK" org-todo-active)
            ;; '("BRAINSTORM" +org-todo-active)
            '("SCAN" +org-todo-active)
            '("NEXT" +org-todo-active)
            '("READING" +org-todo-active)
            '("INBOX" org-todo)
            '("RECIPE" org-todo)))

(after! org-roam (setq! org-roam-directory (concat org-directory "roam/")))

(after! org-journal
  (setq! org-journal-dir (concat org-roam-directory "journal/")
         ;; org-journal-time-format (cdr org-time-stamp-formats)
         org-journal-encrypt-journal t)

  ;; (setq! org-journal-enable-agenda-integration t)

  ;; (setq! org-journal-carryover-items nil)

  ;; NOTE no need auto close I think
  ;; close after save hook
  ;; FIXME NOTE: or use popup window?????
  ;; (add-hook! org-journal-mode :append (add-hook! 'after-save-hook :local 'kill-buffer-and-window))

  ;; highlight time string with org-date face
  (font-lock-add-keywords 'org-journal-mode '(("\\(\\*\\)\\(\\*\\) .*\\([0-9]\\{2\\}:[0-9]\\{2\\}\\) \\(.+\\)"
                                               (1 'org-hide t)
                                               (2 'org-level-2 t)
                                               (3 'org-date t)
                                               (4 'org-level-2 t)))))

;; (use-package! bibtex-completion
;;   :defer t
;;   :config
;;   (setq bibtex-completion-notes-path (concat org-roam-directory "notes/")
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

;; (use-package! citeproc-org
;;   :after org
;;   :config
;;   (citeproc-org-setup))

(after! org-capture
  (setq! org-capture-templates
         (seq-filter
          (lambda (x)
            (and
             (not (string= "t" (car x)))
             (not (string= "j" (car x)))
             (not (string= "n" (car x)))))
          org-capture-templates)))

(after! org-capture
  (setq! org-capture-templates
         (mapcar
          (lambda (x) (if (> (length x) 2) (append x '(:clock-in t :clock-resume t)) x))
          org-capture-templates)))

(after! org-capture
  (defun fanshi/org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  (pushnew! org-capture-templates
            '("Im" "Meeting" entry
              ;; (file "inbox.org")
              (function fanshi/org-journal-find-location)
               ;; "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
               "* %(format-time-string  org-journal-time-format)with %? :Meeting:\n" :clock-in t :clock-resume t)
            '("Ip" "Phone call" entry
              ;; (file "inbox.org")
              (function fanshi/org-journal-find-location)
               ;; "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
               "* %(format-time-string  org-journal-time-format)%? :@Phone:\n" :clock-in t :clock-resume t)
            '("I" "Interrupt")))

(after! org-capture
  (pushnew! org-capture-templates
            `("cr" "sudden link to read with clock" entry (file+olp "read.org" "Link")
              ,(string-join
                '("* READING %(org-web-tools--org-link-for-url)"
                  " %i%?"
                  "")
                "\n")
              :clock-in t :clock-keep t :immediate-finish t)
            `("ct" "sudden task with clock" entry (file+olp "fanshi.org.gpg" "Tasks")
              ,(string-join
                '("* TODO %^{Title}"
                  " %i%?"
                  "")
                "\n")
              :clock-in t :clock-keep t :immediate-finish t)
            '("c" "start clock for")))

(after! org-capture
  (pushnew! org-capture-templates
            `("l" "link" entry (file "inbox.org")
              ,(string-join
                '("* INBOX %(org-web-tools--org-link-for-url)"
                  ":PROPERTIES:"
                  ":CREATED: %U"
                  ":END:"
                  " %a")
                "\n")
              :immediate-finish t)))

(after! org-capture
  (pushnew! org-capture-templates
            `("i" "inbox" entry (file "inbox.org")
              ,(string-join
                '("* INBOX %^{heading}"
                  ":PROPERTIES:"
                  ":CREATED: %U"
                  ":END:"
                  " %i%?"
                  " %a")
                "\n")
              :clock-in t :clock-resume t)))

(after! org
  (setq! fanshi/org-work-directory (concat org-directory "work/")))

(after! org
  (defun fanshi/org-todo-trigger (change-plist) ""
         (when (equal (plist-get change-plist :type) 'todo-state-change)
           (let ((fanshi/proj-org-refile-targets `((,(mapcar (lambda (y) (funcall #'concat org-directory y))
                                                             '("fanshi.org.gpg"
                                                               "agnes_ng.org"
                                                               "projects.org"))
                                                    . (:todo . "PROJ"))))
                 (org-refile-targets (pcase (plist-get change-plist :from)
                    ("INBOX" (pcase (plist-get change-plist :to)
                               ("TOREAD" '(("~/org/read.org" . (:level . 1))))
                               ("TOPLAN" `((("~/org/agnes_ng.org" "~/org/fanshi.org.gpg") . (:level . 1))
                                                (,(directory-files fanshi/org-work-directory t (rx ".org.gpg" eos)) . (:level . 2))))
                               ("TODO" `((,(mapcar
                                            (lambda (y) (funcall #'concat org-directory y))
                                            '("fanshi.org.gpg"
                                              "agnes_ng.org"
                                              "projects.org"))
                                          . (:todo . "PROJ"))))
                               ("IDEA" `((,(mapcar
                                            (lambda (y) (funcall #'concat org-directory y))
                                            '("fanshi.org.gpg"
                                              "agnes_ng.org"
                                              "projects.org"))
                                          . (:todo . "TOPLAN"))
                                         (,(mapcar
                                            (lambda (y) (funcall #'concat org-directory y))
                                            '("fanshi.org.gpg"
                                              "agnes_ng.org"
                                              "projects.org"))
                                          . (:todo . "PROJ"))))
                               ("DEAL" '(("~/org/deals.org" . (:maxlevel . 2))))
                               (_ org-refile-targets)))
                    ("DONE" (pcase (plist-get change-plist :to)
                               ("RECIPE" '(("~/org/cooking.org" . (:tag . "Recipe"))))
                               ("IDEA" `((,(mapcar
                                            (lambda (y) (funcall #'concat org-directory y))
                                            '("fanshi.org.gpg"
                                              "agnes_ng.org"
                                              "projects.org"))
                                          . (:todo . "PROJ"))
                                         ("~/org/cooking.org" . (:tag . ""))
                                         ))
                               ("DEAL" '(("~/org/groceries.org" . (:maxlevel . 2))))
                               (_ nil)))
                    (_ nil))))
             (when org-refile-targets (org-refile))))))

(after! org (setq! org-trigger-hook 'fanshi/org-todo-trigger))
;; org-refile-allow-creating-parent-nodes "confirm"

(after! org-noter
  ;; (defun fanshi/noter-capture-note ()
  ;;   (interactive)
  ;;   (call-interactively #'org-noter-insert-precise-note)
  ;;   (insert "#+ATTR_ORG: :width 500 ")
  ;;   (call-interactively #'org-download-screenshot)
  ;;   )
  (setq! org-noter-notes-search-path (list (concat org-roam-directory "books/"))
         org-noter-doc-split-fraction '(0.57 0.43)))

(defun fanshi/make-line () "" (concat "\n" (make-string (window-width) 9472)))

(after! org-agenda
  (setq!
   ;; org-agenda-start-with-clockreport-mode t
   ;; org-agenda-files (seq-filter (lambda (x) (not (string-match-p "\\.#.*\\.org$" x)))
   ;;                              (append (directory-files (concat fanshi/org-roam-directory "notes/") 'FUll "\\.org$")
   ;;                                      (directory-files org-directory 'FULL "\\.org$")
   ;;                                      ))
   ;; org-agenda-files '("/Users/fanshi/org/event.org"
   ;;                    "/Users/fanshi/org/routine.org"
   ;;                    "/Users/fanshi/org/habit.org.gpg"
   ;;                    "/Users/fanshi/org/fanshi.org.gpg"
   ;;                    ;; "/Users/fanshi/org/deals.org"
   ;;                    "/Users/fanshi/org/agnes_ng_habit.org"
   ;;                    "/Users/fanshi/org/agnes_ng.org")
   org-agenda-files '("~/org/")
   org-clock-report-include-clocking-task t
   org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4 :fileskip0 t :compact t :narrow 80))))

(after! org-agenda
  (setq! org-agenda-block-separator 9472
         org-agenda-compact-blocks t
         org-agenda-breadcrumbs-separator " / "
         org-agenda-span 'day
         org-agenda-start-day nil
         org-agenda-start-on-weekday nil
         org-deadline-warning-days 30
         org-agenda-current-time-string "⬲ NOW -- NOW --"
         org-agenda-prefix-format '(;; (agenda . " %-3i %18s  %?-12t %-25b ")
                                    ;; (agenda . " %-3i %-44b %?18s %?-12t")
                                    ;; (agenda . " %-3i %-44b %?-18s %?-12t")
                                    (agenda . " %-3i %-44b %11s %?-12t")
                                    ;; (todo . " %-3i                     ")
                                    (todo . " %-3i %-44b %?-12t")
                                    (tags . " %i %-12:c")
                                    (search . " %i %-12:c"))
         org-agenda-format-date (lambda (date) (concat (fanshi/make-line) "\n" (org-agenda-format-date-aligned date)))
         org-agenda-sorting-strategy '((agenda time-up habit-down priority-down category-keep)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep))))

(setq! fanshi/agenda
       '((:name "Clocked Today 📰📰📰" :log t)
         ;; (:name "Calendar 📅📅📅" :time-grid t :and (:scheduled today :not (:habit t) ))
         (:name "Calendar 📅📅📅" :time-grid t :and (:scheduled today))
         (:name "Deadlines Just Aren't Real To Me Until I'm Staring One In The Face 🚨🚨🚨" :deadline today :order 2)
         (:name "What Is Dead May Never Die 🚣🚣🚣" :deadline past :order 3)
         (:name "Defuse The Bomb 💣💣💣" :deadline future :order 4)
         (:name "Déjà Vu 🔁🔁🔁" :and (:habit t :todo ("TODO" "[ ]")) :order 5) ;; 🧟🧟🧟
         ;; (:name "Déjà Vu 🔁🔁🔁" :and (:habit t :todo ("TODO" "[ ]") :scheduled today) :order 5) ;; 🧟🧟🧟
         ;; (:name "Déjà vécu 🥶🥶🥶" :and (:habit t :todo ("TODO" "[ ]") :scheduled past) :order 6) ;; 🧟🧟🧟
         ;; (:name "Presque vu ⏩⏩⏩" :and (:habit t :todo ("TODO" "[ ]") :scheduled future) :order 7) ;; 🧟🧟🧟
         ;; (:name "Meetings"
         ;;  :and (:todo "MEETING" :scheduled future)
         ;;  :order 8)
         ))

(after! org
  (setq! fanshi/alltodo
         `((:discard (:scheduled future :deadline future :regexp ,org-scheduled-time-hour-regexp :todo "INBOX"))
           (:name "Important 💎💎💎" :tag "Payment" :priority "A" :order 2) ;;🚔🚔🚔
           ;; (:name "Do I really look like a guy with a plan??? 🃏🃏🃏" :and (:todo "TOPLAN" :priority> "D") :order 3)
           (:name "Do I really look like a guy with a plan??? 🃏🃏🃏" :todo "TOPLAN" :order 3)
           (:name "Camping 🏕🏕🏕" :todo "WAIT" :order 11) ; Set order of this section 💎💎💎
           ;; (:name "Inbox 📬📬📬" :todo "INBOX" :order 30)
           ;; (:name "Peek Into Future 🔮🔮🔮" :scheduled future :order 4)
           (:name "Watching 📺📺📺" :and (:todo "READING" :tag "TV") :order 9)
           (:name "こっちも見ろ 👁👁👁" :todo ("READING" "SCAN") :order 8)
           ;; (:name "Reading 📚📚📚" :todo ("READING" "SCAN") :order 10)
           (:name "Quick Picks 🚀🚀🚀" :and (:effort< "0:10" :todo "TODO") :order 4)
           ;; NOTE: tried to follow logic in org-habit-insert-consistency-graphs to find dying habit but seems not easy
           ;; (:name "Dying Habit" :and (:habit t
           ;;                      :todo ("TODO" "[ ]")
           ;;                      :not (:regexp ,org-scheduled-time-hour-regexp)) :order 5)))
           ;; (:name "Déjà Vu 🔁🔁🔁" :and (:habit t
           ;;                               :todo ("TODO" "[ ]")
           ;;                               :scheduled t
           ;;                               :not (:scheduled future))
           ;;                :order 6)
           ;; (:name "Super B 👶🏿👶🏿👶🏿" :and (:priority "B" :not (:file-path "projects")) :order 9)
           (:name "Super B 👶👶👶" :and (:priority "B" :not (:file-path "projects")) :order 7)
           ;; (:name "Others 🏝🏝🏝" :and (:priority "C" :not (:file-path "projects")) :order 21)
           ;; (:name "Optional 🧧🧧🧧" :and (:priority "C" :not (:file-path "projects")) :order 90)
           ;; (:name "waht 🧧🧧🧧" :todo "TOREAD" :order 90)
           ;; NOTE: check
           ;; (:name "Should Be Nothing"
           ;;  :not (:file-path "projects"
           ;;        :file-path "read"
           ;;        :file-path "idea")
           ;;  :order 99)
           ;; (:discard (:habit t))
           ;; NOTE Project
           (:discard (:not (:file-path "projects")))
           (:auto-outline-path t :order 5))))

(after! org-agenda
  (setq! fanshi/org-agenda-file-regexp
         (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?" org-agenda-file-regexp)))

(use-package! org-super-agenda
  :after org-agenda
  ;; :defer-incrementally org-roam org-journal
  :init
  (setq org-agenda-show-log t
        ;; NOTE: https://github.com/alphapapa/org-super-agenda/issues/50
        org-super-agenda-header-map (make-sparse-keymap)
        ;; fanshi/org-agenda-header (concat "\n" (make-string (window-width) 9472))
        ;; fanshi/make-org-agenda-header (defun () (concat "\n" (make-string (window-width) 9472)))
        fanshi/org-agenda-file-regexp (replace-regexp-in-string
                                       "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                       org-agenda-file-regexp)
        org-agenda-custom-commands '(("a" . "Agenda")
                                     ("aa" "My Agenda"
                                      ((agenda "" ((org-super-agenda-groups fanshi/agenda)))
                                       (alltodo "" ((org-agenda-overriding-header (fanshi/make-line))
                                                    (org-super-agenda-groups fanshi/alltodo))))
                                      ((org-agenda-files '("/Users/fanshi/org/routine.org"
                                                           "/Users/fanshi/org/event.org"
                                                           "/Users/fanshi/org/deals.org"
                                                           "/Users/fanshi/org/agnes_ng.org"))))
                                     ("ag" "Agnes's Agenda"
                                      ((agenda "" ((org-super-agenda-groups fanshi/agenda)))
                                       (alltodo "" ((org-agenda-overriding-header (fanshi/make-line))
                                                    (org-super-agenda-groups fanshi/alltodo))))
                                      ((org-agenda-files '("/Users/fanshi/org/agnes_ng_habit.org"
                                                           "/Users/fanshi/org/agnes_ng.org"))))
                                     ("p" . "Private")
                                     ("pa" "Agenda" ((agenda "" ((org-super-agenda-groups fanshi/agenda)))
                                                     (alltodo "" ((org-agenda-overriding-header (fanshi/make-line))
                                                                  (org-super-agenda-groups fanshi/alltodo))))
                                      ((org-agenda-files '("/Users/fanshi/org/routine.org"
                                                           "/Users/fanshi/org/habit.org.gpg"
                                                           "/Users/fanshi/org/fanshi.org.gpg"
                                                           "/Users/fanshi/org/event.org"
                                                           "/Users/fanshi/org/deals.org"
                                                           "/Users/fanshi/org/agnes_ng.org"
                                                           "/Users/fanshi/org/groceries.org"))))
                                     ("c" "Cooking" todo "" (
                                                                 (org-super-agenda-groups fanshi/alltodo)
                                                                 (org-agenda-files '("/Users/fanshi/org/cooking.org"))))
                                     ("r" "Reading" todo "" (
                                                             ;; (org-super-agenda-groups fanshi/alltodo)
                                                             (org-agenda-files '("/Users/fanshi/org/read.org"))))
                                     ;; ("g" "Groceries List" agenda "" ((org-super-agenda-groups fanshi/agenda)
                                     ;;                                (org-agenda-files '("/Users/fanshi/org/groceries-list.org"))))
                                     ))
  :config
  (org-super-agenda-mode))

(use-package! org-web-tools
  ;; :after-call org-capture
  :commands (org-web-tools--org-link-for-url))

(after! langtool (setq! langtool-bin "languagetool-commandline"))

(after! plantuml-mode (setq! plantuml-default-exec-mode 'executable))

;; TEMP keywords
(after! hl-todo (pushnew! hl-todo-keyword-faces '("TEMP" 'warning 'bold)))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map :n "/" #'dired-narrow-fuzzy))

(after! dired (setq dired-use-ls-dired nil))

;; NOTE: https://github.com/abo-abo/lispy/issues/509
(after! (lispy python lpy)
  (add-hook 'lpy-mode-hook (lambda () (progn
                                   (setq-local python-shell-completion-native-disabled-interpreters (append python-shell-completion-native-disabled-interpreters '("python3")))
                                   (setq-local completion-at-point-functions '(lsp-completion-at-point python-completion-at-point t))
                                   (let ((lispy-python-proc-name (concat "lispy-python-" (projectile-project-name))))
                                     (condition-case nil
                                         (lispy--python-proc lispy-python-proc-name)
                                       (error (setq-local lispy-python-proc (get-process lispy-python-proc-name)))))
                                   (cl-letf (((symbol-function 'python-shell-send-string)
                                              (lambda (str process) (comint-send-string process (format "exec(%s)\n" (python-shell--encode-string str))))))
                                     (python-shell-send-string-no-output python-shell-eval-setup-code lispy-python-proc)
                                     (python-shell-send-string-no-output python-shell-eval-file-setup-code lispy-python-proc))
                                   (lispy-python-middleware-reload)))))
;; NOTE: https://github.com/abo-abo/lispy/issues/509

(after! lispy (setq lispy-compat '(edebug cider)))

(use-package! gif-screencast
  :commands (gif-screencast-start-or-stop)
  :init (setq gif-screencast-args '("-x")
              gif-screencast-capture-format "ppm"
              gif-screencast-cropping-program "" ;; NOTE diable cropping, seems its only crop part of the emacs screen fro some reason
              )
  :bind ("<f12>" . gif-screencast-start-or-stop))

(use-package! nov :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(use-package! keycast
  :defer
  :config (define-minor-mode keycast-mode
            "Show current command and its key binding in the mode line."
            :global t
            (if keycast-mode
                (add-hook 'pre-command-hook 'keycast-mode-line-update t) (remove-hook 'pre-command-hook 'keycast-mode-line-update)))
  (add-to-list 'global-mode-string '("" mode-line-keycast)))

(after! pdf-view
  (setq! pdf-tools-installer-os "nixos")
  (pdf-tools-install)
  (setq! pdf-view-midnight-colors '("#ABB2BF" . "#282C35"))
  (add-hook! pdf-tools-enabled #'pdf-view-midnight-minor-mode)
  ;; (add-hook! pdf-tools-enabled #'hide-mode-line-mode)
  )

(use-package! jest :hook (js2-mode . jest-minor-mode))

(push '("\\.sc\\'" . scala-mode) auto-mode-alist)
(use-package! sbt-mode :disabled)

(use-package! ammonite-term-repl
  :after scala-mode
  :config (progn
            (setq ammonite-term-repl-auto-config-mill-project nil)
            (setq ammonite-term-repl-auto-detect-predef-file nil)
            ;; (setq ammonite-term-repl-program-args '("-s" "--no-default-predef"))
            (set-repl-handler! 'scala-mode #'run-ammonite :persist t)))

(setq! +doom-dashboard-ascii-banner-fn #'(lambda ()))
(setq! +doom-dashboard-menu-sections '())

(after! pass (setq! pass-show-keybindings nil))

(after! notmuch (setq! +notmuch-sync-backend 'mbsync))
;; (setq +notmuch-sync-backend 'mbsync-xdg)

(after! sendmail (setq! sendmail-program (executable-find "msmtp")))

(after! projectile
  (setq projectile-project-name-function (lambda (project-root)
                                           (let ((name (funcall 'projectile-default-project-name project-root)))
                                             (if (member name '("python" "haskell" "bootstrap"))
                                                 (concat (funcall 'projectile-default-project-name (file-name-directory (directory-file-name project-root))) "/" name)
                                               name)))))

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
