;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! lpy :pin "fa95b11e1023704510cc7dd2897bf8bcc3027cbb")
;; (package! worf)
(package! dired-narrow :pin "1596e516835099b96cb65d1dc372cfbdff6aea96")
(package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
(package! org-web-tools :pin "b5b7fee01eaac845ca80240f3a2d22d426179ce3")
;; (package! citeproc-org)
;; (package! org-roam-bibtex)
;; (package! bibtex-completion)
;; (package! gif-screencast :pin "adec408e6adab2e8e057fe0ad828749f473bfb83")
(package! nov :pin "58c35e677e11f5c04a702b42ac753c80c8955089")
;; (package! vuiet)
;; (package! calfw)
;; (package! keycast)
;; (package! jest)
;; (package! sbt-mode :ignore t)

;; (package! ammonite-term-repl)
;; (package! wasp-mode :pin "76198cdd5f0ece3770c3a586115caea3ea613169")
(package! emacsql :pin "2e6056df110e76da4eabb413f8781c4f1edddf20")

;; No org-babel-execute function for ledger!
;; NOTE: https://github.com/purcell/emacs.d/issues/791
;; NOTE: https://github.com/emacsmirror/org-contrib/commit/17f3c514356430448627104e015f155008b45575
;; (package! org-contrib
;;   :recipe (:host github
;;            :repo "emacsmirror/org-contrib")
;;   :pin "17f3c514356430448627104e015f155008b45575")

(package! lsp-tailwindcss)


(package! typescript-mode :disable t)
