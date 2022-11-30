;; Svart bakground och lite andra grejer
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-amsmath-label "eq:")
 '(LaTeX-fill-break-at-separators '(\\\())
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t)
 '(TeX-error-overview-open-after-TeX-run t)
 '(TeX-parse-self t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open")))
 '(ac-auto-show-menu 0.0)
 '(ac-candidate-limit 20)
 '(ac-delay 0.1)
 '(ack-command "ag ")
 '(ack-project-root-patterns
   '("\\ext_emconf\\.php\\'" "\\`\\.dir-locals\\.el\\'" "\\`Project\\.ede\\'" "\\.xcodeproj\\'" "\\`\\.ropeproject\\'" "\\`\\.\\(?:CVS\\|bzr\\|git\\|hg\\|svn\\)\\'"))
 '(ada-auto-case nil)
 '(add-log-mailing-address "arvid@gluteus.se")
 '(agt-language-source "en")
 '(agt-language-target "fr")
 '(agt-use-ido t)
 '(alert-default-style 'libnotify)
 '(auto-hscroll-mode t)
 '(basecamp-subdomain "gluteus")
 '(browse-url-browser-function 'browse-url-firefox)
 '(c-basic-offset 8)
 '(calendar-week-start-day 1)
 '(compilation-skip-threshold 2)
 '(coq-compile-before-require nil)
 '(coq-compile-parallel-in-background nil)
 '(cua-rectangle-modifier-key 'meta)
 '(custom-safe-themes
   '("197cefea731181f7be51e9d498b29fb44b51be33484b17416b9855a2c4243cb1" "40670c1a2158e4387407ca1c8a06c03153baca5016e3f9f23d7fb4157e408960" "5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "27b97024320d223cbe0eb73104f2be8fcc55bd2c299723fc61d20057f313b51c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "248fe8247849ffed2584a0b6413001f133db952b" "c401ccc408b38755bf478e8f233063c44c7e7706" "485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" "1440d751f5ef51f9245f8910113daee99848e2c0" default))
 '(dabbrev-case-replace nil)
 '(delete-selection-mode t)
 '(dhall-format-at-save nil)
 '(dhall-use-header-line nil)
 '(diff-switches "-r -u")
 '(ebib-preload-bib-files
   '("~/Data/Onebox/Research/bibliography.bib" "~/Data/Onebox/Research/teamlibrary.bib"))
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elfeed-feeds
   '("https://emails2rss.appspot.com/rss?id=f6b199360808c045cb0b12a0a8afd6f61886"))
 '(elfeed-search-date-format '("%Y-%m-%d %H:%M" 16 :left))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults))
 '(fic-highlighted-words '("REFACTOR" "FIXME" "TODO" "BUG" "KLUDGE"))
 '(flycheck-disabled-checkers '(php-phpcs))
 '(flycheck-sh-shellcheck-executable "/home/arvid/.cabal/bin/shellcheck")
 '(flymake-log-level -1)
 '(flymake-start-syntax-check-on-newline nil)
 '(flyspell-abbrev-p t)
 '(font-lock-maximum-decoration t)
 '(fstar-executable "/home/arvid/.opam/4.06.1/bin/fstar.exe")
 '(fstar-smt-executable "z3-4.5.1")
 '(geben-display-window-function 'pop-to-buffer)
 '(git-commit-summary-max-length 80)
 '(grep-files-aliases
   '(("asm" . "*.[sS]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("ch" . "*.[ch]")
     ("el" . "*.el")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     \.\.\.))
 '(haskell-interactive-mode-eval-mode 'ignore)
 '(haskell-interactive-popup-errors nil)
 '(helm-bibtex-pdf-open-function 'helm-open-file-with-default-tool)
 '(helm-boring-file-regexp-list
   '("\\.glob$" "\\.vo$" "\\.hi$" "\\.annot$" "\\.cmi$" "\\.cmxa$" "\\.cma$" "\\.cmx$" "\\.cmo$" "\\.o$" "~$" "\\.bin$" "\\.bak$" "\\.obj$" "\\.map$" "\\.ico$" "\\.pif$" "\\.lnk$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.dll$" "\\.drv$" "\\.vxd$" "\\.386$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn$" "\\.hg$" "\\.git$" "\\.bzr$" "CVS$" "_darcs$" "_MTN$" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.log$"))
 '(highlight-indent-guides-method 'bitmap)
 '(ido-default-buffer-method 'selected-window)
 '(ido-default-file-method 'selected-window)
 '(ido-enable-flex-matching nil)
 '(ido-file-extensions-order '(".org" ".tex" ".v"))
 '(ido-ignore-files
   '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`.*\\.hi"))
 '(ido-max-dir-file-cache 0)
 '(ido-max-directory-size 1000000)
 '(indent-tabs-mode nil)
 '(inferior-haskell-find-project-root t)
 '(inferior-php-program-command "/Users/arvidjakobsson/bin/boris" t)
 '(initial-scratch-message "")
 '(ispell-program-name "aspell")
 '(jde-jdk-registry '(("1.6.0" . "/usr/lib/jvm/java-6-sun/")))
 '(jira2-wsdl-descriptor-url
   "http://bugs.gluteus.se:8080/rpc/soap/jirasoapservice-v2?wsdl")
 '(jiralib-host "")
 '(jiralib-url "https://takingwwoofforward.atlassian.net/")
 '(jq-interactive-default-options
   "-L/home/arvid/dev/nomadic-labs/gas-bug-bounty/scenario-language/")
 '(js-enabled-frameworks '(javascript prototype))
 '(js-flat-functions t)
 '(js-indent-level 4)
 '(js2-auto-indent-flag t)
 '(js2-basic-offset 4)
 '(js2-enter-indents-newline t)
 '(js2-mirror-mode t)
 '(js2-mode-escape-quotes nil)
 '(langtool-default-language "\"en\"")
 '(langtool-disabled-rules "FRENCH_WHITESPACE")
 '(langtool-language-tool-jar
   "/home/arvidj/Downloads/lt/LanguageTool-2.5/languagetool-commandline.jar")
 '(langtool-mother-tongue "en")
 '(latex-preview-pane-multifile-mode 'auctex)
 '(magit-branch-arguments nil)
 '(magit-commit-all-when-nothing-staged 'ask)
 '(magit-completing-read-function 'ido-completing-read)
 '(magit-diff-refine-hunk t)
 '(magit-process-popup-time 5)
 '(magit-push-always-verify nil)
 '(magit-repo-dirs
   '("/Users/arvidjakobsson/public_html/wwoof_community/typo3conf/ext/gc_wwoof/"))
 '(magit-revert-buffers 'silent)
 '(markdown-command "pandoc -f markdown -t html")
 '(merlin-eldoc-type-verbosity 'min)
 '(mk-proj-ack-cmd "ag")
 '(mk-proj-ack-cmd-name "ack")
 '(mk-proj-use-ido-selection t)
 '(nyan-bar-length 16)
 '(ocamlformat-show-errors 'echo)
 '(ocp-indent-path "ocp-indent")
 '(org-clock-display-default-range 'thisweek)
 '(org-clock-history-length 20)
 '(org-clock-idle-time nil)
 '(org-clock-persist t)
 '(org-default-notes-file "~/org/notes.org")
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")))
 '(org-highlight-latex-and-related '(latex))
 '(org-latex-listings t)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-refile-use-outline-path t)
 '(org-remember-templates '(("remember" 116 "" nil nil nil)))
 '(org-reverse-note-order t)
 '(org-startup-folded 'showeverything)
 '(org-tag-faces '(("noexport" . org-ellipsis)))
 '(org-todo-keyword-faces
   '(("NOPE" :foreground "violet" :weight bold)
     ("DONE" :foreground "darkgreen" :weight bold)
     ("WAIT" :foreground "blue" :weight bold)
     ("TODO" :foreground "red" :weight bold)))
 '(org-todo-keywords '((sequence "TODO" "WAIT" "|" "NOPE" "DONE")))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(emojify company-lsp lsp-ui dhall-mode diff-hl git-gutter-fringe ox-rst csv-mode jedi jq-mode arduino-mode graphviz-dot-mode magit-delta exwm gitlab nvm merlin ocamlformat mermaid-mode tide highlight-indentation-guides highlight-indent-guides dumb-jump coverage magit-forge browse-at-remote ligo-mode elfeed org-drill-table org-drill nord-theme birds-of-paradise-plus-theme gitlab-ci-mode-flycheck gitlab-ci-mode org-present blacken org-mobile-sync elpygen dockerfile-mode michelson-mode helm-org json-mode lsp-mode auto-yasnippet pyenv-mode org-sync helm-c-yasnippet org-sync-snippets nyan-mode ox-reveal utop default-text-scale elpy org-noter pdf-tools hydra py-yapf feature-mode ecukes htmlize "htmlize" tuareg dune centered-window python-pytest pytest merlin-eldoc reason-mode visual-regexp pydoc-info deferred proof-general chronometer dired-icon-mode dired-icon all-the-icons-dired request writegood-mode pomidor projectile latex-preview-pane fill-column-indicator leuven-theme github-theme ahungry-theme edit-server w3m haskell-mode org-plus-contrib gitignore-templates visual-fill-column company-math boogie-friends fstar-mode web-server org guess-language helm-dictionary dictionary howdoi hc-zenburn-theme ag magit-svn math-symbols zenburn-theme yaml-mode ws-trim wrap-region wc-mode use-package twig-mode test-case-mode solarized-theme soft-stone-theme smex rust-mode purple-haze-theme phpunit password-generator org-journal org-jira nginx-mode minimap markdown-mode magit lorem-ipsum less-css-mode langtool iedit hl-spotlight highlight-thing highlight-symbol highlight-parentheses highlight-indentation helm-bibtex hackernews gradle-mode gnuplot git-gutter flycheck expand-region exec-path-from-shell ebib dtrt-indent drag-stuff company-coq browse-kill-ring autopair anything ada-mode ack ace-jump-mode))
 '(php-completion-file "~/.emacs.d/plugins/php-completion-file")
 '(php-doc-browser-function 'arvid-browse-php-doc)
 '(php-doc-directory "/Users/arvidjakobsson/docs/php/php-manual-english/")
 '(php-manual-path "~/docs/php/php-manual-english")
 '(php-template-compatibility nil)
 '(phpunit-program "php ./typo3/cli_dispatch.phpsh phpunit")
 '(preview-default-preamble
   '("\\RequirePackage["
     ("," . preview-default-option-list)
     "]{preview}[2004/11/05]" "\\PreviewMacro[{!}]{\\rl}"))
 '(preview-scale-function 1.2)
 '(preview-transparent-color '(highlight :foreground))
 '(proof-splash-enable nil)
 '(pytest-cmd-flags "-x")
 '(python-shell-interpreter "python3")
 '(reb-re-syntax 'string)
 '(recentf-auto-cleanup 'never)
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 1000)
 '(request-backend 'url-retrieve)
 '(rst-compile-toolsets
   '((html "poetry run sphinx-build -b html . \"_build\"" nil)
     (latex "rst2latex.py" ".tex" nil)
     (newlatex "rst2newlatex" ".tex" nil)
     (pseudoxml "rst2pseudoxml.py" ".xml" nil)
     (xml "rst2xml.py" ".xml" nil)
     (pdf "rst2pdf" ".pdf" nil)
     (s5 "rst2s5.py" ".html" nil)))
 '(ruby-indent-level 4)
 '(safe-local-variable-values
   '((ag-arguments "--smart-case" "--stats" "--hidden")
     (coverage-dir)
     (coverage-resultset-filename . "coverage.json")
     (eval set
           (make-local-variable 'compile-command)
           "make -k -C `git rev-parse --show-toplevel` ")
     (pytest-cmd-flags . "-x -s -vv")
     (coq-prog-name . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.8.0/_opam/bin/coqtop")
     (coq-compiler . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.8.0/_opam/bin/coqc")
     (coq-prog-name . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.0.8/_opam/bin/coqtop")
     (coq-compiler . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.0.8/_opam/bin/coqc")
     (eval highlight-regexp "<.*?>" "flycheck-error-list-info")
     (eval highlight-regexp "(.*?)" "flycheck-error-list-filename")
     (org-latex-pdf-process "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f")
     (org-latex-pdf-process quote
                            ("%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f"))
     (eval add-hook 'git-commit-setup-hook 'arvid-wl/magit-commit-msg-add-week-no)
     (eval add-hook 'git-commit-setup-hook 'my-magit-add-week-no)
     (whitespace-line-column . 80)
     (org-todo-keywords
      (sequence "[Startingfo]" "[Ongoing]" "|" "[Finished]" "[Reviewed]" "[Announce]"))
     (org-todo-keywords
      (sequence "[Starting]" "[Ongoing]" "|" "[Finished]" "[Reviewed]" "[Announce]"))
     (org-todo-keywords quote
                        ((sequence "[Starting]" "[Ongoing]" "|" "[Finished]" "[Reviewed]" "[Announce]")))
     (blacken-executable . "~/.pyenv/versions/local/bin/black")
     (eval set
           (make-local-variable 'compile-command)
           (format "cd %s/docs ; poetry run sphinx-build -b html . \"_build\" %s -q"
                   (projectile-project-root)
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name))))
     (eval set
           (make-local-variable 'compile-command)
           (format "cd %s/docs ; poetry run sphinx-build -b html . \"_build\" %s -E"
                   (projectile-project-root)
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name))))
     (eval set
           (make-local-variable 'compile-command)
           (format "cd %s ; poetry run sphinx-build -b html . \"_build\" %s -E"
                   (projectile-project-root)
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name))))
     (coq-prog-name . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.08/_opam/bin/coqtop")
     (coq-compiler . "/home/arvid/dev/nomadic-labs/mi-cho-coq/coq8.08/_opam/bin/coqc")
     (eval set
           (make-local-variable 'compile-command)
           (concat "./logchecker.exe -f "
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name))))
     (eval set
           (make-local-variable 'compile-command)
           (concat "./logchecker.exe -f "
                   (if buffer-file-name
                       (shell-quote-argument
                        (file-name-sans-extension buffer-file-name)))))
     (org-image-actual-width)
     (org-image-actual-width . 300)
     (org-latex-caption-above)
     (TeX-engine quote xetex)
     (org-latex-pdf-process "latexmk -latexoption='-shell-escape' -xelatex -interaction=nonstopmode -output-directory=%o %f")
     (org-latex-packages-alist
      ("" "minted"))
     (org-latex-pdf-process "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
     (org-latex-pdf-process "latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f")
     (org-latex-pdf-process quote
                            ("latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f"))
     (org-latex-pdf-process
      (("latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f")))
     (org-latex-pdf-process
      ("latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f"))
     (org-latex-pdf-process
      '("latexmk -xelatex -interaction=nonstopmode -output-directory=%o %f"))
     (org-latex-pdf-process
      ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
     (org-latex-pdf-process quote
                            ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
     (coq-prog-name . "/home/arvid/dev/nomadic-labs/mi-cho-coq/_opam/bin/coqtop")
     (coq-top . "/home/arvid/dev/nomadic-labs/mi-cho-coq/_opam/bin/coqtop")
     (coq-compiler . "/home/arvid/dev/nomadic-labs/mi-cho-coq/_opam/bin/coqc")
     (org-link-file-path-type . relative)
     (org-link-file-type . relative)
     (orch-image-dir . "/home/arvid/Data/Onebox/Research/Thesis/Soutenance/presentation/figures/")
     (ispell-personal-dictionary . "/home/arvid/Data/Onebox/Research/Thesis/arvid_jakobsson_phd_thesis/.aspell.fr.pws")
     (eval ispell-change-dictionary "french" nil)
     (org-latex-pdf-process "pdflatex -shell-escape talk_frank2018.tex")
     (eval progn
           (add-to-list 'LaTeX-verbatim-macros-with-braces-local "bsplibprimitivesec")
           (add-to-list 'LaTeX-verbatim-macros-with-braces-local "rsevalprog"))
     (eval when
           (and
            (buffer-file-name)
            (string-suffix-p ".tex"
                             (buffer-file-name)
                             t))
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode)
           (flycheck-mode)
           (reftex-mode)
           (writegood-passive-voice-turn-on))
     (eval when
           (and
            (buffer-file-name)
            (string-suffix-p ".tex"
                             (buffer-file-name)
                             t))
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode)
           (flycheck-mode)
           (reftex-mode)
           (writegood-mode))
     (writegood-mode . 1)
     (eval progn
           (add-to-list 'LaTeX-verbatim-macros-with-braces-local "bsplibprimitivesec"))
     (orch-image-dir . "/home/arvid/Data/Onebox/Research/Thesis/arvid_jakobsson_phd_thesis.test/figures")
     (ispell-personal-dictionary . "/home/arvid/Data/Onebox/Research/Thesis/arvid_jakobsson_phd_thesis.test/.aspell.en.pws")
     (TeX-command-extra-options . "-shell-escape")
     (uniquify-min-dir-content . 1)
     (eval when
           (and
            (buffer-file-name)
            (string-suffix-p ".tex"
                             (buffer-file-name)
                             t))
           (message
            (concat "X: Opening "
                    (buffer-file-name)))
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode)
           (flycheck-mode)
           (reftex-mode))
     (eval when
           (and
            (buffer-file-name)
            (string-suffix-p ".tex"
                             (buffer-file-name)
                             t))
           (message
            (concat "X: Opening "
                    (buffer-file-name)))
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode)
           (flycheck-mode))
     (flycheck-mode 1)
     (reftex-mode 1)
     (orch-image-dir . "/home/arvid/Data/Onebox/Research/Thesis/arvid_jakobsson_phd_thesis/figures")
     (fci-mode . t)
     (eval when
           (and
            (buffer-file-name)
            (string-suffix-p ".tex"
                             (buffer-file-name)
                             t))
           (message
            (concat "X: Opening "
                    (buffer-file-name)))
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode))
     (eval message
           (concat "X: Opening "
                   (buffer-file-name)))
     (orch-image-dir . "/home/arvid/Data/Onebox/Research/Thesis/meta/figures")
     (org-catch-invisible-edits . error)
     (org-refile-targets
      (:maxlevel . 3))
     (org-refile-targets)
     (eval progn
           (ispell-change-dictionary "en_US" nil)
           (flyspell-buffer)
           (flyspell-mode))
     (eval progn
           (ispell-change-dictionary "english" nil)
           (flyspell-buffer)
           (flyspell-mode))
     (eval progn
           (ispell-change-dictionary "english" nil)
           (flyspell-buffer))
     (ispell-personal-dictionary . "/home/arvid/Data/Onebox/Research/Thesis/arvid_jakobsson_phd_thesis/.aspell.en.pws")
     (reftex-ref-style-default-list "Cleveref")
     (reftex-ref-style-default-list "Default" "Cleveref")
     (org-taskjuggler-reports-directory . "reports-cost")
     (org-taskjuggler-reports-directory . "reports-comm")
     (org-taskjuggler-reports-directory . "reports-sync")
     (eval ispell-change-dictionary "francais" nil)
     (org-use-property-inheritance "ORDERED")
     (orch-image-dir . "Scans")
     (org-use-property-inheritance . t)
     (org-latex-pdf-process "pdflatex -shell-escape talk_tezos2018.tex")
     (eval ispell-change-dictionary "en")
     (ispell-language)
     (flyspell-mode . 1)
     (orch-image-dir . "./Scans")
     (ido-max-directory-size . 1000000)
     (org-latex-pdf-process "pdflatex -shell-escape talk_cea2018v2.tex")
     (org-refile-targets
      (nil :maxlevel . 3))
     (org-refile-targets quote
                         ((nil :maxlevel . 3)))
     (org-refile-targets quote
                         ((nil :maxlevel 3)))
     (eval ispell-change-dictionary "english" nil)
     (eval progn
           (ispell-change-dictionary "english" nil)
           (flyspell-mode)
           (flyspell-buffer))
     (eval progn
           (ispell-change-dictionary "british" nil)
           (flyspell-mode)
           (flyspell-buffer))
     (eval progn
           (ispell-change-dictionary "british" nil)
           (flyspell-buffer))
     (eval flyspell-buffer)
     (eval ispell-change-dictionary "british" nil)
     (arvid-gnat-scenario-file . "c:/dev/u500/designer_tests/Sources/EMIL/Passenger exchange/Test_Compute_RSD_and_PSD_Opening_and_Closing_Dates/ETE(Compute_RSD_and_PSD_Opening_and_Closing_Dates)/scenario.xml")
     (arvid-gnat-project-file . "c:/dev/u500/themes/Library/Make/Library.gpr")
     (TeX-master . main\.tex)
     (TeX-master . rapport\.tex)
     (ispell-dictionary . "english")
     (langtool-default-language . "en")
     (langtool-default-language . "fr")
     (ispell-dictionary . "francais")
     (tex-main-file . "DM.tex")
     (TeX-master . Rapport_PER\.tex)
     (require-final-newline)
     (buffer-file-coding-system . utf-8-unix)
     (ethan-wspace-errors tabs eol no-nl-eof many-nls-eof)
     (ethan-wspace-errors quote
                          (tabs eol no-nl-eof many-nls-eof))
     (indent-tabs-mode nil)
     (ws-trim-method-hook ws-trim-trailing)))
 '(set-mark-command-repeat-pop t)
 '(sgml-basic-offset 4)
 '(smerge-command-prefix "m")
 '(sql-input-ring-file-name "~/.emacs.d/.history/sql")
 '(sql-mysql-options '("-P 8888"))
 '(sql-mysql-program "/opt/local/bin/mysql5")
 '(sql-sqlite-program "C:\\Users\\100759729\\bin\\sqlite3.exe")
 '(svn-log-edit-show-diff-for-commit t)
 '(svn-status-default-diff-arguments '("--diff-cmd=diff"))
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36 48 \.\.\.))
 '(tab-width 4)
 '(test-case-phpunit-executable
   "/Users/arvidjakobsson/public_html/fowolist/typo3/cli_dispatch.phpsh phpunit")
 '(timeclock-file "~/Dropbox/gluteus/timelogs/.timelog")
 '(tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
 '(typescript-indent-level 2)
 '(vc-handled-backends '(git))
 '(w3m-display-mode 'tabbed)
 '(web-mode-comment-style 2)
 '(web-mode-indent-style 2)
 '(web-mode-markup-indent-offset 4)
 '(yas-prompt-functions '(yas/ido-prompt yas/no-prompt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(caml-types-expr-face ((t (:background "gray28"))) t)
 '(compilation-error ((t (:inherit font-lock-warning-face))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "#8691a8" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#818da5"))))
 '(highlight-thing ((t (:inherit 'underline))))
 '(merlin-compilation-error-face ((t (:inherit default :underline (:color "#E44" :style wave)))))
 '(message-header-subject ((t (:foreground "#88C0D0" :weight bold :height 1.2))))
 '(org-level-2 ((t (:inherit outline-8))))
 '(org-level-4 ((t (:inherit outline-6))))
 '(proof-eager-annotation-face ((t (:background "peach puff"))))
 '(show-paren-match ((((class color) (background dark)) (:background "light sea green"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "olive drab"))))
 '(tuareg-font-lock-extension-node-face ((t (:inherit tuareg-font-lock-infix-extension-node-face :background "DodgerBlue4"))))
 '(which-func ((t (:foreground "SpringGreen3"))))
 '(whitespace-line ((t (:foreground nil :background "#2E3440" :underline (:color "dark khaki" :style wave))))))
