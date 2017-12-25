;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; general
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     csv
     ;; deft
     (org :variables
          org-enable-github-support t
          ;; In org doc use: #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.0.0/
          org-enable-reveal-js-support t)
     ;; restclient
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; (syntax-checking :variables
     ;;                  syntax-checking-enable-by-default nil)
     ;; themes-megapack
     ;;; TODO
     ;; other layers to explore:
     ;; floobits http://spacemacs.org/layers/floobits/README.html
     ;;; source control
     git
     version-control
     ;;; languages
     autohotkey
     csharp
     emacs-lisp
     (go :variables
         go-use-gometalinter t
         ;; go-tab-width 4
         gofmt-command "goimports") ;; instead of default gofmt
     haskell
     html
     javascript
     markdown
     nginx
     (python :variables
             python-enable-yapf-format-on-save t)
     ruby
     rust
     shell-scripts
     sql
     windows-scripts
     yaml
     ;;; Tools
     command-log
     ;; (ranger :variables
     ;;         ranger-preview-file t)
     ;; ranger http://spacemacs.org/layers/+tools/ranger/README.html
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ag beacon conkeror-minor-mode crux moz textile-mode ssh-agency
                                         helm-rage monokai-theme gruvbox-theme dockerfile-mode ox-textile pkgbuild-mode
                                         ;; (omnisharp :location (recipe :fetcher github
                                         ;;                              :repo "OmniSharp/omnisharp-emacs"
                                         ;;                              :branch "feature-omnisharp-roslyn-support"
                                         ;;                              :files ("*.el"
                                         ;;                                      "src/*.el"
                                         ;;                                      "src/actions/*.el")))
                                         )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         gruvbox-dark-medium
                         solarized-dark
                         solarized-light
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         tango-dark
                         ;; leuven
                         ;; zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack";;"Iosevka"
                               :size 24
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup "all"
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; This is a bug fix for tramp causing some startup delay on windows
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (defconst *is-osx* (eq system-type 'darwin))
  (defconst *is-windows* (eq system-type 'windows-nt))
  (defconst *is-linux* (eq system-type 'gnu/linux))
  ;; binding for add on, should go to a separate layer
  (spacemacs/set-leader-keys "or" 'helm-rage)
  ;; useful aspell setup and various executables
  (when *is-windows*
    ;; Never use windows encoding or line endings
    (set-language-environment "UTF-8")
    ;;(setq default-buffer-file-coding-system "utf-8-unix")
    ;;(add-to-list 'spacemacs-indent-sensitive-modes 'ruby-mode)
    (setenv "ASPELL_CONF" nil)
    (add-to-list 'exec-path "C:/tools/Aspell/bin/")
    (add-to-list 'exec-path "c:/users/sescriva/appdata/roaming/npm")
    ;; Use chefdk ruby
    (setenv "GEM_ROOT" "C:/opscode/chefdk/embedded/lib/ruby/gems/2.4.0")
    (setenv "GEM_HOME" "C:/Users/sescriva/AppData/Local/chefdk/gem/ruby/2.4.0")
    (setenv "GEM_PATH" "C:/Users/sescriva/AppData/Local/chefdk/gem/ruby/2.4.0;C:/opscode/chefdk/embedded/lib/ruby/gems/2.4.0")
    (add-to-list 'exec-path "c:/opscode/chefdk/embedded/bin")
    (with-eval-after-load "flycheck"
      (setq-default flycheck-sh-posix-bash-executable "c:/windows/system32/bash.exe"
                    flycheck-sh-shellcheck-executable "c:/tools/shellcheck.exe"
                    flycheck-ruby-executable "c:/opscode/chefdk/embedded/bin/ruby.exe"
                    ;; TODO: add python
                    flycheck-disabled-checkers '(ruby-rubocop)))
    (setq-default
     ispell-program-name (executable-find "aspell")
     flycheck-json-jsonlint-executable "C:/Users/sescriva/AppData/Roaming/npm/jsonlint.cmd"
     omnisharp--curl-executable-path "C:/Program Files/Git/mingw64/bin/curl.exe"
     omnisharp-server-executable-path "c:/tools/omnisharp/bin/OmniSharp.exe"
     org-mobile-checksum-binary "c:/Program Files/Git/usr/bin/md5sum.exe"
     w32-get-true-file-attributes nil)
    ;; rust
    (add-to-list 'exec-path "c:/Users/sescriva/.cargo/bin")
    ) ;; end *is-windows*
  (with-eval-after-load "projectile"
    (spacemacs/set-leader-keys "pss" 'projectile-ag)
    (spacemacs/set-leader-keys "psg" 'projectile-grep))
  (with-eval-after-load "vc-hooks"
      (setq vc-follow-symlinks t))
  ;; c# build files
  (add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode))
  ;; Turn on some modes
  (global-hl-line-mode -1)
  (global-company-mode)
  (prettify-symbols-mode)
  ;; (beacon-mode 1)
  (golden-ratio-mode 1)
  (global-set-key (kbd "C-c n") #'spacemacs/indent-region-or-buffer)
  ;; org mode adjustments
  (setq-default org-element-use-cache nil)
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-directory (concat
                       (file-name-as-directory (getenv "HOME"))
                       "org/"
                       )
        deft-directory org-directory)
  (setq org-src-fontify-natively t
        org-log-done t
        org-refile-use-outline-path nil
        deft-recursive t
        epa-file-cache-passphrase-for-symmetric-encryption t)
  ;; setup ssh on windows for magit
  (when *is-windows*
    (use-package ssh-agency)
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (with-eval-after-load "magit"
      (add-hook 'magit-credential-hook 'ssh-agency-ensure)))
  ;; org paths and org-mobile
  (when *is-windows*
    (use-package ssh-agency)
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (setenv "GNUPGHOME" "c:/Users/sescriva/.gnupg")
    (setq org-directory (concat
                         (file-name-as-directory (getenv "SystemDrive"))
                         "src/org/")
          org-mobile-directory (concat
                                (file-name-as-directory (getenv "USERPROFILE"))
                                "Dropbox/Apps/MobileOrg")
          deft-directory org-directory))
  (setq org-default-notes-file (concat org-directory "todo.org")
        org-mobile-inbox-for-pull (concat org-directory "inbox.org")
        org-agenda-files (cons org-default-notes-file
                               (file-expand-wildcards (concat org-directory "todo/?*org"))))
  ;; see: http://orgmode.org/manual/Extending-ODT-export.html
  ;; (setq org-odt-preferred-output-format "doc")
  (let ((default-directory (concat org-directory "todo")))
    (file-expand-wildcards "?*org"))
  ;; js defaults
  (setq-default js2-basic-offset 2
                js-indent-level 2)
  ;; eshell and eshell-prompt-extras
  (with-eval-after-load "eshell-prompt-extras"
    (set-face-attribute 'epe-git-face nil :inherit font-lock-keyword-face))
  ;; buffer auto revert
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'buffer-menu-mode)
  ;; conkerorrc editing
  ;; (add-hook 'js-mode-hook (lambda ()
  ;;                           (when (string-match "conkeror" (buffer-name))
  ;;                             (conkeror-minor-mode 1))))
  ;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

  ;; (add-hook 'javascript-mode-hook 'javascript-custom-setup)
  ;; (defun javascript-custom-setup ()
  ;;   (moz-minor-mode 1))
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#3C3D37" t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (pkgbuild-mode csv-mode ghub let-alist excorporate go-guru go-eldoc flycheck-gometalinter company-go go-mode dockerfile-mode org-category-capture nginx-mode editorconfig shut-up crux winum unfill fuzzy ahk-mode helm-rage ssh-agency yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic ox-reveal ox-gfm intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode autothemer pcache minitest insert-shebang hide-comnt web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data toml-mode racer flycheck-rust cargo rust-mode omnisharp csharp-mode org goto-chg diminish seq xterm-color omtose-phellack-theme undo-tree ox-textile uuidgen rake org-projectile org-download ob-http mwim livid-mode skewer-mode simple-httpd link-hint git-link flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump f darkokai-theme company-shell column-enforce-mode color-identifiers-mode powerline pcre2el hydra spinner alert log4e gntp markdown-mode json-snatcher json-reformat multiple-cursors js2-mode parent-mode request gitignore-mode fringe-helper git-gutter+ git-gutter flycheck pkg-info epl flx magit-popup git-commit with-editor smartparens iedit anzu highlight dash-functional tern pos-tip company inf-ruby yasnippet packed s dash avy async auto-complete popup package-build bind-key bind-map ranger moz ag conkeror-minor-mode textile-mode projectile helm helm-core magit evil restclient zonokai-theme zenburn-theme zen-and-art-theme yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme sql-indent spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle shell-pop seti-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powershell popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum linum-relative light-soap-theme leuven-theme json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme flycheck-pos-tip flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme eshell-prompt-extras esh-help elisp-slime-nav dracula-theme django-theme diff-hl deft define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-tern company-statistics company-quickhelp command-log-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822" :family "Hack" :foundry "simp" :slant normal :weight normal :height 121 :width normal)) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C" :family "Hack" :foundry "simp" :slant normal :weight normal :height 121 :width normal)))))
