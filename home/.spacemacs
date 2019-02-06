;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; general
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     csv
     emoji
     ;; deft
     (org :variables
          org-enable-github-support t
          ;; In org doc use:
          ;; #+REVEAL_ROOT: http://cdn.jsdelivr.net/reveal.js/3.0.0/
          org-enable-reveal-js-support t)
     ;; restclient
     (restclient :variables
                 restclient-use-org t)
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;; (syntax-checking :variables
     ;;                  syntax-checking-enable-tooltips nil)
     ;; themes-megapack
     ;;; source control
     git
     version-control
     ;;; languages
     autohotkey
     csharp
     emacs-lisp
     ;; ess
     groovy
     (go :variables
         go-use-golangci-lint t
         go-backend 'lsp
         ;; go-tab-width 4 or nil if using .editorconfig
         godoc-at-point-function 'godoc-gogetdoc
         go-format-before-save t
         gofmt-command "goimports") ;; instead of default gofmt
     haskell
     html
     javascript
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     nginx
     (python :variables
             python-enable-yapf-format-on-save t)
     (ruby :variables
           ruby-enable-enh-ruby-mode t)
     (rust :variables
           rust-format-on-save t)
     shell-scripts
     sql
     terraform
     typescript
     yaml
     ;;; Tools
     command-log
     lsp
     (neotree :variables
              neo-theme 'icons)
     typography
     ;; pdf-tools
     ;; (ranger :variables
     ;;         ranger-preview-file t)
     ;; ranger http://spacemacs.org/layers/+tools/ranger/README.html
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location ' property :
   ;;                              '(your-package:location "~/path/to/your-package/")
   ;;                                      Also include the dependencies as they will not be resolved automatically
   dotspacemacs-additional-packages '(
                                      ag beacon conkeror-minor-mode crux moz textile-mode ssh-agency exec-path-from-shell
                                      helm-rage monokai-theme base16-theme gruvbox-theme dockerfile-mode ox-textile pkgbuild-mode
                                      chef-mode deft org-kanban org-vcard vdirel)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(base16-railscasts
                         gruvbox-dark-medium
                         spacemacs-dark
                         monokai
                         ;; tango-dark
                         ;; leuven
                         ;; zenburn
                         )
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("SauceCodePro Nerd Font"
                               :size 18
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
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
  ;; customize modeline separators
  (setq powerline-default-separator 'arrow-fade)
  ;; binding for add on, should go to a separate layer
  (spacemacs/set-leader-keys "or" 'helm-rage)
  (spacemacs/set-leader-keys "omg" 'org-mobile-pull)
  (spacemacs/set-leader-keys "omp" 'org-mobile-push)
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
  (when *is-osx*
    ;; use dark theme
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GEM_ROOT")
    (exec-path-from-shell-copy-env "GEM_HOME")
    (exec-path-from-shell-copy-env "GEM_PATH")
    (setq-default
     ispell-program-name (executable-find "ispell"))
    ;; use chefdk ruby
    ;; in reverse order since add-to-list adds to the front of exec-path
    ;; (getenv "PATH")

    ;; "/Users/sme/.local/bin:/Users/sme/.cargo/bin:/Users/sme/bin:/Users/sme/go/bin:/opt/chefdk/bin:/Users/sme/.chefdk/gem/ruby/2.5.0/bin:/opt/chefdk/embedded/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/share/dotnet:~/.dotnet/tools:/opt/chefdk/gitbin:/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_10:/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_10" [2 times]
    ;; (add-to-list 'exec-path "/opt/chefdk/embedded/bin")
    ;; (add-to-list 'exec-path "/Users/sme/.chefdk/gem/ruby/2.5.0/bin")
    ;; (add-to-list 'exec-path "/opt/chefdk/bin")
    ) ;; end *is-osx*
  ;; display emoji automatically in magit commit history views
  (with-eval-after-load "magit"
    (add-hook 'magit-status-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode))
  (with-eval-after-load 'projectile
    (spacemacs/set-leader-keys "pss" 'helm-multi-swoop-projectile)
    (spacemacs/set-leader-keys "psa" 'projectile-ag)
    (spacemacs/set-leader-keys "psg" 'projectile-grep)
    (spacemacs/set-leader-keys "psr" 'projectile-ripgrep)
    (spacemacs/set-leader-keys "pS" 'projectile-save-project-buffers))
  ;; (with-eval-after-load "deft"
  ;;   (spacemacs/set-leader-keys "d" 'deft)
  ;;   (spacemacs/set-leader-keys "dq" 'quit-window))
  ;; c# build files
  (add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode))
  ;; Turn on some modes
  (global-hl-line-mode -1)
  (global-company-mode)
  (prettify-symbols-mode)
  ;; (beacon-mode 1)
  (golden-ratio-mode 1)
  (global-set-key (kbd "C-c n") #'spacemacs/indent-region-or-buffer)
  ;; vc
  (setq vc-follow-symlinks t)
  ;; csv-mode
  (setq-default csv-invisibility-default nil)
  ;; ruby
  (setq ruby-insert-encoding-magic-comment nil)
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
          deft-directory org-directory)) ;; end windows
  (when *is-osx*
    (setq org-directory (concat
                         (file-name-as-directory (getenv "HOME"))
                         "org/")
          org-mobile-directory (concat
                                (file-name-as-directory (getenv "HOME"))
                                "Dropbox/Apps/MobileOrg")
          deft-directory org-directory)) ;; end osx

  ;; org specific paths and settings
  (with-eval-after-load 'org
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
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Do not prompt to resume an active clock, just resume it
    (setq org-clock-persist-query-resume nil)
    (setq org-duration-format (quote h:mm)
          org-default-notes-file (concat org-directory "todo.org")
          org-mobile-inbox-for-pull (concat org-directory "inbox.org")
          org-agenda-files (cons org-default-notes-file
                                 (file-expand-wildcards (concat org-directory "todo/?*org"))))
    (let ((default-directory (concat org-directory "todo")))
      (file-expand-wildcards "?*org"))
    ;; see: http://orgmode.org/manual/Extending-ODT-export.html
    ;; (setq org-odt-preferred-output-format "doc")
    )
  (setq vdirel-repository "~/.contacts/card/")
  (with-eval-after-load "org-kanban"
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "kn" 'org-kanban/next)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "kp" 'org-kanban/prev)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "C-n" 'org-kanban/next)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "C-p" 'org-kanban/prev))
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
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)
  (setq pos-tip-background-color nil
        pos-tip-foreground-color "#3a4055")
  ;; (set 'org-structure-template-alist '(("a" . "export ascii")
  ;;                                      ("c" . "center")
  ;;                                      ("C" . "comment")
  ;;                                      ("e" . "example")
  ;;                                      ("E" . "export")
  ;;                                      ("h" . "export html")
  ;;                                      ("l" . "export latex")
  ;;                                      ("q" . "quote")
  ;;                                      ("s" . "src")
  ;;                                      ("v" . "verse")))
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
 '(global-spacemacs-whitespace-cleanup-mode t)
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
    (chef-mode spotify ess-smart-equals ess-R-data-view ctable ess restclient-helm ob-restclient company-restclient know-your-http-well org-kanban apache-mode pdf-tools tablist vmd-mode tide typescript-mode terraform-mode hcl-mode org-mime pkgbuild-mode csv-mode ghub let-alist excorporate go-guru go-eldoc flycheck-gometalinter company-go go-mode dockerfile-mode org-category-capture nginx-mode editorconfig shut-up crux winum unfill fuzzy ahk-mode helm-rage ssh-agency yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic ox-reveal ox-gfm intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode autothemer pcache minitest insert-shebang hide-comnt web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data toml-mode racer flycheck-rust cargo rust-mode omnisharp csharp-mode org goto-chg diminish seq xterm-color omtose-phellack-theme undo-tree ox-textile uuidgen rake org-projectile org-download ob-http mwim livid-mode skewer-mode simple-httpd link-hint git-link flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump f darkokai-theme company-shell column-enforce-mode color-identifiers-mode powerline pcre2el hydra spinner alert log4e gntp markdown-mode json-snatcher json-reformat multiple-cursors js2-mode parent-mode request gitignore-mode fringe-helper git-gutter+ git-gutter flycheck pkg-info epl flx magit-popup git-commit with-editor smartparens iedit anzu highlight dash-functional tern pos-tip company inf-ruby yasnippet packed s dash avy async auto-complete popup package-build bind-key bind-map ranger moz ag conkeror-minor-mode textile-mode projectile helm helm-core magit evil restclient zonokai-theme zenburn-theme zen-and-art-theme yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme sql-indent spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle shell-pop seti-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powershell popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum linum-relative light-soap-theme leuven-theme json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme flycheck-pos-tip flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme eshell-prompt-extras esh-help elisp-slime-nav dracula-theme django-theme diff-hl deft define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-tern company-statistics company-quickhelp command-log-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 ;; '(pos-tip-background-color "#FFFACE")
 ;; '(pos-tip-foreground-color "#272822")
 '(pyvenv-mode t)
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
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822" :family "Hack" :foundry "simp" :slant normal :weight normal :height 121 :width normal)) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C" :family "Hack" :foundry "simp" :slant normal :weight normal :height 121 :width normal))))
;;  '(company-tooltip-common
;;    ((t (:inherit company-tooltip :foreground "#00000" :weight bold :underline nil))))
;;  '(company-tooltip-common-selection
;;    ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#3C3D37" t)
 '(global-spacemacs-whitespace-cleanup-mode t)
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
    (yasnippet-snippets lsp-ui helm-xref emojify doom-modeline lsp-mode counsel swiper ivy chef-mode spotify ess-smart-equals ess-R-data-view ctable ess restclient-helm ob-restclient company-restclient know-your-http-well org-kanban apache-mode pdf-tools tablist vmd-mode tide typescript-mode terraform-mode hcl-mode org-mime pkgbuild-mode csv-mode ghub let-alist excorporate go-guru go-eldoc flycheck-gometalinter company-go go-mode dockerfile-mode org-category-capture nginx-mode editorconfig shut-up crux winum unfill fuzzy ahk-mode helm-rage ssh-agency yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic ox-reveal ox-gfm intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode autothemer pcache minitest insert-shebang hide-comnt web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data toml-mode racer flycheck-rust cargo rust-mode omnisharp csharp-mode org goto-chg diminish seq xterm-color omtose-phellack-theme undo-tree ox-textile uuidgen rake org-projectile org-download ob-http mwim livid-mode skewer-mode simple-httpd link-hint git-link flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump f darkokai-theme company-shell column-enforce-mode color-identifiers-mode powerline pcre2el hydra spinner alert log4e gntp markdown-mode json-snatcher json-reformat multiple-cursors js2-mode parent-mode request gitignore-mode fringe-helper git-gutter+ git-gutter flycheck pkg-info epl flx magit-popup git-commit with-editor smartparens iedit anzu highlight dash-functional tern pos-tip company inf-ruby yasnippet packed s dash avy async auto-complete popup package-build bind-key bind-map ranger moz ag conkeror-minor-mode textile-mode projectile helm helm-core magit evil restclient zonokai-theme zenburn-theme zen-and-art-theme yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme sql-indent spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle shell-pop seti-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powershell popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pastels-on-dark-theme paradox page-break-lines orgit organic-green-theme org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum linum-relative light-soap-theme leuven-theme json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme flycheck-pos-tip flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme eshell-prompt-extras esh-help elisp-slime-nav dracula-theme django-theme diff-hl deft define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-tern company-statistics company-quickhelp command-log-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme bracketed-paste birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pyvenv-mode t)
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
 )
)
