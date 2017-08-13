(eval-when-compile
  (unless (require 'cl-lib nil t)
    (require 'cl)
    (defalias 'cl-case 'case))
  )

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defcustom solarized-termcolors 16
  "This is set to 16 by default, meaning that Solarized will attempt to use the
standard 16 colors of your terminal emulator. You will need to set those colors
to the correct Solarized values either manually or by importing one of the many
colorscheme available for popular terminal emulators and Xdefaults."
  :type 'integer
  :options '(16 256)
  :group 'solarized)

(defcustom solarized-degrade nil
  "For test purposes only; when in GUI mode, forces Solarized to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-bold t
  "Stops Solarized from displaying bold when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-underline t
  "Stops Solarized from displaying underlines when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-italic t
  "Stops Solarized from displaying italics when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-contrast 'normal
  "Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Solarized palette but simply shifts some values up or
down in order to expand or compress the tonal range displayed."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-broken-srgb
  (if (and (eq system-type 'darwin) (eq window-system 'ns))
      (not (and (boundp 'ns-use-srgb-colorspace)
                ns-use-srgb-colorspace))
    nil)
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Solarized works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
(defvar solarized-colors           ; ANSI(Solarized terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
    (base02  "#073642" "#0a2832" "#262626" "black"         "black")
    (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
    (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
    (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
    (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
    (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
    (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
    (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
    (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
    (green   "#859900" "#728a05" "#5f8700" "green"         "green"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defun solarized-face-for-index (facespec index &optional light)
  "Creates a face from facespec where the colors use the names from
  `solarized-colors`."
  (let ((new-fontspec (copy-sequence facespec)))
    (dolist (property '(:foreground :background :color))
      (let ((color-name (plist-get new-fontspec property)))
        (when color-name
          ;; NOTE: We try to turn an 8-color term into a 10-color term by not
          ;;       using default background and foreground colors, expecting the
          ;;       user to have the right colors set for them.
          (when (and (= index 5)
                     (or (and (eq property :background)
                              (eq color-name 'back))
                         (and (eq property :foreground)
                              (member color-name '(base0 base1)))))
            (setf color-name nil))
          (when (eq color-name 'back)
            (setf color-name 'base03))
          (when light
            (setf color-name
                  (cl-case color-name
                    (base03 'base3)
                    (base02 'base2)
                    (base01 'base1)
                    (base00 'base0)
                    (base0 'base00)
                    (base1 'base01)
                    (base2 'base02)
                    (base3 'base03)
                    (otherwise color-name))))
          (plist-put new-fontspec
                     property
                     (nth index (assoc color-name solarized-colors))))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec
                 :box
                 (solarized-face-for-index (plist-get new-fontspec :box) index
                                           light)))
    new-fontspec))

(defun create-face-spec (name facespec)
  `(,name ((((background dark) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))))
           (((background dark) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec
                                       (if (= solarized-termcolors 16) 4 3)))
           (((background dark) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4))
           (((background dark) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5))
           (((background light) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))
                                       t))
           (((background light) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec
                                       (if (= solarized-termcolors 16) 4 3)
                                       t))
           (((background light) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4 t))
           (((background light) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5 t)))))

(defun solarized-color-definitions ()
  (let ((bold        (if solarized-bold 'bold        'unspecified))
        (bright-bold (if solarized-bold 'unspecified 'bold))
        (underline   (if solarized-underline t 'unspecified))
        (opt-under   'unspecified)
        (italic      (if solarized-italic 'italic 'unspecified)))
    (cond ((eq 'high solarized-contrast)
           (let ((orig-base3 base3))
             (rotatef base01 base00 base0 base1 base2 base3)
             (setf base3 orig-base3)))
          ((eq 'low solarized-contrast)
           (setf back      base02
                 opt-under t)))
    (let ((bg-back   '(:background back))
          (bg-base03 '(:background base03))
          (bg-base02 '(:background base02))
          (bg-base01 '(:background base01))
          (bg-base00 '(:background base00))
          (bg-base0 '(:background base0))
          (bg-base1 '(:background base1))
          (bg-base2 '(:background base2))
          (bg-base3 '(:background base3))
          (bg-green '(:background green))
          (bg-yellow '(:background yellow))
          (bg-orange '(:background orange))
          (bg-red '(:background red))
          (bg-magenta '(:background magenta))
          (bg-violet '(:background violet))
          (bg-blue '(:background blue))
          (bg-cyan '(:background cyan))

          (fg-base03 '(:foreground base03))
          (fg-base02 '(:foreground base02))
          (fg-base01 '(:foreground base01))
          (fg-base00 '(:foreground base00))
          (fg-base0 '(:foreground base0))
          (fg-base1 '(:foreground base1))
          (fg-base2 '(:foreground base2))
          (fg-base3 '(:foreground base3))
          (fg-green '(:foreground green))
          (fg-yellow '(:foreground yellow))
          (fg-orange '(:foreground orange))
          (fg-red '(:foreground red))
          (fg-magenta '(:foreground magenta))
          (fg-violet '(:foreground violet))
          (fg-blue '(:foreground blue))
          (fg-cyan '(:foreground cyan))

          (fmt-none   `())
          (fmt-bold   `(:weight ,bold))
          (fmt-bldi   `(:weight ,bold :slant ,italic))
          (fmt-undr   `(                             :underline ,underline))
          (fmt-undb   `(:weight ,bold                :underline ,underline))
          (fmt-undi   `(              :slant ,italic :underline ,underline))
          (fmt-uopt   `(                             :underline ,opt-under))
          ;; FIXME: don’t hardcode the SRGB color names
          (fmt-curl-red    `(                        :underline (:color "#dc322f" :style wave)))
          (fmt-curl-yellow `(                        :underline (:color "#b58900" :style wave)))
          (fmt-curl-magenta `(                       :underline (:color "#d33682" :style wave)))
          (fmt-curl-cyan `(                          :underline (:color "#2aa198" :style wave)))
          (fmt-curl-orange `(                          :underline (:color "#cb4b16" :style wave)))
          (fmt-curl-violet `(                          :underline (:color "#6c71c4" :style wave)))
          (fmt-curl-green `(                          :underline (:color "#859900" :style wave)))
          (fmt-ital   `(              :slant ,italic))
          ;; FIXME: not quite the same
          (fmt-stnd   `(                                                   :inverse-video t))
          (fmt-revr   `(                                                   :inverse-video t))
          (fmt-revb   `(:weight ,bold                                      :inverse-video t))
          (fmt-revbb  `(:weight ,bright-bold                               :inverse-video t))
          (fmt-revbbu `(:weight ,bright-bold         :underline ,underline :inverse-video t)))
      (eval-after-load 'ansi-color
        '(setf ansi-color-names-vector [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00]))
      (mapcar (lambda (face) (apply 'create-face-spec face))
              `(;; basic
                (default (,@fg-base0 ,@bg-back))   ; Normal
                (cursor (,@fg-base03 ,@bg-base1))  ; Cursor
                (shadow (,@fg-base01))
                (link (,@fmt-undr ,@fg-violet))    ; Underlined
                (link-visited (,@fmt-undr ,@fg-magenta))
                (match (,@fmt-revr ,@fg-yellow))   ; Search
                (error (,@fg-red))      ; ErrorMsg
                (warning (,@fg-violet))    ; WarningMsg
                (success (,@fg-green))              ; MoreMsg
                (escape-glyph-face (,@fg-red))
                (fringe (,@fg-base01 ,@bg-base02))
                (linum (,@fg-base01 ,@bg-base02))
                (header-line (,@fg-base0 ,@bg-base02 ,@fmt-revbb)) ; Pmenu
                (highlight (,@bg-base02))
                (hl-line (:underline ,opt-under ,@bg-base02)) ; CursorLine
                (isearch (,@fmt-stnd ,@fg-orange ,@bg-back)) ; IncSearch
                (isearch-fail (:inherit error)) ; ErrorMsg
                (lazy-highlight (:inherit match)) ; Search
                (menu (,@fg-base0 ,@bg-base02))
                (minibuffer-prompt (,@fmt-bold ,@fg-cyan)) ; Question
                (region (,@fg-base01 ,@bg-base03 ,@fmt-revbb)) ; Visual
                (secondary-selection (,@bg-base02))
                (shadow (,@fg-base01))
                (trailing-whitespace (,@fmt-revr ,@fg-red))
                (vertical-border (,@fg-base0))
                ;; comint
                (comint-highlight-prompt (,@fg-blue))
                ;; compilation
                (compilation-info (,@fmt-bold ,@fg-green))
                (compilation-warning (,@fmt-bold ,@fg-orange))
                ;; custom
                (custom-button
                 (,@fg-base1 ,@bg-base02 :box (:line-width 2 :style released-button)))
                (custom-button-mouse
                 (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit custom-button))
                (custom-button-pressed
                 (,@fmt-revr ,@fg-base1 ,@bg-base02
                             :box (:line-width 2 :style pressed-button)
                             :inherit custom-button-mouse))
                (custom-changed (,@fmt-revr ,@fg-blue ,@bg-base3))
                (custom-comment (,@fg-base1 ,@bg-base02))
                (custom-comment-tag (,@fg-base1 ,@bg-base02))
                (custom-documentation (:inherit default))
                (custom-group-tag (,@fg-base1))
                (custom-group-tag-1 (,@fmt-bold ,@fg-base1))
                (custom-invalid (,@fmt-revr ,@fg-red ,@bg-back))
                (custom-link (,@fg-violet))
                (custom-state (,@fg-green))
                (custom-variable-tag (,@fg-base1))
                ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
                ,@(cl-case solarized-diff-mode
                    (high
                     `((diff-added (,@fmt-revr ,@fg-green))
                       (diff-changed (,@fmt-revr ,@fg-yellow))
                       (diff-removed (,@fmt-revr ,@fg-red))
                       (diff-refine-change
                        (,@fmt-revr ,@fg-blue ,@bg-back))))
                    (low
                     `((diff-added (,@fmt-undr ,@fg-green))
                       (diff-changed (,@fmt-undr ,@fg-yellow))
                       (diff-removed (,@fmt-bold ,@fg-red))
                       (diff-refine-change (,@fmt-undr ,@fg-blue ,@bg-back))))
                    (normal
                     (if window-system
                         `((diff-added (,@fmt-bold ,@fg-green ,@bg-base02))
                           (diff-changed (,@fmt-bold ,@fg-yellow ,@bg-base02))
                           (diff-removed (,@fmt-bold ,@fg-red ,@bg-base02))
                           (diff-refine-change (,@fmt-bold ,@fg-blue ,@bg-base02)))
                       `((diff-added (,@fg-green ,@bg-base02))
                         (diff-changed (,@fg-yellow ,@bg-base02))
                         (diff-removed (,@fg-red ,@bg-base02))
                         (diff-refine-change (,@fg-blue ,@bg-base02))))))
                (diff-refine-added (:inherit diff-added ,@fmt-revr))
                (diff-refine-removed (:inherit diff-removed ,@fmt-revr))
                (diff-file-header (:inherit default ,@fg-blue))
                (diff-hunk-header (:inherit default))
                (diff-header (,@fg-base1 ,@bg-back))
                ;; IDO
                (ido-only-match (,@fg-green))
                (ido-subdir (,@fg-blue))
                (ido-first-match (,@fmt-bold ,@fg-green))
                ;; emacs-wiki
                (emacs-wiki-bad-link-face (,@fmt-undr ,@fg-red))
                (emacs-wiki-link-face (,@fmt-undr ,@fg-blue))
                (emacs-wiki-verbatim-face (,@fmt-undr ,@fg-base00))
                ;; eshell
                (eshell-ls-archive (,@fg-magenta))
                (eshell-ls-backup (,@fg-yellow))
                (eshell-ls-clutter (,@fg-orange))
                (eshell-ls-directory (,@fg-blue)) ; Directory
                (eshell-ls-executable (,@fg-green))
                (eshell-ls-missing (,@fg-red))
                (eshell-ls-product (,@fg-yellow))
                (eshell-ls-readonly (,@fg-base1))
                (eshell-ls-special (,@fg-violet))
                (eshell-ls-symlink (,@fg-cyan))
                (eshell-ls-unreadable (,@fg-base00))
                (eshell-prompt (,@fmt-bold ,@fg-green))
                ;; font-lock
                (font-lock-builtin-face (,@fmt-none ,@fg-green)) ; Statement
                (font-lock-comment-face (,@fmt-ital ,@fg-base01)) ; Comment
                (font-lock-constant-face (,@fmt-none ,@fg-cyan)) ; Constant
                (font-lock-function-name-face ; Identifier
                 (,@fmt-none ,@fg-blue))
                (font-lock-keyword-face (,@fmt-none ,@fg-green)) ; Statement
                (font-lock-string-face (,@fmt-none ,@fg-cyan)) ; Constant
                (font-lock-type-face (,@fmt-none ,@fg-yellow)) ; Type
                (font-lock-variable-name-face ; Identifier
                 (,@fmt-none ,@fg-blue))
                (font-lock-warning-face (,@fmt-bold ,@fg-red)) ; Error
                (font-lock-doc-face (,@fmt-ital ,@fg-base01)) ; Comment
                (font-lock-doc-string-face ; Comment (XEmacs-only)
                 (,@fmt-ital ,@fg-base01))
                (font-lock-color-constant-face (,@fmt-none ,@fg-green))
                (font-lock-comment-delimiter-face ; Comment
                 (,@fmt-ital ,@fg-base01))
                (font-lock-preprocessor-face (,@fmt-none ,@fg-orange)) ; PreProc
                (font-lock-reference-face (,@fmt-none ,@fg-cyan))
                (font-lock-negation-char-face (,@fmt-none ,@fg-red))
                (font-lock-other-type-face (,@fmt-ital ,@fg-blue))
                (font-lock-regexp-grouping-construct (,@fmt-none ,@fg-orange))
                (font-lock-special-keyword-face (,@fmt-none ,@fg-red)) ; Special
                (font-lock-exit-face (,@fmt-none ,@fg-red))
                (font-lock-other-emphasized-face (,@fmt-bldi ,@fg-violet))
                (font-lock-regexp-grouping-backslash (,@fmt-none ,@fg-yellow))
                ;; info
                (info-xref (:inherit link))
                (info-xref-visited (:inherit link-visited))
                ;; org
                (org-block-background (,@bg-base03))
                (org-hide (,@fg-base03))
                (org-todo (,@fmt-bold ,@fg-orange ,@bg-base02 :family "Courier New"))
                (org-done (,@fmt-bold ,@fg-green ,@bg-base02 :family "Courier New"))
                (org-todo-kwd-face (,@fmt-bold ,@fg-base03 ,@bg-red :family "Courier New"))
                (org-done-kwd-face (,@fmt-bold ,@fg-green :family "Courier New"))
                (org-project-kwd-face (,@fg-violet ,@bg-base03 :family "Courier New"))
                (org-waiting-kwd-face (,@fg-orange ,@bg-base03 :family "Courier New"))
                (org-someday-kwd-face (,@fg-blue ,@bg-base03 :family "Courier New"))
                (org-started-kwd-face (,@fg-yellow ,@bg-base03 :family "Courier New"))
                (org-cancelled-kwd-face (,@fg-green ,@bg-base03 :family "Courier
          New"))
                (org-delegated-kwd-face (,@fg-cyan ,@bg-base03 :family "Courier New"))
                (org-default (:inherit default))
                (org-level-1 (:inherit outline-1))
                (org-level-2 (:inherit outline-2))
                (org-level-3 (:inherit outline-3))
                (org-level-4 (:inherit outline-4))
                (org-level-5 (:inherit outline-5))
                (org-level-6 (:inherit outline-6))
                (org-level-7 (:inherit outline-7))
                (org-level-8 (:inherit outline-8))
                (org-special-keyword (,@fg-green ,@bg-base02 :family "Courier New"))
                (org-drawer (,@fmt-bold ,@fg-blue))
                (org-column (,@fmt-revr ,@fg-cyan))
                (org-column-title (,@fmt-bold ,@fmt-revr))
                (org-warning (,@fg-orange))
                (org-archived (,@fg-base01))
                (org-link (,@fmt-undr ,@fg-violet))
                (org-footnote (,@fmt-undr ,@fg-violet))
                (org-ellipsis (,@fg-base01))
                (org-target (,@fmt-undr))
                (org-date (,@fmt-undr ,@fg-base01 :family "Courier New"))
                (org-date-selected (,@fmt-revr ,@fg-red))
                (org-sexp-date (,@fmt-undr ,@fg-cyan))
                (org-tag (,@fg-yellow ,@bg-base02 :family "Courier New"))
                (org-list-dt (,@fg-green))
                (org-agenda-done (,@fg-green))
                (org-headline-done (,@fg-base01))
                (org-priority (,@fmt-ital ,@fg-base01 :family "Courier New"))
                (org-checkbox (,@fmt-bold ,@fg-yellow))
                (org-table (,@fg-base01))
                (org-formula (:weight bold :slant italic ,@fg-red))
                (org-code (,@fg-green :family "Courier New"))
                (org-document-title (,@fmt-bold ,@fg-cyan))
                (org-document-info-keyword (,@fg-base01))
                (org-block (,@fg-base01))
                (org-verbatim (,@fg-cyan :family "Courier New"))
                (org-clock-overlay (,@bg-base2))
                (org-agenda-structure (,@fg-violet))
                (org-scheduled (,@fg-cyan))
                (org-scheduled-today (:weight bold :slant italic ,@fg-orange))
                (org-agenda-dimmed-todo-face (,@fg-base00))
                (org-scheduled-previously (,@fg-cyan))
                (org-upcoming-deadline (,@fmt-bldi ,@fg-red))
                (org-agenda-restriction-lock (,@fmt-revr ,@fg-base03 ,@bg-cyan))
                (org-time-grid (,@fg-yellow))
                (org-latex-and-related(,@fg-orange))
                ;; table
                (table-cell (,@fmt-none ,@fg-base0 ,@bg-back))
                ;; outline - pandocBlockQuoteLeader[1–6]
                (outline-1 (,@fg-blue))
                (outline-2 (,@fg-cyan))
                (outline-3 (,@fg-green))
                (outline-4 (,@fg-yellow))
                (outline-5 (,@fg-orange))
                (outline-6 (,@fg-red))
                (outline-7 (,@fg-magenta))
                (outline-8 (,@fg-violet))
                ;; show-paren - MatchParen
                (show-paren-match (,@fg-magenta :underline t))
                (show-paren-mismatch (,@fmt-bold ,@bg-magenta ,@fg-base03 :underline t :slant italic))
                ;; speedbar
                ;; (speedbar-button-face (,@fmt-none ,@fg-base1))
                (speedbar-button-face
                 (,@fg-base1 ,@bg-base02
                             :box (:line-width 2 :style released-button)))
                (speedbar-directory-face (,@fmt-none ,@fg-blue))
                (speedbar-file-face (,@fmt-none ,@fg-green))
                ;; (speedbar-highlight-face (,@bg-base02))
                (speedbar-highlight-face
                 (,@fmt-revr ,@fg-base1 ,@bg-base02
                             :inherit speedbar-button-face))
                ;; (speedbar-selected-face (,@fmt-undr ,@fg-yellow))
                (speedbar-selected-face (,@fmt-none ,@bg-base02 ,@fg-green))
                (speedbar-separator-face (,@fmt-stnd))
                (speedbar-tag-face (,@fmt-none ,@fg-blue))
                ;; widgets
                (widget-field
                 (,@fg-base1 ,@bg-base02 :box (:line-width 1 :color base2)
                             :inherit default))
                (widget-single-line-field (:inherit widget-field))
                ;; extra modules
                ;; -------------
                ;; alert
                (alert-urgent (,@fg-red))
                (alert-high (,@fg-orange))
                (alert-moderate (,@fg-yellow))
                (alert-normal (,@fg-green))
                (alert-low (,@fg-blue))
                (alert-trivial (,@fg-violet))
                ;; col-highlight -- Highlight the current column.
                ;; http://www.emacswiki.org/emacs/col-highlight.el
                (col-highlight (,@bg-base02))
                ;; ace-jump-mode
                (ace-jump-face-background (,@fmt-none ,@fg-base01))
                (ace-jump-face-foreground (,@fmt-bold ,@fg-red))
                ;; bm visual bookmarks
                (bm-fringe-face (,@bg-orange ,@fg-base03))
                (bm-fringe-persistent-face (,@bg-blue ,@fg-base03))
                ;; Flymake
                (flymake-errline (,@fmt-bold ,@fg-red)) ; Error
                (flymake-warnline (,@fmt-bold ,@fg-red))
                ;; column-marker
                (column-marker-1 (,@bg-base01))
                (column-marker-2 (,@bg-cyan))
                (column-marker-3 (,@bg-violet))
                ;; jabber
                (jabber-activity-face (,@fmt-bold ,@fg-red))
                (jabber-activity-personal-face (,@fmt-bold ,@fg-blue))
                (jabber-chat-error (,@fmt-bold ,@fg-red))
                (jabber-chat-prompt-foreign (,@fmt-bold ,@fg-red))
                (jabber-chat-prompt-local (,@fmt-bold ,@fg-blue))
                (jabber-chat-prompt-system (,@fmt-bold ,@fg-green))
                (jabber-chat-text-foreign (,@fg-base1))
                (jabber-chat-text-local (,@fg-base0))
                (jabber-chat-rare-time-face (,@fmt-undr ,@fg-green))
                (jabber-roster-user-away (,@fmt-ital ,@fg-green))
                (jabber-roster-user-chatty (,@fmt-bold ,@fg-orange))
                (jabber-roster-user-dnd (,@fmt-ital ,@fg-red))
                (jabber-roster-user-error
                 (:weight light :slant italic ,@fg-red))
                (jabber-roster-user-offline (,@fg-base01))
                (jabber-roster-user-online (,@fmt-bold ,@fg-blue))
                (jabber-roster-user-xa (,@fmt-ital ,@fg-magenta))
                ;; git-gutter
                (git-gutter:modified (,@fg-blue ,@bg-base02))
                (git-gutter:added (,@fg-green ,@bg-base02))
                (git-gutter:deleted (,@fg-red ,@bg-base02))
                (git-gutter:unchanged (,@fg-base01 ,@bg-base02))
                (git-gutter:separator (,@bg-base02))
                ;; gnus - these are mostly taken from mutt, not VIM
                (gnus-cite-1 (:inherit outline-1))
                (gnus-cite-2 (:inherit outline-2))
                (gnus-cite-3 (:inherit outline-3))
                (gnus-cite-4 (:inherit outline-4))
                (gnus-cite-5 (:inherit outline-5))
                (gnus-cite-6 (:inherit outline-6))
                (gnus-cite-7 (:inherit outline-7))
                (gnus-cite-8 (:inherit outline-8))
                (gnus-cite-9 (,@fg-green))
                (gnus-cite-10 (,@fg-magenta))
                (gnus-cite-11 (,@fg-base02))
                (gnus-group-mail-1 (,@fmt-bold ,@fg-base3))
                (gnus-group-mail-1-empty (,@fg-base3))
                (gnus-group-mail-2 (,@fmt-bold ,@fg-base2))
                (gnus-group-mail-2-empty (,@fg-base2))
                (gnus-group-mail-3 (,@fmt-bold ,@fg-magenta))
                (gnus-group-mail-3-empty (,@fg-magenta))
                (gnus-group-mail-low (,@fmt-bold ,@fg-base00))
                (gnus-group-mail-low-empty (,@fg-base00))
                (gnus-group-news-1 (,@fmt-bold ,@fg-base1))
                (gnus-group-news-1-empty (,@fg-base1))
                (gnus-group-news-2 (,@fmt-bold ,@fg-blue))
                (gnus-group-news-2-empty (,@fg-blue))
                (gnus-group-news-low (,@fmt-bold ,@fg-violet))
                (gnus-group-news-low-empty (,@fg-violet))
                (gnus-emphasis-highlight-words ; highlight
                 (,@fmt-none ,fg-yellow))
                (gnus-header-content (,@fmt-none ,@fg-base01)) ; hdrdefault
                (gnus-header-from (,@fmt-none ,@fg-base00)) ; header ^From
                (gnus-header-name (,@fmt-none ,@fg-base01)) ; hdrdefault
                (gnus-header-newsgroups (,@fmt-none ,@fg-base02)) ; hdrdefault
                (gnus-header-subject (,@fmt-none ,@fg-blue)) ; header ^Subject
                (gnus-server-agent (,@fmt-bold ,@fg-base3))
                (gnus-server-closed (,@fmt-ital ,@fg-base1))
                (gnus-server-denied (,@fmt-bold ,@fg-base2))
                (gnus-server-offline (,@fmt-bold ,@fg-green))
                (gnus-server-opened (,@fmt-bold ,@fg-cyan))
                (gnus-signature (,@fmt-none ,@fg-base01)) ; signature
                (gnus-splash (,@fg-base2))
                (gnus-summary-cancelled ; deleted messages
                 (,@fmt-none ,@fg-red))
                (gnus-summary-high-ancient
                 (,@fmt-bold :inherit gnus-summary-normal-ancient))
                (gnus-summary-high-read
                 (,@fmt-bold :inherit gnus-summary-normal-read))
                (gnus-summary-high-ticked
                 (,@fmt-bold :inherit gnus-summary-normal-ticked))
                (gnus-summary-high-undownloaded
                 (,@fmt-bold :inherit gnus-summary-normal-undownloaded))
                (gnus-summary-high-unread
                 (,@fmt-bold :inherit gnus-summary-normal-unread))
                (gnus-summary-low-ancient
                 (,@fmt-ital :inherit gnus-summary-normal-ancient))
                (gnus-summary-low-read
                 (,@fmt-ital :inherit gnus-summary-normal-ancient))
                (gnus-summary-low-unread
                 (,@fmt-ital :inherit gnus-summary-normal-unread))
                (gnus-summary-low-ticked
                 (,@fmt-ital :inherit gnus-summary-normal-ancient))
                (gnus-summary-low-undownloaded
                 (,@fmt-ital :inherit gnus-summary-normal-ancient))
                (gnus-summary-normal-ancient ; old messages
                 (,@fmt-none ,@fg-blue))
                (gnus-summary-normal-read ; read messages
                 (,@fmt-none ,@fg-base01))
                (gnus-summary-normal-ticked (,@fmt-none ,@fg-red)) ; flagged
                (gnus-summary-normal-undownloaded (,@fmt-none ,@fg-base2))
                (gnus-summary-normal-unread ; unread messages
                 (,@fmt-none ,@fg-blue))
                (gnus-summary-selected ; indicator
                 (,@fmt-none ,@fg-base03 ,@bg-yellow))
                ;;helm
                (helm-apt-deinstalled (,@fg-base01))
                (helm-apt-installed (,@fg-green))
                (helm-bookmark-addressbook (,@fg-blue))
                (helm-bookmark-directory (:inherit helm-ff-directory))
                (helm-bookmark-file (:inherit helm-ff-file))
                (helm-bookmark-gnus (,@fg-cyan))
                (helm-bookmark-info (,@fg-green))
                (helm-bookmark-man (,@fg-violet))
                (helm-bookmark-w3m (,@fg-yellow))
                (helm-bookmarks-su (,@fg-orange))
                (helm-buffer-directory (,@fg-blue))
                (helm-buffer-not-saved (,@fg-orange))
                (helm-buffer-process (,@fg-magenta))
                (helm-buffer-saved-out (,@fmt-revr ,@fg-red ,@bg-back))
                (helm-buffer-size (,@fg-base01))
                (helm-candidate-number (,@fmt-bold ,@bg-base02 ,@fg-base1))
                (helm-emms-playlist (,@fmt-none ,@fg-base01))
                (helm-etags+-highlight-face (:inherit highlight))
                (helm-ff-directory (,@bg-back ,@fg-blue))
                (helm-ff-dotted-directory (:inherit helm-ff-directory))
                (helm-ff-dotted-symlink-directory (:inherit helm-ff-dotted-directory))
                (helm-ff-executable (,@fmt-bold ,@fg-green))
                (helm-ff-file (:inherit default))
                (helm-ff-invalid-symlink (,@bg-base02 ,@fg-red))
                (helm-ff-prefix (,@fmt-revr ,@fg-yellow))
                (helm-ff-symlink (,@fmt-bold ,@fg-cyan))
                (helm-gentoo-match (:inherit helm-match))
                (helm-grep-cmd-line (:inherit diff-added))
                (helm-grep-file (,@fmt-undr ,@fg-cyan))
                (helm-grep-finish (,@fg-green))
                (helm-grep-lineno (,@fg-orange))
                (helm-grep-match (:inherit helm-match))
                (helm-grep-running (,@fg-red))
                (helm-source-header (:inherit header-line))
                (helm-helper (:background yellow :foreground base3))
                (helm-header (:background base02 :foreground cyan))
                (helm-history-deleted (:inherit helm-ff-invalid-symlink))
                (helm-history-remote (,@fg-red))
                (helm-lisp-completion-info (,@fg-base0))
                (helm-lisp-show-completion (,@fmt-bold ,@fg-yellow ,@bg-base02))
                (helm-ls-git-added-copied-face (,@fg-green))
                (helm-ls-git-conflict-face (,@fmt-bold ,@fg-red))
                (helm-ls-git-deleted-and-staged-face (,@fmt-ital ,@fg-base01))
                (helm-ls-git-deleted-not-staged-face (,@fmt-bold ,@fg-green))
                (helm-ls-git-modified-and-staged-face (,@fmt-ital ,@fg-base01))
                (helm-ls-git-modified-not-staged-face (,@fmt-ital ,@fg-base01))
                (helm-ls-git-renamed-modified-face (,@fg-green))
                (helm-ls-git-untracked-face (,@fg-red))
                (helm-M-x-key (,@fmt-none ,@fg-red))
                (helm-match (:foreground base3 :underline t))
                (helm-moccur-buffer (,@fmt-undr ,@fg-cyan))
                (helm-selection (:background base02))
                (helm-selection-line (:inherit secondary-selection))
                (helm-separator (,@fg-red))
                (helm-time-zone-current (,@fg-green))
                (helm-time-zone-home (,@fg-red))
                (helm-visible-mark (,@fmt-bold ,@bg-back ,@fg-magenta))
                (helm-w3m-bookmarks (:inherit helm-bookmark-w3m))
                ;; markdown
                (markdown-bold-face (:inherit bold))
                (markdown-header-delimiter-face (:inherit shadow))
                (markdown-header-face (:inherit outline-1))
                (markdown-header-face-1 (:inherit outline-1))
                (markdown-header-face-2 (:inherit outline-2))
                (markdown-header-face-3 (:inherit outline-3))
                (markdown-header-face-4 (:inherit outline-4))
                (markdown-header-face-5 (:inherit outline-5))
                (markdown-header-face-6 (:inherit outline-6))
                (markdown-header-rule-face (:inherit shadow))
                (markdown-italic-face (:inherit italic))
                (markdown-link-face (:inherit shadow))
                (markdown-link-title-face (:inherit link))
                (markdown-url-face (:inherit link))
                ;; Message
                (message-mml (,@fg-blue))
                (message-cited-text (,@fg-base2))
                (message-separator (,@fg-base3))
                (message-header-xheader (,@fg-violet))
                (message-header-name (,@fg-cyan))
                (message-header-other (,@fg-red))
                (message-header-newsgroups (,@fmt-bldi ,@fg-yellow))
                (message-header-subject (,@fg-base00))
                (message-header-cc (,@fmt-bold ,@fg-green))
                (message-header-to (,@fmt-bold ,@fg-base1))
                ;; minimap
                (minimap-active-region-background (,@bg-base02))
                (minimap-semantic-function-face (,bg-base3))
                (minimap-semantic-type-face (,bg-base3))
                (minimap-semantic-variable-face (,bg-base3))
                ;; parenface
                (parenface-bracket-face (:inherit shadow))
                (parenface-curly-face (:inherit shadow))
                (parenface-paren-face (:inherit shadow))
                ;; paren-face
                (parenthesis (:inherit shadow))
                ;; rainbow-delimiters
                (rainbow-delimiters-depth-1-face (:inherit outline-1))
                (rainbow-delimiters-depth-2-face (:inherit outline-2))
                (rainbow-delimiters-depth-3-face (:inherit outline-3))
                (rainbow-delimiters-depth-4-face (:inherit outline-4))
                (rainbow-delimiters-depth-5-face (:inherit outline-5))
                (rainbow-delimiters-depth-6-face (:inherit outline-6))
                (rainbow-delimiters-depth-7-face (:inherit outline-7))
                (rainbow-delimiters-depth-8-face (:inherit outline-8))
                (rainbow-delimiters-depth-9-face (,@fg-green))
                ;; slime
                (slime-error-face (,@fmt-revr ,@fg-red)) ; ErrorMsg
                (slime-note-face (,@fg-yellow))
                (slime-repl-inputted-output-face (,@fg-red))
                (slime-repl-output-mouseover-face (:box (:color base3)))
                (slime-style-warning-face (,@fmt-bold ,@fg-orange))
                (slime-warning-face (,@fmt-bold ,@fg-red)) ; WarningMsg
                ;; smartparens
                (sp-pair-overlay-face (,@bg-base02))
                (sp-wrap-overlay-face (,@bg-base02))
                (sp-wrap-tag-overlay-face (,@bg-base02))
                (sp-show-pair-match-face (,@fg-magenta ,@bg-back))
                (sp-show-pair-mismatch-face (,@bg-red ,@fg-base02))
                ;; whitespace
                (whitespace-empty (,@fg-red))
                (whitespace-hspace (,@fg-orange))
                (whitespace-indentation (,@fg-base02))
                (whitespace-space (,@fg-base02))
                (whitespace-space-after-tab (,@fg-cyan))
                (whitespace-space-before-tab (,@fmt-bold ,@fg-red))
                (whitespace-tab (,@fg-base02))
                (whitespace-trailing (,@fmt-bold ,@fg-red ,@bg-base02))
                (whitespace-highlight-face (,@fg-red ,@bg-blue))
                (whitespace-line (,@fg-magenta))
                (whitespace-newline (:inherit shadow :slant normal))
                ;; writegood
                (writegood-weasels-face (,@fmt-curl-cyan ,@fg-cyan))
                (writegood-passive-voice-face (,@fg-magenta))
                (writegood-duplicates-face (:inherit error))
                ;; rcirc
                (rcirc-my-nick (,@fg-blue))
                (rcirc-nick-in-message (,@fg-orange))
                (rcirc-other-nick (,@fg-green))
                (rcirc-prompt (,@fg-yellow))
                (rcirc-bright-nick (,@fg-magenta))
                (rcirc-server (,@fg-base1))
                (rcirc-timestamp (,@fg-base01))
                ;;font-latex
                (font-latex-warning-face (,@fg-red))
                (font-latex-sectioning-5-face (,@fg-violet))
                ;;flyspell
                (flyspell-incorrect (,@fmt-curl-orange)) ; SpellBad
                (flyspell-duplicate (,@fmt-curl-yellow))
                ;; rst-mode
                (rst-level-1 (:inherit outline-1))
                (rst-level-2 (:inherit outline-2))
                (rst-level-3 (:inherit outline-3))
                (rst-level-4 (:inherit outline-4))
                (rst-level-5 (:inherit outline-5))
                (rst-level-6 (:inherit outline-6))
                ;;ansi-term
                (term-color-black (,@fg-base02))
                (term-color-red (,@fg-red))
                (term-color-green (,@fg-green))
                (term-color-yellow (,@fg-yellow))
                (term-color-blue (,@fg-blue))
                (term-color-magenta (,@fg-magenta))
                (term-color-cyan (,@fg-cyan))
                (term-color-white (,@fg-base00))
                ;; company
                (company-tooltip (,@fg-base00 ,@bg-base02))
                (company-tooltip-selection (,@fg-green ,@bg-base02))
                (company-tooltip-mouse (,@fg-base1 ,@bg-base02))
                (company-tooltip-common (,@fg-blue ,@bg-base02 ,@fmt-undr))
                (company-tooltip-common-selection (,@fg-green ,@bg-base02 ,@fmt-undr))
                (company-tooltip-annotation (,@fg-yellow ,@bg-base02))
                (company-scrollbar-fg (,@bg-base0))
                (company-scrollbar-bg (,@bg-base02))
                (company-preview (,@bg-green))
                (company-preview-common (,@bg-base02))
                (company-template-field (,@fg-base03 ,@bg-yellow))
                ;; hydra
                (hydra-face-red (,@fmt-bold ,@fg-red))
                (hydra-face-blue (,@fmt-bold ,@fg-blue))
                (hydra-face-amaranth (,@fmt-bold ,@fg-orange))
                (hydra-face-pink (,@fmt-bold ,@fg-magenta))
                (hydra-face-teal (,@fmt-bold ,@fg-cyan))
                ;; guide-key
                (guide-key/prefix-command-face (,@fg-blue))
                (guide-key/highlight-command-face (,@fg-orange))
                (guide-key/key-face (,@fg-green))
                ;; magit
                (magit-log-sha1 (,@fg-red))
                (magit-branch (,@fg-yellow))
                (magit-tag (,@fg-green))
                (magit-log-author (,@fg-cyan))
                (magit-log-head-label-remote (,@fg-green))
                (magit-log-head-label-tags (,@fg-orange))
                (magit-log-head-label-local (,@fg-yellow))
                (magit-log-head-label-head (,@fg-violet))
                (magit-process-ok (,@fg-green :inherit magit-section-title))
                (magit-process-ng (,@fg-red :inherit magit-section-title))
                (magit-blame-heading (,@bg-base01 ,@fg-base03 :slant normal))
                (magit-blame-date (,@bg-base01 ,@fg-base0 :slant italic))
                (magit-blame-name (,@bg-base01 ,@fg-base03 :slant normal))

                ;; magit (copied from other solarized theme)
                (magit-section-highlight (,@bg-base03))
                (magit-section-heading (,@fg-yellow ,@fmt-bold))
                (magit-section-heading-selection (,@fg-orange ,@fmt-bold))
                (magit-diff-file-heading (,@bg-base02 ,@fmt-bold))
                (magit-diff-file-heading-highlight (,@bg-base03 ,@fmt-bold))
                (magit-diff-file-heading-selection (,@bg-base03 ,@fg-orange ,@fmt-bold))
                (magit-diff-hunk-heading (,@bg-base02))
                (magit-diff-hunk-heading-highlight (,@bg-base03))
                (magit-diff-hunk-heading-selection (,@bg-base02 ,@fg-orange))
                (magit-diff-lines-heading (,@bg-orange ,@fg-base3))
                (magit-diff-context (,@bg-base02 ,@fg-base01))
                (magit-diff-context-highlight (,@bg-base03 ,@fg-base01))
                (magit-diff-added (,@bg-base02 ,@fg-green))
                (magit-diff-added-highlight (,@bg-base03 ,@fg-green))
                (magit-diff-removed (,@bg-base02 ,@fg-red))
                (magit-diff-removed-highlight (,@bg-base03 ,@fg-red))
                (magit-diffstat-added (,@fg-green))
                (magit-diffstat-removed (,@fg-red))
                (magit-popup-heading (,@fg-base1 :weight normal))
                (magit-popup-key (,@fg-base1 ,@fmt-bold))
                (magit-popup-argument (,@fg-base1 ,@fmt-bold))
                (magit-popup-disabled-argument (,@fg-base01 :weight normal))
                (magit-popup-option-value (,@fg-base1 ,@fmt-bold))
                (magit-process-ok (,@fg-green ,@fmt-bold))
                (magit-process-ng (,@fg-orange ,@fmt-bold))
                (magit-log-author (,@fg-base01 ,@fmt-bold))
                (magit-log-date (,@fg-base01))
                (magit-log-graph (,@fg-base1))
                (magit-sequence-pick (,@fg-yellow))
                (magit-sequence-stop (,@fg-green))
                (magit-sequence-part (,@fg-yellow))
                (magit-sequence-head (,@fg-blue))
                (magit-sequence-drop (,@fg-red))
                (magit-sequence-done (,@fg-base01))
                (magit-sequence-onto (,@fg-base01))
                (magit-bisect-good (,@fg-green))
                (magit-bisect-skip (,@fg-yellow))
                (magit-bisect-bad (,@fg-red))
                (magit-dimmed (,@fg-base01))
                (magit-hash (,@fg-blue))
                (magit-tag (,@fg-cyan ,@fmt-bold))
                (magit-branch-remote (,@fg-green ,@fmt-bold))
                (magit-branch-local (,@fg-blue ,@fmt-bold))
                (magit-branch-current (,@fg-blue ,@fmt-bold))
                (magit-head (,@fg-blue ,@fmt-bold))
                (magit-refname (,@bg-base02 ,@fg-base01 ,@fmt-bold))
                (magit-refname-stash (,@bg-base02 ,@fg-base01 ,@fmt-bold))
                (magit-refname-wip (,@bg-base02 ,@fg-base01 ,@fmt-bold))
                (magit-signature-good (,@fg-green))
                (magit-signature-bad (,@fg-red))
                (magit-signature-untrusted (,@fg-yellow))
                (magit-cherry-unmatched (,@fg-cyan))
                (magit-cherry-equivalent (,@fg-magenta))
                (magit-reflog-commit (,@fg-green))
                (magit-reflog-amend (,@fg-magenta))
                (magit-reflog-merge (,@fg-green))
                (magit-reflog-checkout (,@fg-blue))
                (magit-reflog-reset (,@fg-red))
                (magit-reflog-rebase (,@fg-magenta))
                (magit-reflog-cherry-pick (,@fg-green))
                (magit-reflog-remote (,@fg-cyan))
                (magit-reflog-other (,@fg-cyan))


                ;; undo-tree
                (undo-tree-visualizer-current-face (,@fg-orange))
                (undo-tree-visualizer-default-face (:inherit shadow))
                (undo-tree-visualizer-active-branch-face (:inherit default))
                (undo-tree-visualizer-unmodified-face (,@fg-cyan))
                (undo-tree-visualizer-register-face (,@fg-yellow))
                ;; haskell
                (haskell-keyword-face (,@fg-cyan))

                ;; Matt - misc
                (italic (,@fmt-ital ,@fg-base1))
                (bold (,@fmt-bold ,@fg-base2))
                (org-agenda-date (,@fg-blue))
                (org-agenda-date-today (,@fmt-bldi ,@fg-orange))
                (org-block-begin-line (,@fg-base01))
                (org-block-end-line (,@fg-base01))

                ;; SML
                (sml/filename (,@fg-base1 :weight bold))
                (sml/folder (,@fg-base01))
                (sml/prefix (:foreground unspecified))
                (sml/git (:foreground unspecified))
                (sml/process (:weight bold))
                (sml/sudo (,@fg-orange :weight bold))
                (sml/read-only (,@fg-cyan))
                (sml/outside-modified (,@fg-cyan))
                (sml/modified (,@fg-cyan))
                (sml/vc (,@fg-cyan))
                (sml/vc-edited (,@fg-yellow))
                (sml/charging (,@fg-base1))
                (sml/discharging (,@fg-base1 :weight bold))
                (sml/modes (,@fg-cyan))

                ;; ag-mode
                (ag-match-face (:inherit helm-match))

                ;; flycheck
                (flycheck-fringe-error (,@fg-orange ,@bg-base03))  ; These
                                         ; control the bitmap colours in the fringe
                (flycheck-fringe-warning (,@fg-violet ,@bg-base02))
                (flycheck-fringe-success (,@fg-green))
                (flycheck-error (,@fmt-curl-orange))
                (flycheck-warning (,@fmt-curl-violet))
                (flycheck-info (,@fmt-curl-green))

                ;; elscreen
                (elscreen-tab-background-face 
                  (,@fg-base02 ,@bg-base0 
                  :box (:line-width 1 :color base02 :style released-button)))
                (elscreen-tab-control-face 
                  (:inherit elscreen-tab-background-face
                   ,@fg-base02 ,@bg-base0))
                (elscreen-tab-current-screen-face 
                  (:inherit elscreen-tab-background-face
                    ,@fg-base03 ,@bg-blue))
                (elscreen-tab-other-screen-face 
                  (:inherit elscreen-tab-background-face))

                ;; neotree
                (neo-dir-link-face (,@fg-blue))
                (neo-banner-face (,@fg-cyan))
                (neo-root-dir-face (,@fg-magenta ,@bg-base02))
                (neo-file-link-face (,@fg-base1))
                (neo-header-face (,@fg-base01))
                (neo-button-face (,@fg-green))
                (neo-expand-btn-face (,@fg-base01))

                ;; powerline
                (mode-line
                 (,@fg-base0 ,@bg-base03
                             :inverse-video nil
                             :box (:line-width 1 :color base02 :style released-button)
                             :underline nil
                             ))
                (powerline-active1 (,@fg-cyan ,@bg-base02 :inherit mode-line))
                (powerline-active2 (,@fg-base1 ,@bg-base01 :inherit mode-line))
                (md/powerline-normal (,@fg-base2 ,@bg-base01 :inherit mode-line))
                (md/powerline-insert (,@fg-base2 ,@bg-yellow :inherit mode-line))
                (md/powerline-visual (,@fg-base2 ,@bg-cyan :inherit mode-line))
                (md/powerline-replace (,@fg-base2 ,@bg-orange :inherit mode-line))
                (md/powerline-emacs (,@fg-base2 ,@bg-magenta :inherit mode-line))

                (eyebrowse-mode-line-active (,@fg-magenta ,@bg-base02))
                (eyebrowse-mode-line-inactive (,@fg-base01 ,@bg-base02))
                (eyebrowse-mode-line-delimiters (,@fg-base01 ,@bg-base02))
                (eyebrowse-mode-line-separator (,@fg-base01 ,@bg-base02))

                (mode-line-inactive
                 (,@fg-base02 ,@bg-base0
                              :inverse-video nil
                              :box (:line-width -1 :color base01 :style nil)
                              :underline nil
                              ))
                (powerline-inactive1 (,@fg-base02 ,@bg-base0 :inherit mode-line-inactive))
                (powerline-inactive2 (,@fg-base01 ,@bg-base0 :inherit mode-line-inactive))
                (md/powerline-inactive (,@fg-base01 ,@bg-base0 :inherit mode-line-inactive))

                (md/modeline-flycheck-error
                 (,@fg-base2 ,@bg-orange))
                (md/modeline-flycheck-warning
                 (,@fg-base2 ,@bg-violet))

                (mode-line-buffer-id (,@fg-magenta))  ; Not sure what this does

                (sh-quoted-exec (,@fg-orange))


                ;; ediff
                (ediff-current-diff-A (,@bg-base02 ,@fg-orange :weight bold))
                (ediff-current-diff-B (,@bg-base02 ,@fg-yellow :weight bold))
                (ediff-current-diff-C (,@bg-base02 ,@fg-cyan :weight bold))
                (ediff-fine-diff-A (,@bg-base02 ,@fg-red :weight normal))
                (ediff-fine-diff-B (,@bg-base02 ,@fg-green :weight normal))
                (ediff-fine-diff-C (,@bg-base02 ,@fg-blue :weight normal))
                (ediff-even-diff-A (,@bg-base01 ,@fg-base3))
                (ediff-odd-diff-A (,@bg-base01 ,@fg-base03))
                (ediff-even-diff-B (,@bg-base01 ,@fg-base03))
                (ediff-odd-diff-B (,@bg-base01 ,@fg-base03))
                (ediff-even-diff-C (,@bg-base01 ,@fg-base0))
                (ediff-odd-diff-C (,@bg-base01 ,@fg-base03))

                ;; mu4e
                (mu4e-header-face (,@fg-base01 :weight normal
                                               :box (:line-width 1 :color base03
                :style nil)))
                (mu4e-unread-face (:inherit mu4e-header-face
                                            ,@fg-base1
                                            ,@bg-base02
                                            ,@fmt-bold
                                            ))
                (mu4e-replied-face (:inherit mu4e-header-face))
                (mu4e-flagged-face (:inherit mu4e-header-face))
                (mu4e-draft-face (:inherit mu4e-header-face ,@fg-cyan))
                (mu4e-header-highlight-face
                 (:inherit mu4e-header-face ,@fg-base03 ,@bg-cyan))
                
                ;; help-fns+
                (describe-variable-value (,@fg-cyan))

                ;; highlight-thing
                (highlight-thing (,@fg-magenta))

                ;; edebug-x
                (hi-edebug-x-stop (,@bg-base02 ,@fg-cyan))
                (hi-edebug-x-debug-line (,@bg-base02 ,@fg-base1))

                (fic-face (:inherit font-lock-comment-face ,@fg-base2 ,@bg-base02))
                (fic-author-face (:inherit fic-face)))))))


(defun solarized-get-definition (color-name)
  "Translate given colour name into a value from solarized-colors."
  (when (eq 'light (frame-parameter nil 'background-mode))
    (setq color-name
          (cl-case color-name
            (base03 'base3)
            (base02 'base2)
            (base01 'base1)
            (base00 'base0)
            (base0 'base00)
            (base1 'base01)
            (base2 'base02)
            (base3 'base03)
            (otherwise color-name))))
  (nth (cond
        ((and (not (display-graphic-p)) (= solarized-termcolors 16)) 4)
        ((and (not (display-graphic-p)) (not (= solarized-termcolors 16)) 3))
        (solarized-degrade     3)
        (solarized-broken-srgb 2)
        (t                     1))
       (assoc color-name solarized-colors)))

(defun solarized-variable-definitions ()
  (list

   '(org-priority-faces 
     `((?A . (:foreground ,(solarized-get-definition 'red)
              :background ,(solarized-get-definition 'base02)
              :family "Courier New"
                          ))
       (?B . (:foreground ,(solarized-get-definition 'base01)
              :background ,(solarized-get-definition 'base02)
              :family "Courier New"))))

     '(org-todo-keyword-faces
       `(("NOW" .
          (:foreground ,(solarized-get-definition 'orange)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("BLOCKED" .
          (:foreground ,(solarized-get-definition 'magenta)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("HOLD" .
          (:foreground ,(solarized-get-definition 'base01)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("DONE" .
          (:foreground ,(solarized-get-definition 'base01)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("DROPPED" .
          (:foreground ,(solarized-get-definition 'base01)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("A_OPEN" .
          (:foreground ,(solarized-get-definition 'blue)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))
         ("A_CLOSED" .
          (:foreground ,(solarized-get-definition 'base01)
           :family "Courier New"
           :background ,(solarized-get-definition 'base02)))))

    '(fci-rule-color (solarized-get-definition 'base02))

    '(evil-emacs-state-cursor (list (solarized-get-definition 'cyan) 'box))

    '(evil-default-cursor t)

    '(evil-normal-state-cursor (list (solarized-get-definition 'base1) 'box))
    '(evil-visual-state-cursor (list (solarized-get-definition 'base1) 'box))
    '(evil-motion-state-cursor (list (solarized-get-definition 'base1) 'box))

    '(evil-insert-state-cursor (list (solarized-get-definition 'base1) '(bar . 2)))
    '(evil-replace-state-cursor (list (solarized-get-definition 'red) '(hbar . 2)))
    '(evil-operator-state-cursor (list (solarized-get-definition 'violet) 'box))))

(defvar solarized--comment-hidden-p nil)
(defun solarized-toggle-comment-visibility ()
  "Toggle the font-comment-lock-face between a more-visible format and a
less-visible format"
  (interactive)
  (if solarized--comment-hidden-p
      (progn
        (set-face-attribute 'font-lock-comment-face nil
                            :foreground (solarized-get-definition 'base01))  ; visible
        (setq solarized--comment-hidden-p nil))
    (set-face-attribute 'font-lock-comment-face nil
                        :foreground (solarized-get-definition 'base02))  ; less visible
    (setq solarized--comment-hidden-p t))
  (font-lock-fontify-buffer))

(defun solarized-load-theme ()
  (interactive)
  (let ((dark-mode-p (eq 'dark (frame-parameter nil 'background-mode))))
    (load-theme 'solarized t)  ; Reloads variables
    (if dark-mode-p
        (solarized-enable-theme 'dark))
    (if (fboundp 'powerline-reset)
        (powerline-reset))
    (font-lock-fontify-buffer)))

(defun solarized-enable-theme (mode)
  (interactive "Slight or dark? ")
  (solarized-set-frame-bg-mode mode)
  (enable-theme 'solarized)
  ;; Reset powerline separator cache
  (if (fboundp 'powerline-reset)
      (powerline-reset))
  (font-lock-fontify-buffer))

(defun solarized-toggle-theme-mode ()
  (interactive)
  (if (eq 'dark (frame-parameter nil 'background-mode))
      (progn
        (solarized-enable-theme 'light))
    (solarized-enable-theme 'dark)))

(defun solarized-set-frame-bg-mode (mode)
  ;; https://github.com/sellout/emacs-color-theme-solarized/issues/142
  ;; To switch to light mode in terminal, you have to use set-terminal-parameter.
  (set-frame-parameter nil 'background-mode mode)
  (set-terminal-parameter nil 'background-mode mode))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(defmacro create-solarized-theme (name description color-definitions variable-definitions)
  `(progn
     (deftheme ,name ,description)
     (apply 'custom-theme-set-faces
            ',name ,color-definitions)
     (apply 'custom-theme-set-variables
            ',name ,variable-definitions)

     (provide-theme ',name)))

(provide 'solarized-definitions)
;;; solarized-definitions.el ends here
