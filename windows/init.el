                                        ; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ------------------------------------------------------------------------
;; @ package manager
;;------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ------------------------------------------------------------------------
;; @ るびきちおすすめ設定
;; ------------------------------------------------------------------------

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; ファイルを開いた位置を保存する
;;(require 'saveplace)
;;(setq-default save-place t)
;;(setq save-place-file (concat user-emacs-directory "places"))

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ------------------------------------------------------------------------
;; @ Windowのカラー
(set-frame-parameter nil 'alpha 100)

;; ------------------------------------------------------------------------
;; @ coding system

;; 日本語入力のための設定
(set-keyboard-coding-system 'cp932)

(prefer-coding-system 'utf-8-dos)
(set-file-name-coding-system 'cp932)
(setq default-process-coding-system '(cp932 . cp932))

;; ------------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format "%b")
;;(add-to-list 'default-frame-alist '(alpha . (1.0 1.0)))

;; ------------------------------------------------------------------------
;; @ buffer

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; ------------------------------------------------------------------------
;; @ modeline

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; ------------------------------------------------------------------------
;; @ cursor

(defvar scroll-speedup-count 5)
(defvar scroll-speedup-rate 2)
(defvar scroll-speedup-time 200)

(defvar scroll-step-default 1)
(defvar scroll-step-count 1)
(defvar scroll-speedup-zero (current-time))

(defun scroll-speedup-setspeed ()
  (let* ((now (current-time))
         (min (- (car now) (car scroll-speedup-zero)))
         (sec (- (car (cdr now)) (car (cdr scroll-speedup-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr scroll-speedup-zero))))
                  1000))
         (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (if (> lag scroll-speedup-time)
        (progn
          (setq scroll-step-default 1)
          (setq scroll-step-count 1))
      (setq scroll-step-count (+ 1 scroll-step-count)))
    (setq scroll-speedup-zero (current-time))))

(defun scroll-speedup-next-line (arg)
  (if (= (% scroll-step-count scroll-speedup-count) 0)
      (setq scroll-step-default
            (+ scroll-speedup-rate scroll-step-default)))
  (if (string= arg 'next)
      (forward-line scroll-step-default)
    (forward-line (* -1 scroll-step-default))))

(defadvice next-line
  (around next-line-speedup activate)
  (if (and (string= last-command 'next-line)
           (interactive-p))
      (progn
        (scroll-speedup-setspeed)
        (scroll-speedup-next-line 'next))
    (setq scroll-step-default 1)
    (setq scroll-step-count 1)
    ad-do-it))

(defadvice previous-line
  (around previous-line-speedup activate)
  (if (and (string= last-command 'previous-line)
           (interactive-p))
      (progn
        (scroll-speedup-setspeed)
        (scroll-speedup-next-line 'previous))
    (setq scroll-step-default 1)
    (setq scroll-step-count 1)
    ad-do-it))
;; ------------------------------------------------------------------------
;; @ default setting

;; 起動メッセージの非表示
(setq inhibit-startup-message t)

;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;; ------------------------------------------------------------------------
;; @ backup

;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;; 変更ファイルの番号つきバックアップ
(setq version-control nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 30)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 500)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)

;; 上書き時の警告表示
;; (setq trim-versions-without-asking nil)

;; 古いバックアップファイルの削除
(setq delete-old-versions t)

;; ------------------------------------------------------------------------
;; @ key bind
;;------------------------------------------------------------------------------

;; 標準キーバインド変更
;;(global-set-key "\C-z"          'scroll-down)

;; By an unknown contributor
(global-set-key "\M-p" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; C-hで文字削除
(global-set-key "\C-h" 'delete-backward-char)

;; 再読み込み
(global-set-key (kbd "\C-x g") 'revert-buffer)

;; ------------------------------------------------------------------------
;; @ print
;;------------------------------------------------------------------------------

(setq ps-print-color-p t
      ps-lpr-command "gswin32c.exe"
      ps-multibyte-buffer 'non-latin-printer
      ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
      printer-name nil
      ps-printer-name nil
      ps-printer-name-option nil
      ps-print-header nil          ; ヘッダの非表示
      )

;; ------------------------------------------------------------------------
;; @ shell
;;------------------------------------------------------------------------------
(require 'shell)
(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "-c")
(setq shell-file-name "bash.exe")

;; (M-! and M-| and compile.el)
(setq shell-file-name "bash.exe")
(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)

;;------------------------------------------------------------------------------
;; shellモードの時の^M抑制
;;------------------------------------------------------------------------------
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;;------------------------------------------------------------------------------
;; shell-modeでの補完 (for drive letter)
;;------------------------------------------------------------------------------
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

;;------------------------------------------------------------------------------
;; エスケープシーケンス処理の設定
;;------------------------------------------------------------------------------
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(setq shell-mode-hook
      (function
       (lambda ()
         ;; シェルモードの入出力文字コード
         (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
         (set-buffer-file-coding-system    'utf-8-unix)
         )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "ＭＳ ゴシック" :foundry "outline" :slant normal :weight normal :height 98 :width normal))))
 '(font-lock-comment-face ((t (:foreground "gray" :slant oblique))))
 '(font-lock-doc-face ((t (:foreground "gray" :slant oblique))))
 '(jaspace-highlight-tab-face ((t (:foreground "purple" :underline t))) t)
 '(js2-error ((t (:foreground "red" :weight bold))) t)
 '(js2-warning ((t (:background "snow" :foreground "peru" :weight bold))) t)
 '(nxml-comment-content-face ((t (:foreground "red"))) t)
 '(nxml-comment-delimiter-face ((t (:foreground "red"))) t)
 '(nxml-delimited-data-face ((t (:foreground "DarkViolet"))) t)
 '(nxml-delimiter-face ((t (:foreground "blue"))) t)
 '(nxml-element-colon-face ((t (:foreground "LightSteelBlue"))) t)
 '(nxml-element-local-name-face ((t (:inherit nxml-name-face :foreground "blue"))) t)
 '(nxml-name-face ((t (:foreground "dark green"))) t)
 '(nxml-ref-face ((t (:foreground "DarkGoldenrod"))) t)
 '(nxml-tag-slash-face ((t (:inherit nxml-name-face :foreground "blue"))) t)
 '(trailing-whitespace ((t (:background "LightPink1")))))


;; ------------------------------------------------------------------------
;; @ My settings
;;------------------------------------------------------------------------------

;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20140803.2118")
(add-to-list 'load-path "~/.emacs.d/elpa/js2-mode-20140803.2031")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20140207.1702")
(add-to-list 'load-path "~/.emacs.d/elpa/psvn-20120401.1440")

;;------------------------------------------------------------------------------
;; メニュー非表示
;;------------------------------------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;------------------------------------------------------------------------------
;; 対応括弧ハイライト
;;------------------------------------------------------------------------------
(show-paren-mode t)

;;------------------------------------------------------------------------------
;; 縦分割抑制
;;------------------------------------------------------------------------------
(setq split-width-threshold nil)

;;------------------------------------------------------------------------------
;; インデント設定
;;------------------------------------------------------------------------------
;; indent by tab-width 8
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq js-indent-level 2)

;;------------------------------------------------------------------------------
;; 行末の空白を表示
;;------------------------------------------------------------------------------
(setq-default show-trailing-whitespace t)

;;------------------------------------------------------------------------------
;; タブ, 全角スペース、改行直前の半角スペースを表示する
;;------------------------------------------------------------------------------
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'nxml-mode
                                      'web-mode
                                      'c-mode
                                      'c++-mode
                                      'csharp-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))
                                 (t (:foreground "purple"
                                     :background "unspecified"
                                     :strike-through nil
                                     :underline t))))))))

;;------------------------------------------------------------------------------
;; HTML5対応
;; web-mode
;;------------------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)

;;------------------------------------------------------------------------------
;; auto install
;;------------------------------------------------------------------------------
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する
  ;; 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp")

  ;; EmacsWiki に登録されている elisp の名前を取得する
  ;; (auto-install-update-emacswiki-package-name t)

  ;; 必要であればプロキシの設定を行う
  ;;  (setq url-proxy-services '(("http" . "localhost:8080")))

  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup)
  )

;;------------------------------------------------------------------------------
;; anything setting
;;------------------------------------------------------------------------------
(when (require 'anything-startup nil t)
  (global-set-key (kbd "\C-x b") 'anything)
  )
;;------------------------------------------------------------------------------
;; YASnippet
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20140729.1240")
(when (require 'yasnippet nil t)
  (yas--initialize)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
          "~/.emacs.d/elisp/yasnippet/snippets" ;; 最初から入っていたスニペット(省略可能)
          ))
  ;;  (yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets")
  ;;  (yas/load-directory "~/.emacs.d/elisp/yasnippet/extras/imported")
  (yas/global-mode 1)
  )

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;;------------------------------------------------------------------------------
;; popup-el
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/popup-el")

;;------------------------------------------------------------------------------
;; auto-complete depend on popup-el
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
  (setq ac-auto-start 4) ;; 4文字以上から補完を始める
  (add-to-list 'ac-sources 'ac-source-yasnippet) ;; 常にYASnippetを補完候補に
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete-20140803.2118/dict") ;; 辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/elisp/auto-complete-20140803.2118/ac-comphist.dat") ;; 補完履歴のキャッシュ先

  )

;;------------------------------------------------------------------------------
;; js2-mode
;;------------------------------------------------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
(setq-default js2-basic-offset 2)
(setq ac-modes (append '(js2-mode)))
(add-hook 'js2-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;;------------------------------------------------------------------------------
;; php-mode
;;------------------------------------------------------------------------------
(require 'php-mode)
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;------------------------------------------------------------------------------
;; subversion
;;------------------------------------------------------------------------------
(require 'psvn)
(setq process-coding-system-alist '(("svn" . utf-8)))
(setq default-file-name-coding-system 'cp932)
(setq svn-status-svn-file-coding-system 'utf-8)
(autoload 'svn-status "psvn" nil t)
(add-hook 'svn-pre-parse-status-hook 'svn-status-parse-fixup-externals-full-path)

(defun svn-status-parse-fixup-externals-full-path ()
  "SubVersion 1.17 adds the full path to externals;
  this pre-parse hook fixes it up to look like pre-1.17.
  Allowing psvn to continue as normal"
  (goto-char (point-min))
  (let (( search-string  (file-truename default-directory) ))
    (save-match-data
      (save-excursion
    (while (re-search-forward search-string (point-max) t)
      (replace-match "" nil nil)
      )))))
(define-key global-map
  "\C-xvn" 'svn-status)
(define-key global-map
  "\C-xvk" 'svn-update)

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "ja_JP.UTF-8")

;;------------------------------------------------------------------------------
;; Dired で Windows に関連付けられたファイルを起動する。
;; http://uenox.infoseek.livedoor.com/meadow/
;;------------------------------------------------------------------------------
(defun uenox-dired-winstart ()
  "Type '\\[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (w32-shell-execute "open" fname)
        (message "win-started %s" fname))))
;;; dired のキー割り当て追加
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "z" 'uenox-dired-winstart))) ;;; 関連付け

;;------------------------------------------------------------------------------
;; dos-mode
;;------------------------------------------------------------------------------
(autoload 'dos-mode "dos-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(cmd\\|bat\\)$" . dos-mode))

;; gtags
;;(require 'gtags)
;;(add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
;;(add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
;;(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))

;;------------------------------------------------------------------------------
;; c/c++-mode
;;------------------------------------------------------------------------------
(add-hook 'c-mode-hook (lambda () (auto-complete-mode 1)))
(add-hook 'c++mode-hook (lambda () (auto-complete-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(c-basic-offset 4)
 '(custom-enabled-themes (quote (manoj-dark))))

;;------------------------------------------------------------------------------
;; csharp-mode
;;------------------------------------------------------------------------------
(require 'csharp-mode)

;; cモード共通フック
(add-hook 'csharp-mode-hook
          '(lambda()
             (setq comment-column 40)
             (setq c-basic-offset 4)
             (font-lock-add-magic-number)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'case-label '+)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close 0)
             (auto-complete-mode 1)
             )
          )


;;------------------------------------------------------------------------------
;; flymake-mode
;;------------------------------------------------------------------------------
(require 'flymake)
(global-set-key "\M-e" 'flymake-goto-next-error)
(global-set-key "\M-E" 'flymake-goto-prev-error)

;; gotoした際にエラーメッセージをminibufferに表示する
(defun display-error-message ()
  (message (get-char-property (point) 'help-echo)))
(defadvice flymake-goto-prev-error (after flymake-goto-prev-error-display-message)
  (display-error-message))
(defadvice flymake-goto-next-error (after flymake-goto-next-error-display-message)
  (display-error-message))
(ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
(ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message)

;;------------------------------------------------------------------------------
;; popwin
;;------------------------------------------------------------------------------
(require 'popwin)
(popwin-mode 1)
;; M-x anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

;; direx
(push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config)

;;------------------------------------------------------------------------------
;; direx
;;------------------------------------------------------------------------------
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;;------------------------------------------------------------------------------
;; grep-find
;;------------------------------------------------------------------------------
(setq grep-find-command "find . -type f ! -name '*.sdf' ! -name '*.lsn' ! -name '*.suo' ! -name '*.jpg' ! -name '*.gif' ! -name '*.JPEG' ! -name '*.jpeg' ! -name '*.png' ! -name '*.bmp' ! -name '*.pdf' ! -name '*.doc' ! -name '*.docx' ! -name '*.xls' ! -name '*.xlsm' ! -name '*.xlsb' ! -name '*.xps' ! -name '*.xla' ! -name '*.xlsx' ! -name '*.ppt' ! -name '*.pptx' ! -name '*.docx' ! -name '*,v' ! -name '*.obj' ! -name '*.pdb' ! -name '*.lib' ! -name '*.dll' ! -name '*~' ! -name '*.o' ! -name '*.a' ! -name '*.so' ! -name '*.class' ! -name '*.jar' ! -name '*.ilk' ! -name '*.exe' ! -name 'semantic.cache' ! -path '*.deps*' ! -path '*/obsolete/*' ! -path '*/.svn/*' ! -path '*/CVS/*' -print0 | xargs -0 -e grep -n -e ")

;;------------------------------------------------------------------------------
;; grep-edit
;;------------------------------------------------------------------------------
(require 'grep)
(require 'grep-edit)

;;------------------------------------------------------------------------------
;; flymake-jslint
;;------------------------------------------------------------------------------
(require 'flymake-jslint) ;; Not necessary if using ELPA package
(add-hook 'js2-mode-hook 'flymake-jslint-load)
(setq flymake-jslint-command "~/.emacs.d/jslint.bat")

;;------------------------------------------------------------------------------
;; markdown-mode
;;------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "perl /usr/local/bin/Markdown.pl")

;;------------------------------------------------------------------------------
;; eww
;;------------------------------------------------------------------------------
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

