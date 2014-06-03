;;-----------------------------------------------------------
;; load-path
;;-----------------------------------------------------------
;;(setq load-path (cons (expand-file-name "~/.emacs.d/elisp") load-path))
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/html5-el")
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
(add-to-list 'load-path "~/.emacs.d/elisp/popup-el")
(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-w3m/share/emacs/site-lisp/w3m")

;;-----------------------------------------------------------
;; System Settings
;;-----------------------------------------------------------
;;(set-keyboard-coding-system 'cp932)
 (set-default-coding-systems 'utf-8)
 (prefer-coding-system 'utf-8)
 (set-terminal-coding-system 'utf-8)
 (set-file-name-coding-system 'utf-8)
 (setq default-process-coding-system '(utf-8 . utf-8))
;;(set-default-coding-systems 'euc-jp-unix)
;;(prefer-coding-system 'euc-jp-unix)
;;(set-terminal-coding-system 'euc-jp-unix)
;;(set-file-name-coding-system 'euc-jp-unix)
;;(setq default-process-coding-system '(euc-jp-unix . euc-jp-unix))

;;-----------------------------------------------------------
;; My Settings
;;-----------------------------------------------------------

;;--------------------
;; for cursor speed-up
;;--------------------
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


;;--------------------
;; subversion
;;--------------------
(require 'psvn)
(setq process-coding-system-alist '(("svn" . utf-8)))
(setq default-file-name-coding-system 'utf-8)
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


;;--------------------
;; その他色々設定
;;--------------------
;; 対応括弧ハイライト
(show-paren-mode t)
;; インデント設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
;; By an unknown contributor
(global-set-key "\M-p" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; added underline
(global-set-key [f9] 'global-hl-line-mode)
(setq hl-line-face 'underline)
;; カラム行数表示
(column-number-mode t)
;; 日付表示
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
;; カスタム設定
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(blink-cursor-mode nil)
 '(diff-switches "-u")
 '(show-paren-mode t))

;;バックアップファイル作成
;; 変更ファイルのバックアップ
(setq make-backup-files nil)

;;タブ表示
;;(global-whitespace-mode 1)
;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'c-mode
                                      'nxml-mode
                                      'js2-mode
                                      'c++-mode
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

;;--------------------
;; HTML5対応
;;--------------------
(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . nxml-mode)
                ("\\.php\\'" . php-mode)
                )
              auto-mode-alist))

(load "rng-auto.el" 't)
(add-hook 'nxml-mode-hook
          (lambda ()
            ;; 更新タイムスタンプの自動挿入
            (setq time-stamp-line-limit 10000)
            (if (not (memq 'time-stamp write-file-hooks))
                (setq write-file-hooks
                      (cons 'time-stamp write-file-hooks)))
            (setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y %Z")
            (setq time-stamp-start "Last modified:[ \t]")
            (setq time-stamp-end "$")
            ;;
            (setq auto-fill-mode -1)
            (setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
            (setq nxml-child-indent 2)                  ; タグのインデント幅
            (setq nxml-attribute-indent 4)              ; 属性のインデント幅
            (setq indent-tabs-mode nil)
            (setq nxml-bind-meta-tab-to-complete-flag t)
            (setq nxml-slash-auto-complete-flag t)      ; </の入力で閉じタグを補完する
            (setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
            (setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示
            (setq tab-width 4)))  ; /(終了タグ)

(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/elisp/html5-el/schemas.xml"))
(require 'whattf-dt)

;;--------------------
;; auto install
;;--------------------
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
;;--------------------

;;--------------------
;; anything setting
;;--------------------
(when (require 'anything-startup nil t)
  (global-set-key (kbd "\C-x b") 'anything)
  )

;;--------------------
;; YASnippet
;;--------------------
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

;;--------------------
;; popup-el
;;--------------------
;; auto-complete depend on popup-el
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
  (add-to-list 'ac-sources 'ac-source-yasnippet) ;; 常にYASnippetを補完候補に
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict")
  ;;(setq ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict") ;; 辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/elisp/auto-complete/ac-comphist.dat") ;; 補完履歴のキャッシュ先
  )

;;--------------------
;; js2-mode
;;--------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))

;;--------------------
;; php-mode
;;--------------------
;;(require 'php-mode)
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;--------------------
;; grep-edit
;;--------------------
(require 'grep)
(require 'grep-edit)

(defadvice grep-edit-change-file (around inhibit-read-only activate)
  ""
  (let ((inhibit-read-only t))
    ad-do-it))
;; (progn (ad-disable-advice 'grep-edit-change-file 'around 'inhibit-read-only) (ad-update 'grep-edit-change-file)) 

(defun my-grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)

;; jslint
;;(require 'flymake-jslint) ;; Not necessary if using ELPA package
;;(add-hook 'js-mode-hook 'flymake-jslint-load)

;;-----------------------------------------------------------
;; w3m
;;-----------------------------------------------------------
(require 'w3m-load)
(setq w3m-command-arguments-alist
      '(;; Don't use the proxy server to visit local web pages.
        ("^http://\\(?:[^/]*\\.\\)*your-company\\.com\\(?:/\\|$\\)"
         "-no-proxy")
        ;; Use the proxy server to visit any foreign urls.
        (""
         "-o" "http_proxy=http://proxy.sso.ntts.co.jp:18080/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jaspace-highlight-tab-face ((t (:foreground "red" :underline t)))))
