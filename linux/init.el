;; package manager
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;-----------------------------------------------------------
;; System Settings
;;-----------------------------------------------------------
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
;; font
(add-to-list 'default-frame-alist '(font . "ricty-10"))

;;-----------------------------------------------------------
;; My Settings
;;-----------------------------------------------------------

;; 起動時の画面非表示
(setq inhibit-startup-message t)
;; menu
;; ツールバーを非表示
(tool-bar-mode -1)
;; メニューバーを非表示
(menu-bar-mode -1)
;; スクロールバーを非表示
;;(scroll-bar-mode 0)


;; C-h backspace
(global-set-key "\C-h" 'delete-backward-char)

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
;; (require 'psvn)
;; (setq process-coding-system-alist '(("svn" . utf-8)))
;; (setq default-file-name-coding-system 'utf-8)
;; (setq svn-status-svn-file-coding-system 'utf-8)

;; (autoload 'svn-status "psvn" nil t)
;; (add-hook 'svn-pre-parse-status-hook 'svn-status-parse-fixup-externals-full-path)

;; (defun svn-status-parse-fixup-externals-full-path ()
;;     "SubVersion 1.17 adds the full path to externals; 
;;   this pre-parse hook fixes it up to look like pre-1.17.
;;   Allowing psvn to continue as normal"
;;     (goto-char (point-min))
;;     (let (( search-string  (file-truename default-directory) ))
;;       (save-match-data
;;   (save-excursion
;;     (while (re-search-forward search-string (point-max) t)
;;       (replace-match "" nil nil)
;;       )))))
;; (define-key global-map
;;   "\C-xvn" 'svn-status)
;; (define-key global-map
;;   "\C-xvk" 'svn-update)

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
 '(column-number-mode t)
 '(diff-switches "-u")
 '(display-time-mode t)
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
              'web-mode
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

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)

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
;; (when (require 'anything-startup nil t)
;;   (global-set-key (kbd "\C-x b") 'anything)
;;   )

;;--------------------
;; YASnippet
;;--------------------
(add-to-list 'load-path
             "~/.emacs.d/.cask/24.5/elpa/yasnippet-20160801.1142")
(require 'yasnippet)
(yas-global-mode 1)

;; (when (require 'yasnippet nil t)
;;   (yas--initialize)
;;   (setq yas-snippet-dirs
;;   ;;  '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
;;   ;;  "~/.emacs.d/.cask/24.5/elpa/yasnippet-20160801.1142/snippets" ;; 最初から入っていたスニペット(省略可能)
;;     )
;;   ;;  (yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets")
;;   ;;  (yas/load-directory "~/.emacs.d/elisp/yasnippet/extras/imported")
;;   (yas/global-mode 1)
;;   )

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
(add-hook 'js2-mode-hook
    #'(lambda ()
        (setq js2-basic-offset 2
        indent-tabs-mode nil)
        ))

;;--------------------
;; php-mode
;;--------------------
;;(require 'php-mode)
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;-----------------------------------------------------------
;; magit
;;-----------------------------------------------------------
(require 'magit)

;;-----------------------------------------------------------
;; tern-mode(for javascript)
;;-----------------------------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
           (tern-ac-setup)))

;;-----------------------------------------------------------
;; helm
;;-----------------------------------------------------------
(require 'helm-config)
(helm-mode 1)

;; C-hで前の文字削除
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
