(require 'cask "/usr/local/Cellar/cask/0.8.4/cask.el")
(cask-initialize)

;;----------------------------------------------------------- System
;; Settings
;; -----------------------------------------------------------
;; (set-default-coding-systems 'utf-8) (prefer-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8) (set-file-name-coding-system
;; 'utf-8) (setq default-process-coding-system '(utf-8 . utf-8))

;; CommandとOptionを入れ替える(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; shell環境を引き継ぐ(exec-path-from-shell-initialize)

;;----------------------------------------------------------- My
;; Settings
;; -----------------------------------------------------------

;; 起動時の画面非表示(setq inhibit-startup-message t) ;; menu ;; ツー
ルバーを非表示(tool-bar-mode -1) ;; メニューバーを非表示(menu-bar-mode
-1) ;; スクロールバーを非表示;;(scroll-bar-mode 0)

;; C-h backspace (global-set-key "\C-h" 'delete-backward-char)

;; current directory open (defun finder-current-dir-open()
(interactive) (shell-command "open ."))

;; directory open (defun finder-open(dirname) (interactive
"DDirectoryName:") (shell-command (concat "open " dirname)))

;; set the keybind (global-set-key (kbd "C-x M-f")
'finder-current-dir-open)

;;-------------------- for cursor speed-up --------------------
;; (defvar scroll-speedup-count 5) (defvar scroll-speedup-rate 2)
;; (defvar scroll-speedup-time 200)

(defvar scroll-step-default 1) (defvar scroll-step-count 1) (defvar
scroll-speedup-zero (current-time))

(defun scroll-speedup-setspeed () (let* ((now (current-time)) (min (-
  (car now) (car scroll-speedup-zero))) (sec (- (car (cdr now)) (car
  (cdr scroll-speedup-zero)))) (msec (/ (- (car (cdr (cdr now))) (car
  (cdr (cdr scroll-speedup-zero)))) 1000)) (lag (+ (* 60000 min) (*
  1000 sec) msec))) (if (> lag scroll-speedup-time) (progn (setq
  scroll-step-default 1) (setq scroll-step-count 1)) (setq
  scroll-step-count (+ 1 scroll-step-count))) (setq
  scroll-speedup-zero (current-time))))

(defun scroll-speedup-next-line (arg) (if (= (% scroll-step-count
  scroll-speedup-count) 0) (setq scroll-step-default (+
  scroll-speedup-rate scroll-step-default))) (if (string= arg 'next)
  (forward-line scroll-step-default) (forward-line (* -1
  scroll-step-default))))

(defadvice next-line (around next-line-speedup activate) (if (and
    (string= last-command 'next-line) (called-interactively-p 'any))
    (progn (scroll-speedup-setspeed) (scroll-speedup-next-line 'next))
    (setq scroll-step-default 1) (setq scroll-step-count 1) ad-do-it))

(defadvice previous-line (around previous-line-speedup activate) (if
    (and (string= last-command 'previous-line) (called-interactively-p
    'any)) (progn (scroll-speedup-setspeed) (scroll-speedup-next-line
    'previous)) (setq scroll-step-default 1) (setq scroll-step-count
    1) ad-do-it))

;;--------------------その他色々設定--------------------対応括弧ハイラ
;; イト(show-paren-mode t)インデント設定(setq-default indent-tabs-mode
;; nil) (setq-default tab-width 4)行末の空白を表示(setq-default
;; show-trailing-whitespace t) By an unknown contributor
;; (global-set-key "\M-p" 'match-paren) (defun match-paren (arg) "Go
;; to the matching paren if on a paren; otherwise insert %."
;; (interactive "p") (cond ((looking-at "\\s\(") (forward-list 1)
;; (backward-char 1)) ((looking-at "\\s\)") (forward-char 1)
;; (backward-list 1)) (t (self-insert-command (or arg 1)))))

;; added underline (global-set-key [f9] 'global-hl-line-mode) (setq
hl-line-face 'underline) ;; カラム行数表示(column-number-mode t) ;; 日
付表示(setq display-time-24hr-format t) (setq
display-time-day-and-date t) (display-time) ;; カスタム設定
(custom-set-variables ;; custom-set-variables was added by Custom.  ;;
If you edit it by hand, you could mess it up, so be careful.  ;; Your
init file should contain only one such instance.  ;; If there is more
than one, they won't work right.  '(auto-compression-mode t nil
(jka-compr)) '(blink-cursor-mode nil) '(column-number-mode t)
'(diff-switches "-u") '(display-time-mode t)
'(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
'(js-indent-level 2) '(package-selected-packages '(yasnippet wgrep-ag
web-mode use-package tern-auto-complete smex smartparens projectile
prodigy popwin pallet nyan-mode multiple-cursors magit js2-mode
idle-highlight-mode htmlize helm flycheck-cask expand-region
exec-path-from-shell drag-stuff ag)) '(show-paren-mode t))

;;バックアップファイル作成変更ファイルのバックアップ(setq
;; make-backup-files nil)

;;タブ表示(global-whitespace-mode 1)タブ, 全角スペース、改行直前の半角
;;スペースを表示する(when (require 'jaspace nil t) (when (boundp
;;'jaspace-modes) (setq jaspace-modes (append jaspace-modes (list
;;'php-mode 'yaml-mode 'javascript-mode 'ruby-mode 'text-mode 'c-mode
;;'nxml-mode 'web-mode 'js2-mode 'c++-mode 'fundamental-mode)))) (when
;;(boundp 'jaspace-alternate-jaspace-string) (setq
;;jaspace-alternate-jaspace-string "□")) (when (boundp
;;'jaspace-highlight-tabs) (setq jaspace-highlight-tabs ?^)) (add-hook
;;'jaspace-mode-off-hook (lambda() (when (boundp
;;'show-trailing-whitespace) (setq show-trailing-whitespace nil))))
;;(add-hook 'jaspace-mode-hook (lambda() (progn (when (boundp
;;'show-trailing-whitespace) (setq show-trailing-whitespace t))
;;(face-spec-set 'jaspace-highlight-jaspace-face '((((class color)
;;(background light)) (:foreground "blue")) (t (:foreground
;;"green")))) (face-spec-set 'jaspace-highlight-tab-face '((((class
;;color) (background light)) (:foreground "red" :background
;;"unspecified" :strike-through nil :underline t)) (t (:foreground
;;"purple" :background "unspecified" :strike-through nil :underline
;;t)))) (face-spec-set 'trailing-whitespace '((((class color)
;;(background light)) (:foreground "red" :background "unspecified"
;;:strike-through nil :underline t)) (t (:foreground "purple"
;;:background "unspecified" :strike-through nil :underline t))))))))

;;-------------------- HTML5対応--------------------

;; web-mode (require 'web-mode) (add-to-list 'auto-mode-alist
'("\\.html?\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.phtml\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.tpl\\.php\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.[gj]sp\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.as[cp]x\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.erb\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.mustache\\'" . web-mode)) (add-to-list 'auto-mode-alist
'("\\.djhtml\\'" . web-mode))

(defun web-mode-hook () "Hooks for Web mode."  (setq
web-mode-markup-indent-offset 2) (setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)) (add-hook 'web-mode-hook
'web-mode-hook)

;;-------------------- auto install -------------------- (when
;; (require 'auto-install nil t)インストールディレクトリを設定する初期
;; 値は ~/.emacs.d/auto-install/ (setq auto-install-directory
;; "~/.emacs.d/elisp")

  ;; EmacsWiki に登録されている elisp の名前を取得する
  ;; (auto-install-update-emacswiki-package-name t)

  ;; 必要であればプロキシの設定を行う(setq url-proxy-services
  ;;  '(("http" . "localhost:8080")))

  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup) ) ;;--------------------

;;-------------------- anything setting -------------------- (when
;; (require 'anything-startup nil t) (global-set-key (kbd "\C-x b")
;; 'anything) )

;;-------------------- YASnippet -------------------- (require
;; 'yasnippet) (yas-global-mode 1)

;; 既存スニペットを挿入する(define-key yas-minor-mode-map (kbd "C-x i
i") 'yas-insert-snippet) ;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet) ;; 既
存スニペットを閲覧・編集する(define-key yas-minor-mode-map (kbd "C-x i
v") 'yas-visit-snippet-file)

;;-------------------- popup-el -------------------- auto-complete
;; depend on popup-el (when (require 'auto-complete-config nil t)
;; (ac-config-default) (defvar ac-auto-start 1) (defvar ac-dwim t)
;; (defvar ac-use-menu-map t) ;; C-n/C-pで候補選択可能(add-to-list
;; 'ac-sources 'ac-source-yasnippet) ;; 常にYASnippetを補完候補に
;; (add-to-list 'ac-dictionary-directories
;; "~/.emacs.d/elisp/auto-complete/dict") (setq
;; ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/dict") ;;
;; 辞書ファイルのディレクトリ(defvar ac-comphist-file
;; "~/.emacs.d/elisp/auto-complete/ac-comphist.dat") ;; 補完履歴のキャッ
;; シュ先)

;;-------------------- js2-mode -------------------- (autoload
;; 'js2-mode "js2-mode" nil t) (add-to-list 'auto-mode-alist
;; '("\\.\\(js\\|json\\)$" . js2-mode)) (add-hook 'js2-mode-hook
;; (lambda () (defvar js2-basic-offset 2) (defvar js2-indent-level 2)
;; (defvar js2-switch-indent-offset 2) (defvar indent-tabs-mode nil)
;; ))

;;-------------------- php-mode -------------------- (require
;; 'php-mode) (autoload 'php-mode "php-mode") (add-to-list
;; 'auto-mode-alist '("\\.php$" . php-mode)) (add-to-list
;; 'auto-mode-alist '("\\.inc$" . php-mode))

;;----------------------------------------------------------- magit
;; -----------------------------------------------------------
;; (require 'magit)

;;-----------------------------------------------------------
;; tern-mode(for javascript)
;; -----------------------------------------------------------
;; (autoload 'js2-mode "js2-mode" nil t) (add-to-list 'auto-mode-alist
;; '("\.js$" . js2-mode)) (add-hook 'js2-mode-hook (lambda ()
;; (tern-mode t)))

(eval-after-load 'tern '(progn (require 'tern-auto-complete)
  (tern-ac-setup)))

;;----------------------------------------------------------- helm
;; -----------------------------------------------------------
;; (require 'helm-config) (helm-mode 1)

;; C-hで前の文字削除(define-key helm-map (kbd "C-h")
'delete-backward-char) (define-key helm-find-files-map (kbd "C-h")
'delete-backward-char)

;; flycheck (require 'flycheck) (add-hook 'after-init-hook
#'global-flycheck-mode)

(eval-after-load 'flycheck '(custom-set-variables
  '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
  (custom-set-faces ;; custom-set-faces was added by Custom.  ;; If
  you edit it by hand, you could mess it up, so be careful.  ;; Your
  init file should contain only one such instance.  ;; If there is
  more than one, they won't work right.  )


;;----------------------------------------------------------- eww
;; ----------------------------------------------------------- google
;; search (defvar eww-search-prefix
;; "https://www.google.co.jp/search?q=") (defun eww-disable-images ()
;; "ewwで画像表示させない" (interactive) (setq-local
;; shr-put-image-function 'shr-put-image-alt) (eww-reload)) (defun
;; eww-enable-images () "ewwで画像表示させる" (interactive)
;; (setq-local shr-put-image-function 'shr-put-image) (eww-reload))
;; (defun shr-put-image-alt (spec alt &optional flags) (insert alt))
;; (defun eww-mode-hook--disable-image () (setq-local
;; shr-put-image-function 'shr-put-image-alt)) (add-hook
;; 'eww-mode-hook 'eww-mode-hook--disable-image)

;;----------------------------------------------------------- markdown
;; -----------------------------------------------------------
;; (autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
;; (setq markdown-preview-stylesheets (list "github.css"))


;;----------------------------------------------------------- neotree
;; -----------------------------------------------------------隠しファ
;; イルをデフォルトで表示(require 'all-the-icons) (require 'neotree)
;; (setq neo-show-hidden-files t) cotrol + q でneotreeを起動
;; (global-set-key "\C-q" 'neotree-toggle)

;;-----------------------------------------------------------
;; plantuml-mode
;; ----------------------------------------------------------- .pu拡張
;; 子のファイルをplantuml-modeで開く(add-to-list 'auto-mode-alist'
;; ("\.pu$" . plantuml-mode))あなたのplantuml.jarファイルの絶対パスを
;; かく(defvar plantuml-jar-path
;; "/usr/local/Cellar/plantuml/1.2019.10/libexec/plantuml.jar") javaに
;; オプションを渡したい場合はここにかく(defvar plantuml-java-options
;; "") plantumlのプレビューをsvg, pngにしたい場合はここをコメントイン
;; デフォルトでアスキーアート(setq plantuml-output-type "svg")日本語を
;; 含むUMLを書く場合はUTF-8を指定(defvar plantuml-options "-charset
;; UTF-8") plantuml-modeの時にC-c C-sでplantuml-save-png関数を実行
;; (add-hook 'plantuml-mode-hook (lambda () (local-set-key (kbd "C-c
;; C-s") 'plantuml-save-png)))

;; もしも.puファイルを保存した時にpngファイルを保存したい場合はこちら
;; をコメントイン(add-hook 'plantuml-mode-hook (lambda () (add-hook
;; 'after-save-hook 'plantuml-save-png)))

;; plantumlをpngで保存する関数(defun plantuml-save-png ()
(interactive) (when (buffer-modified-p) (map-y-or-n-p "Save this
buffer before executing PlantUML?"  'save-buffer (list
(current-buffer)))) (let ((code (buffer-string)) out-file cmd) (when
(string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code) (setq
out-file (match-string 1 code))) (setq cmd (concat "java
-Djava.awt.headless=true -jar " plantuml-java-options " "
(shell-quote-argument plantuml-jar-path) " " (and out-file (concat
"-t" (file-name-extension out-file))) " " plantuml-options " "
(buffer-file-name))) (message cmd) (call-process-shell-command cmd nil
0)))

;;-----------------------------------------------------------
;; smooth-scrolling
;; -----------------------------------------------------------
;; (require 'smooth-scrolling) (smooth-scrolling-mode 1)
