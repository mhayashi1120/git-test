;;; -*- coding: utf-8; -*-
;;; gdic.el --- Google translator for Japanese

;;; Commentary:
;; 

;; 日本人向け Emacs 用 Google Translater インタフェース
;; # を適当に作ったもの。(^^;;

;;; Install:

;; json.el はここからダウンロードしたものを使ってます。
;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs

;;; Usage:

;;    M-x gdic
;;
;; 私は sdic と組み合わせて使っています。
;; sdic に対象エントリがないときは gdic を呼び出す感じ。


;;    M-x gdic-start-auto-echo
;; 
;; 英語の文書を読むときにカーソル位置の単語を表示してくれます。

;;; TODO:
;; from は auto にする方がいいかな？ドイツ、フランスなんでもいけるかも。

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'json)

(defgroup gdic nil
  "Google translator interface."
  :group 'tools
  :prefix "gdic-")

(defvar current-prefix-arg)

(defcustom gdic-format-function 'gdic-format
  ""
  :group 'gdic)

(defconst gdic-request-default-parameters
  '(
    ("prev" . "conf")
    ("otf" . "1")
    ("pc" . "0")
    ("client" . "t")
    ("hl")
    ("sl")
    ("tl")
    ("ptl")
    ("psl")
    ))

(defconst gdic-base-url "http://translate.google.co.jp/translate_a/t")
    
(defun gdic-generate-request-url (word from to)
  (format "%s?%s"
	  gdic-base-url
	  (mapconcat
	   (lambda (i) 
	     (let ((key (car i))
		   (val (cdr i))
		   value)
	       (cond
		((member key '("hl" "sl" "psl"))
		 (setq value from))
		((member key '("tl" "ptl"))
		 (setq value to))
		(t
		 (setq value (gdic-url-encode-string val))))
	       (concat key "=" value)))
	   (cons (cons "text" word) gdic-request-default-parameters)
	   "&")))

;; replace original by `flet'
(defun gdic-json-read-array ()
  "Read the JSON array at point."
  ;; Skip over the "["
  (json-advance)
  (json-skip-whitespace)
  ;; read values until "]"
  (let (elements)
    (while (not (char-equal (json-peek) ?\]))
      (push (json-read) elements)
      (json-skip-whitespace)
      (unless (char-equal (json-peek) ?\])
        (unless (char-equal (json-peek) ?,)
	  (signal 'json-error (list 'bleah)))
	(json-advance)
	(when (char-equal (json-peek) ?\])
	  (push (gdic-json-read-empty) elements))))
    ;; Skip over the "]"
    (json-advance)
    (apply json-array-type (nreverse elements))))

(defun gdic-json-read-from-string (string)
  (let ((json-readtable (copy-seq json-readtable)))
    (add-to-list 'json-readtable (list ?, 'gdic-json-read-empty))
    (add-to-list 'json-readtable (list ?\] 'gdic-json-read-empty))
    (flet ((json-read-array () (gdic-json-read-array)))
      (json-read-from-string string))))

(defun gdic-json-read-empty ()
  nil)

(defun gdic-url-encode-string (str)
  (apply 'concat
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	 ; printable
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "") 'utf-8)))))

(defun gdic-current-word ()
  (if mark-active
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'word)))

(defun gdic-read-word ()
  (let ((word (gdic-current-word)))
    (when (or (null word) (not (string-match "^\\ca+$" word)))
      (setq word (read-from-minibuffer "Word: " word)))
    word))

(defvar gdic-http-user-agent "Mozilla/5.0")

(defun gdic-search-word/json (word)
  (require 'url-http)
  (flet ((url-http-user-agent-string 
	  () 
	  (format "User-Agent: %s\r\n" gdic-http-user-agent))
	 (url-mime-charset-string () nil))
    (let ((url-request-method "POST")
	  (url-request-data nil)
	  (url-request-extra-headers '(("Content-Length" . "0")))
	  (url-mime-charset-string)
	  (url-extensions-header)
	  (url-show-status) ;; suppress message
	  (from&to (gdic-guessed-from&to word))
	  json buffer)
      (setq buffer (url-retrieve-synchronously
		    (gdic-generate-request-url word (car from&to) (cdr from&to))))
      (with-current-buffer buffer
	(goto-char (point-min))
	(re-search-forward "^$" nil t)
	(forward-line 1)
	(let ((body (buffer-substring (point) (point-max))))
	  (setq json 
		(gdic-json-read-from-string 
		 (decode-coding-string body 'utf-8)))))
      (kill-buffer buffer)
      json)))

(defun gdic-guessed-from&to (word)
  (cond
   ((string-match "\\cj" word)
    (cons "ja" "en"))
   ((string-match "^\\ca+$" word)
    (cons "en" "ja"))
   (t
    (cons "auto" "ja"))))

(defun gdic-format (object)
  (let ((summary (gdic-aref (gdic-aref object 0) 0))
	(details (gdic-aref (gdic-aref object 1) 0))
	(type (gdic-aref object 2)))
    (format "%s [%s]\n%s"
	    (mapconcat 'identity summary " ")
	    (or (gdic-aref details 0) "")
	    (with-temp-buffer
	      (insert (mapconcat 'identity (gdic-aref details 1) " / "))
	      (fill-region (point-min) (point-max))
	      (buffer-string)))))

(defun gdic-format-simple (object)
  (let ((summary (gdic-aref (gdic-aref object 0) 0))
	(details (gdic-aref (gdic-aref object 1) 0))
	(type (gdic-aref object 2)))
    (format "%s [%s] %s"
	    (mapconcat 'identity summary " ")
	    (or (gdic-aref details 0) "")
	    (mapconcat 'identity (gdic-aref details 1) "/"))))

(defun gdic-aref (array idx)
  (when array 
    (aref array idx)))

;;TODO maximum threshold
(defvar gdic-search-word-hash-table (make-hash-table :test 'equal))

(defun gdic-search-word/cache/json (word)
  (let ((cached (gethash word gdic-search-word-hash-table)))
    (unless cached
      (setq cached (gdic-search-word/json word))
      (puthash word cached gdic-search-word-hash-table))
    cached))


;; echo current word.
;; inspired from http://d.hatena.ne.jp/kitokitoki/20100913/p1

(defvar gdic-echo-timer nil)
(defvar gdic-echo-word nil)
(defvar gdic-echo-idle-delay 1)

(defun gdic-auto-echo-respect-other ()
  (when (or (null (current-message))
	    (and gdic-echo-word
		 (string= (current-message) (cdr gdic-echo-word))))
    (let ((word (thing-at-point 'word)))
      (if (and word (string-match "^\\ca+" word))
	  (condition-case err
	      (let ((msg
		     (if (or (null gdic-echo-word)
			     (not (string= (car gdic-echo-word) word)))
			 (gdic-format-simple (gdic-search-word/cache/json word))
		       (cdr gdic-echo-word))))
		(let ((message-log-max)
		      (minibuffer-message-timeout 10))
		  (message msg))
		(setq gdic-echo-word (cons word msg)))
	    ;; stop timer (Denied by google?)
	    (error (gdic-stop-auto-echo) 
		   (message "Gdic auto echo is stopped. (Access is denied by google?)")))
	(message nil)
	(setq gdic-echo-word nil)))))

(defun gdic-start-auto-echo ()
  (interactive)
  (unless gdic-echo-timer
    (setq gdic-echo-timer
	  (run-with-idle-timer gdic-echo-idle-delay t 'gdic-auto-echo-respect-other))))

(defun gdic-stop-auto-echo ()
  (interactive)
  (when gdic-echo-timer
    (cancel-timer gdic-echo-timer)
    (setq gdic-echo-timer nil)))



(defun gdic (word &optional arg)
  "WORD を翻訳する。
\\[universal-argument] をつけることで結果をコピーする。"
  (interactive
   (let ((word (gdic-read-word)))
     (list word current-prefix-arg)))
  (let ((json (gdic-search-word/json word)))
    (let (message-log-max tmp)
      (setq tmp (message (funcall gdic-format-function json)))
      (when arg
	(kill-new tmp)))))

(provide 'gdic)

;;; gdic.el ends here
