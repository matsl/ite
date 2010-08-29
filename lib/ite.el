;;; ite.el, the (X)Emacs interface for iTe.
;;;
;;; Copyright (C) 2000, 2002, 2005 by Wolfgang Kuehn <wolfgang@decatur.de>
;;;
;;; Version: _ITEVERSION_
;;; Time-stamp: <05/03/21 19:45:31 wolfgang>
;;; Syntax          : emacs-lisp
;;;
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU General Public License
;;;    as published by the Free Software Foundation. See the copyright
;;;    notice included in the iTe distribution for more details.
;;;
;;; Consult installation.html for help on installation and configuration.
;;;

;;; Notes:
;;; * We do not use (defvar ...) to declare or define variables, because all
;;;   user defined configuration items come from the configuration files.

(defconst ite-version "_ITEVERSION_")

(require 'browse-url)

;; List of all configuration items.
(setq ite-configuration-items 
      (list
       'ite-temp-path
       'ite-doc-path
       'ite-lib-path
       'ite-dvips-command
       'ite-gs-path
       'ite-gs-options
       'ite-perl-path
       'ite-perl-options
       'ite-eps-limit
       'ite-ps-limit
       'ite-unselect-color 
       'ite-select-color
       'ite-move-left
       'ite-move-left-slower
       'ite-move-left-slowest
       'ite-move-right
       'ite-move-right-slower
       'ite-move-right-slowest
       'ite-move-up
       'ite-move-up-slower
       'ite-move-up-slowest
       'ite-move-down
       'ite-move-down-slower
       'ite-move-down-slowest
       'ite-left-rotate
       'ite-left-rotate-slower
       'ite-left-rotate-slowest
       'ite-right-rotate
       'ite-right-rotate-slower
       'ite-right-rotate-slowest
       'ite-magnify
       'ite-magnify-slower
       'ite-magnify-slowest
       'ite-shrink
       'ite-shrink-slower
       'ite-shrink-slowest
       'ite-toggle-key-bindings
       'ite-toggle-bounding-box
       'ite-next-page
       'ite-previous-page
       'ite-next-object
       'ite-previous-object
       'ite-quit
       'ite-redisplay
       'ite-zoom-out
       'ite-zoom-in
       'ite-zoom-closer
       'ite-zoom-closest
       'ite-debug-level))

(if (not (boundp 'running-xemacs))
    (eval-and-compile
      (defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))))
      
(if (not running-xemacs) (require 'easymenu))
  
;; Borrowed from python-mode.el

(if (not (fboundp 'temp-directory))
    (defun temp-directory ()
      "*Directory used for temporary files."
      (let ((ok '(lambda (x)
		   (and x
			(setq x (expand-file-name x)) ; always true
			(file-directory-p x)
			(file-writable-p x)
			x))))
	(or (funcall ok (getenv "TMPDIR"))
	    (funcall ok "/usr/tmp")
	    (funcall ok "/tmp")
	    (funcall ok  ".")
	    (error
	     "Couldn't find a default temporary directory")))))

(defun ite-backslash-to-slash (s)
  (mapconcat (lambda (x) x) (split-string s "\\\\") "/"))

;; In the current buffer, read one line at point and place point at the beginning of the next line.
;; Returns the line, or nil if point was at eof.
(defun ite-read-line ()
  (if (= (point) (point-max)) nil
    (let ((p (point)) (line-content nil))
      (end-of-line)
      (setq line-content (buffer-substring p (point)))
      (forward-line 1)
      line-content)))

;; Trims margin of the left side of s.
(defun ite-left-trim (s margin)
  (let ((l (length margin)))
    (if (and (<= l (length s)) (string= (substring s 0 l) margin))
	(substring s l (length s))
      s)))

;; Trims margin of the right side of s.
(defun ite-right-trim (s margin)
  (let ((l (length margin)))
    (if (and (<= l (length s)) (string= (substring s (- (length s) l) (length s)) margin))
	(substring s 0 (- (length s) l))
      s)))

;; Trims margin of both sides of s.
(defun ite-trim (s margin)
  (ite-right-trim (ite-left-trim s margin) margin))

;; Trims any whitespace of both sides of s.
(defun ite-trim-whitespace (s)
  (if s
      (progn
	(string-match "^\\s-*\\(.*?\\)\\s-*$" s)
	(match-string 1 s))
    nil))

;; Not used.
(defun ite-split-line (line)
  (let ((tokens (split-string line)) (result nil))
    (while (car tokens)
      (if (not (string= (car tokens) ""))
	  (setq result (append result (list (ite-trim (car tokens) "\"")))))
      (setq tokens (cdr tokens))
      )
    result))

;; Read one token from s and return this token and the remaining string as a list, or
;; return nil if no more token is found. After a # character the remaining string is ignored. 
;; A token is either a number, an identifier, a bracketed key sequence, or a quoted string.
(defun ite-read-token (s)
  (setq s (ite-trim-whitespace s))
  (if (or (= 0 (length s)) (= (elt s 0) ?#))
      nil ;; No more tokens
    (let ((match nil) (readMatch nil))
      (cond
       (; A bracketed key sequence.
	(= (elt s 0) ?\[)
	(if (not (string-match "^\\(\\[.*?\\]\\)" s))
	    (error (format "Bracketed [] string expected at <%s>" s)))
	(setq match (match-string 1 s))
	(setq readMatch (read match)))
       (; A quoted string.
	(= (elt s 0) ?\")
	(if (not (string-match "^\\(\".*?\"\\)" s))
	    (error (format "Quoted string expected at '%s'" s)))
	(setq match (match-string 1 s))
	(if (string-match "\\\\" match) (error "String must not contain backslash characters"))
	(setq readMatch (read match)))
       (; An identifier.
	(string-match "^\\([-a-z]+\\)" s)
	(setq match (match-string 1 s))
	(setq readMatch match))
      (; A number.
       (string-match "^\\([0-9]+\\)" s)
	(setq match (match-string 1 s))
	(setq readMatch (read match)))
      (t
       (error (format "Syntax error at <%s>" s))))
      (list readMatch (ite-trim-whitespace (substring s (length match) (length s)))))))

;; Returns a list of all tokens in s.
(defun ite-tokenize (s)
  (let ((result (list)) (token nil))
    (while (setq tokenizer (ite-read-token s))
      (setq token (elt tokenizer 0))
      (setq result (append result (list token)))
      (setq s (elt tokenizer 1)))
    result))

;; Takes the first token of the specified token list as the name of a configuration item and
;; assigns the rest of the tokens as value.
(defun ite-assign-configuration-item (tokens)
  (let ((item nil))
    (if (not (stringp (elt tokens 0)))
	(error (format "Configuration item expected, found %s") (elt tokens 0)))
    (setq item (intern (concat "ite-" (elt tokens 0))))
    (if (not (memq item ite-configuration-items))
	(error (format "Configuration item <%s> not allowed" (elt tokens 0))))
    (if (or (eq item 'ite-gs-options) (eq item 'ite-perl-options))
	(set item (cdr tokens))
      (progn
	(if (> (length tokens) 2)
	    (error (format "Configuration item <%s> can have only one value" item)))
	(set item (elt tokens 1))))))

;; Takes the first token of the specified token list as the name of a configuration item and
;; assigns the rest of the tokens as value.
(defun ite-assign-configuration-item-platform (tokens)
  (let ((firstToken (elt tokens 0)))
    (if (not (stringp firstToken))
	(error (format "Configuration item expected, found %s") firstToken))
    
    (cond
     ((string= firstToken "MS-WINDOWS")
      (if (not (string= path-separator ":"))
	  (ite-assign-configuration-item (cdr tokens))))
     ((string= firstToken "UNIX")
      (if (string= path-separator ":")
	  (ite-assign-configuration-item (cdr tokens))))
     (t
      (ite-assign-configuration-item tokens)))))
      
	
(defun ite-read-config-file (file)
  (if (not (file-readable-p file))
      (ite-abort (format "Alert: Configuration file '%s' not found!" file)))
  (save-excursion
    (let ((line nil) (lineNumber 0))
      (ite-log (format "Reading configuration file %s ..." file))
      (set-buffer (get-buffer-create "ite-config"))
      (erase-buffer)
      (insert-file-contents file)
      (condition-case exception
	  (while (setq line (ite-trim-whitespace (ite-read-line)))
	    (setq lineNumber (1+ lineNumber))
	    (let ((tokens (ite-tokenize line)) (item nil))
	      (if tokens
		  (progn
		    (ite-log (concat "    " line))
		    (ite-assign-configuration-item-platform tokens)))))
	(error (ite-abort (format "Parse error in file %s at line %s: %s" file lineNumber exception)))))))


; Read and validate the configuration.
(defun ite-read-configuration ()
  (setq ite-configuration-read-p nil)
  
  ;; Read all configuration files.
  (mapcar 'ite-read-config-file ite-config-files)

  (ite-check-path (expand-file-name "ite.pl" ite-lib-path))
  (ite-check-path (expand-file-name "ite.ps" ite-lib-path))
  (ite-check-executable ite-gs-path)
  (ite-check-executable ite-perl-path)
  (ite-check-executable ite-dvips-command)

  (ite-log "Checking all mandatory configuration items ...")
  (let ((l (mapconcat 'ite-boundp ite-configuration-items "")))
    (if (> (length l) 0)
        (ite-abort l)))

  
  (ite-log "Setting keys ...")
  (ite-set-keys)

  (ite-log "End of initialization.")

  ;; Set marker at current point in log buffer.
  (save-excursion
    ;; Function point-marker in XEmacs accepts optional buffer argument!
    (set-buffer (get-buffer-create ite-log-buffer-name))
    (setq ite-log-marker (point-marker)))
  (setq ite-configuration-read-p t))


(defun ite-boundp (symbol)
  " This function returns an error string if symbol is not bound."
  (if (not (boundp symbol))
      (format "Symbol '%s' not bound!%c" (symbol-name symbol) ?\n)))

;; I really shouldn't do this.
(if (not running-xemacs)
    (progn
      (defun first (l)
	(nth 0 l))
      (defun second (l)
	(nth 1 l))
      (defun event-window (event)
	(posn-window (event-end event)))
      (defun event-point (event)
	(posn-point (event-end event)))
      (defalias 'get-dialog-box-response 'x-popup-dialog)))

(defun ite-show-log ()
  (interactive)
  (let ((w (selected-window)))
    (save-excursion
      (switch-to-buffer-other-window ite-log-buffer-name)
      (goto-char (point-max))
      (recenter))
    (select-window w)))

(defun ite-log (s &rest l)
  "Append string into log buffer without moving point."
  (save-excursion
    (set-buffer (get-buffer-create ite-log-buffer-name))
    (goto-char (point-max))
    (eval (append '(insert "\n" s) l))))

(defun ite-dialog (message buttons)
  (get-dialog-box-response  t (append (list message) buttons)))

(defun ite-abort (message)
  "Aborts iTe with message."
  (ite-exit message)
  (error (concat "iTe error: " message)))

(defun ite-exit (message)
  "Aborts iTe with message."
  (ite-log message)
  (ite-dialog message '(("Ok" . nil)))
  (ite-show-log)
  (if (= ite-debug-level 0) (ite-kill)))

(defun ite-send-string (string)
  "Send STRING to process with 'ITEDict' on top of dict stack."
  (ite-send-string-nodict (format "ITEDict begin %s \n end\n" string)))

(defun ite-send-string-nodict (string)
  "Send STRING to process, and to log buffer if debugging is on."
  (if (> ite-debug-level 1) (ite-log "SEND: " string "\n"))
  (if (string= (process-status ite-gs-process) "run")
      (process-send-string ite-gs-process (concat string "\n"))))

(defun ite-filter (proc string)
  (condition-case exception
      (progn 
	(ite-log string)
	(setq ite-log-string (concat ite-log-string string))
	
	(if (string-match "Error" ite-log-string)
	    (ite-abort "PostScript error!"))
	
	(if (string-match "iTe_GhostScriptStarted" ite-log-string)
	    (progn
	      (setq ite-log-string (substring ite-log-string (match-end 0)))
	      (ite-gs-callback)))
	
	(if (string-match "iTe_PerlFinished" ite-log-string)
	    (progn
	      (setq ite-log-string (substring ite-log-string (match-end 0)))
	      (ite-perl-callback)))
	
	(if (string-match "\\(.*\n\\)*" ite-log-string)
	    (setq ite-log-string (substring ite-log-string (match-end 0))))
	)
    (error (ite-log (format "Error: %s" exception)) exception)))

(defun ite-kill ()
  "Delete all temporary files and buffers, reset text properties, 
kill ghostscript process."
  (interactive)
  (setq ite-current-object nil)
  
  (if ite-font-lock
      (add-hook 'after-change-functions
		ite-font-lock nil t))
  (setq ite-font-lock nil)
  
  (let ((begin) (end (point-min)))
    (while (setq begin (next-single-property-change end 'ite-number))
      (setq end (next-single-property-change begin 'ite-number))
      (remove-text-properties begin end 
			      (list 'face nil
				   (if running-xemacs 'keymap 'local-map) nil
				   'mouse-face nil
				   'start-open nil
				   'rear-nonsticky nil
				   'ite-x0 nil
				   'ite-y0 nil
				   'ite-page nil
				   'ite-number nil
				   'help-echo nil))))
    
  (if ite-gs-process 
      ;;(delete-process ite-gs-process))
      (ite-send-string-nodict "quit\n\n"))
  
  (if (and ite-dir
	   (file-directory-p ite-dir)
	   (file-writable-p ite-dir)
	   (= ite-debug-level 0))
      (progn
	(sleep-for 1)			; MSDOS releases lock late. 
	(ite-delete-directory ite-dir)
        ))
  
  (setq ite-dir nil))

(defun ite-delete-directory (directoryName)
  "Deletes non-empty directory."
  (progn
    (setq fileList (directory-files directoryName))
    (mapcar (lambda (fileName) 
	      (let ((absoluteFileName (expand-file-name fileName directoryName)))
		(if (not (file-directory-p absoluteFileName))
		    (delete-file absoluteFileName))))
	    fileList))
  (delete-directory directoryName))

(defun ite-parse ()
  "Parse TeX file for iTe objects."
  (widen)
  (let ((ll) (count) (x0) (y0) (old case-fold-search))
    (save-excursion
      (setq case-fold-search nil)
      (goto-char (point-min))
      
      (while (setq ll (car ite-info-list))
	(setq ite-info-list (cdr ite-info-list)
	      count 0)
	(while (< count (second ll))
	  (if (not (ite-next-object))
	      (ite-abort "Cannot find all iTe objects!"))
	  ;; Point is at beginning of a "\ITE(x0 y0 alpha mag)".
	  (setq begin (1+ (point)))
	  (search-forward "(")
	  (setq x0 (read (current-buffer)))
	  (setq y0 (read (current-buffer)))
	  (search-forward ")")
	  (add-text-properties begin (point)
			       (list 'mouse-face 'highlight
				     'face 'ite-unselect-face
				     (if running-xemacs 'keymap 'local-map) ite-keymap
				     'ite-x0 x0
				     'ite-y0 y0
				     'ite-page (first ll)
				     'ite-number count
				     'help-echo "Button2 selects iTe object."))
	  (if running-xemacs 
	      (add-text-properties begin (point) '(start-open t))
	    (add-text-properties (1- (point)) (point) '(rear-nonsticky t)))
	  
	  (setq count (1+ count))))

      (if (ite-next-object) (ite-abort "Spurious iTe object in (La)TeX source")))
    (setq case-fold-search old)))


(defun ite-looking-at (s1 s2)
  (save-excursion
    (let* ((p2 (- (point) (length s2))) (p1 (- p2 (length s1))))
      (and
       (>= p1 (point-min))
       (string= s1 (buffer-substring p1 p2))))))


(setq ite-TeX-pattern
      (concat "\\(" (mapconcat '(lambda (e) e) 
			       '("\\\\ITE[^a-zA-Z@]"
				 "\\\\ite[^a-zA-Z@]"
				 "\\\\verb[^a-zA-Z]"
				 "verbatim[^a-zA-Z@]"
				 "itebind[^a-zA-Z@]"
				 "[^\\\\]%"
				 "document[^a-zA-Z@]")
			       "\\)\\|\\(")
	      "\\)"))



(defun ite-next-object ()
  (interactive)
  "*Find next occurrence of '\ite' or '\ITE', but not inside a comment, a
'\verb@...@', and not inside a 'verbatim' or 'itebind' environment.
If object is found, return 't' and set point to beginning of '\ITE...'.
Otherwise, return 'nil' and set point to (point-max)."

  (while 
      (let ((p (re-search-forward ite-TeX-pattern (point-max) 1)))
	(cond
	 ((not p)
	  nil)
	 ((match-string 6)
	  (end-of-line)
	  t)
	 ((match-string 4)
	  (if (ite-looking-at "\\begin{" (match-string 4))
	      (search-forward "\\end{verbatim}"))
	  t)
	 ((match-string 3)
	  (search-forward (char-to-string (preceding-char)))
	  t)
	 ((match-string 7)
	  (if (ite-looking-at "\\end{" (match-string 7))
	      (goto-char (point-max)))
	  t)
	 ((match-string 5)
	  (cond
	   ((ite-looking-at "\\begin{" (match-string 5))
	    (search-forward "\\end{itebind"))
	   ((ite-looking-at "\\begin" (match-string 5))
	    (search-forward "\\enditebind")))
	  t)
	 ((match-string 1)
	  (backward-char 1)
	  (ite-format-check)
	  (backward-char 4)
	  nil)
	 ((match-string 2)
	  (backward-char 1)
	  (delete-char -3)
	  (insert "ITE(0 0 0 1)")
	  (goto-char (match-beginning 0))
	  nil)
	 (t
	  (error "Should never be here!")))))

  (not (eobp)))


(defun ite-close-page ()
  (ite-send-string-nodict "ITEDict /Eop get exec\n\n"))


(defun ite-zoom-in (ite-zoom-unit)
  (interactive "P")

  (if (eq ite-zoom-unit nil)
      (setq ite-zoom-unit 10))
  
  (setq ite-zoom-unit (/ ite-zoom-unit 10.0))
  
  (ite-send-string (format "/obj%d load 0 4 getinterval exec BeginObject currentpoint transform"
			   (get-text-property (point) 'ite-number)))

  (ite-close-page)
  
  (ite-send-string (format "/Zoom %f 4 -2 roll ZoomMatrix def"
			   ite-zoom-unit))
  
  (if (> ite-debug-level 1)
      (ite-send-string "/Zoom load == flush"))
  
  (ite-display-page))


(defun ite-toggle-box ()
  (interactive)
  (setq ite-bb-only (not ite-bb-only))
  (if ite-bb-only 
      (progn
	(ite-send-string "/bbonly true def")
	;; Display bounding box
	(ite-send-query nil))
    (ite-redisplay)))


(defun ite-toggle-keys ()
  (interactive)
  (set-keymap-parent ite-select-keymap
		     (if (eq (keymap-parent ite-select-keymap) 
			     ite-keymap-A)
			 ite-keymap-B ite-keymap-A)))


(defun ite-redisplay ()
  (interactive)
  (ite-close-page)
  (ite-display-page))


(defun ite-zoom-out ()
  (interactive)
  (ite-close-page)
  (ite-send-string "/Zoom matrix def")
  (ite-display-page))


(defun ite-display-page ()
  (interactive)

  (let ((page (get-text-property (point) 'ite-page))
	(m 0)
	(p)
	(pp (point)))
    
    (ite-send-string (format "Zoom concat /bbonly %s def"
			     (if ite-bb-only "true" "false")))

    (while pp
      (setq p pp
	    pp (ite-prev pp t)))

    (while p
      (setq m (get-text-property p 'ite-number))
      (save-excursion
	(goto-char (first (ite-extend-region p)))
	(search-forward "(") (backward-char 1)
	(ite-format-check))
      (ite-send-string-nodict 
       (format "/info%d [%d %d %s %s %s %s] def"
	       m
	       (get-text-property p 'ite-x0)
	       (get-text-property p 'ite-y0)
	       (match-string 1) (match-string 2)
	       (match-string 3) (match-string 4)))
      
      (if (> ite-debug-level 1)
	  (ite-send-string (format "(/info%d = ) == info%d =="
				   m m)))
      (setq p (ite-next p t)))
    
    (if (> ite-debug-level 1) (ite-send-string-nodict "pstack"))
    (ite-send-string-nodict (format "(page%d.ps) run" page))
    (ite-send-string-nodict "flushpage")))


(defun ite-extend-region (p)
  (let ((begin) (end))
    (setq end (next-single-property-change p 'ite-number))
    (if end
	(setq begin (previous-single-property-change end 'ite-number)))
    (if (and begin end) (list begin end) nil)))


(defun ite-select (p)
  (let ((page (get-text-property p 'ite-page)) region (new-page nil))

    (if ite-current-object
	(let ((pos (marker-position (car ite-current-object))))
	  (if (equal (cdr ite-current-object)
		     (list (get-text-property pos 'ite-number)
			   (get-text-property pos 'ite-page)))
	      (progn
		(setq region (ite-extend-region pos))
		(add-text-properties (first region) (second region)
				     (list 'face 'ite-unselect-face
					   (if running-xemacs 'keymap 'local-map) ite-keymap))))

	  (if (not (= (nth 2 ite-current-object)
		      page))
	      (progn
		(ite-close-page)
		(ite-send-string "/Zoom matrix def")
		(setq new-page t))))
      (setq new-page t))

    (setq region (ite-extend-region p))
    (goto-char (first region))

    (add-text-properties (first region) (second region)
			 (list 'face 'ite-select-face
			       (if running-xemacs 'keymap 'local-map) ite-select-keymap))
    
    (setq ite-current-object (list (point-marker)
				   (get-text-property p 'ite-number)
				   page))
    (if new-page (ite-display-page))))


(defun ite-next-page ()
  (interactive)
  (let ((p (ite-next (point) nil)))
    (while (ite-same-page (point) p)
      (setq p (ite-next p nil)))

    (if p (ite-select p))))


(defun ite-prev-page ()
  (interactive)
  (let ((p (ite-prev (point) nil)))

    (while (ite-same-page (point) p)
      (setq p (ite-prev p nil)))

    (if p (ite-select p))))

	
(defun ite-same-page (p1 p2)
  (and p1 p2 
       (eq (get-text-property p1 'ite-page)
	   (get-text-property p2 'ite-page))))


(defun ite-mouse-select (event)
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (ite-select (event-point event)))
  

(defun ite-next (p same)
  (interactive)
  (let ((next))
    (setq next (next-single-property-change p 'ite-number))
    (if (and next
	     (not (get-text-property next 'ite-number)))
	(setq next (next-single-property-change next 'ite-number)))

    (if (or (not same)
	     (ite-same-page p next))
	next nil)))


(defun ite-prev (p same)
  (let ((prev))
    (setq prev (previous-single-property-change p 'ite-number))
    (if (and prev
	     (get-text-property prev 'ite-number))
	(setq prev (previous-single-property-change prev 'ite-number)))
    (if prev
	(setq prev (previous-single-property-change prev 'ite-number)))
  
    (if (or (not same)
	     (ite-same-page p prev))
	prev nil)))


(defun ite-next-select ()
  (interactive)
  (let ((p (ite-next (point) t)))
    (if p (ite-select p))))


(defun ite-prev-select ()
  (interactive)
  (let ((p (ite-prev (point) t)))
    (if p (ite-select p))))


(defun ite-send-query (erase)
  (if erase (ite-send-string "/Erase true def"))
  
  (ite-send-string-nodict (format "ITEDict /obj%d get exec flushpage"
				  (get-text-property (point) 'ite-number)))
  
  (if erase (ite-send-string "/Erase false def")))


(defun ite-format-check ()
  (let ((float "[+-]?[0-9]+\\.?[0-9]*"))
    (if (not (looking-at (format "(\\(%s\\) \\(%s\\) \\(%s\\) \\(%s\\))"
				 float float float float)))
	(ite-abort "Argument syntax error!"))))

(defun ite-update (n op formstring)
  (let ((value))
    (goto-char (first (ite-extend-region (point))))
    (search-forward "(") (backward-char 1)
    (ite-format-check)    
    (setq value (read (match-string n)))
    (goto-char (match-beginning n))
    (delete-region (point) (match-end n))
    (setq value (eval op))
    (ite-send-string (format
		      "info%s 2 %d add %s put"
		      (get-text-property (point) 'ite-number)
		      (1- n)
		      value))
    (insert-and-inherit (format formstring value))))

(defun ite-move (dx dy)
  (ite-send-query t)
  
  (if (not (= 0 dx))
      (ite-update 1 '(+ dx value) "%d"))

  (if (not (= 0 dy))
      (ite-update 2 '(+ dy value) "%d"))
  
  (ite-send-query nil))

  
(defun ite-rotate (dangle)
  (ite-send-query t)
  (ite-update 3 '(+ dangle value) "%d")
  (ite-send-query nil))


(defun ite-scale (factor)
  (ite-send-query t)
  (ite-update 4 '(* factor value) "%.3g")
  (ite-send-query nil))

(defun ite-key-syntax ()
  (interactive)
  (let (event)
    (while (and (setq event (read-key-sequence "hit a key "))
		(if running-xemacs
		    (setq event (events-to-keys event))
		  event)
		(equal event [button2up])))
    (end-of-line)
    (setq buffer-read-only nil)
    (insert (prin1-to-string event))
    (setq buffer-read-only t)))


(defun ite-set-keys ()
  (let (iter)
    
    (setq iter (list
                (list ite-toggle-key-bindings 'ite-toggle-keys)
                (list ite-toggle-bounding-box 'ite-toggle-box)
                (list ite-next-page 'ite-next-page)
                (list ite-previous-page 'ite-prev-page)
                (list ite-next-object 'ite-next-select)
                (list ite-previous-object 'ite-prev-select)
                (list ite-quit 'ite-kill)
                (list ite-redisplay 'ite-redisplay)
                (list ite-zoom-out 'ite-zoom-out)
                (list ite-zoom-in 'ite-zoom-in)
                (list ite-zoom-closer '(lambda () (interactive) (ite-zoom-in 15)))
                (list ite-zoom-closest '(lambda () (interactive) (ite-zoom-in 20)))))

    (while iter
      (let ((l (car iter)))
	(define-key ite-select-keymap (nth 0 l) (nth 1 l))
	(setq iter (cdr iter))))

    (setq iter
          (list
           (list ite-move-left '(ite-move -100 0))
           (list ite-move-left-slower '(ite-move -10 0))
           (list ite-move-left-slowest '(ite-move -1 0))
           (list ite-move-right '(ite-move 100 0))
           (list ite-move-right-slower '(ite-move 10 0))
           (list ite-move-right-slowest '(ite-move 1 0))
           (list ite-move-up '(ite-move 0 100))
           (list ite-move-up-slower '(ite-move 0 10))
           (list ite-move-up-slowest '(ite-move 0 1))
           (list ite-move-down '(ite-move 0 -100))
           (list ite-move-down-slower '(ite-move 0 -10))
           (list ite-move-down-slowest '(ite-move 0 -1))))
    
    (while iter
      (let ((l (car iter)))
	(define-key ite-keymap-A
	  (nth 0 l) `(lambda () (interactive) ,(nth 1 l)))
	(setq iter (cdr iter))))

    (setq iter (list
                (list ite-left-rotate '(ite-rotate 90))
                (list ite-left-rotate-slower '(ite-rotate 10))
                (list ite-left-rotate-slowest '(ite-rotate 1))    
                (list ite-right-rotate '(ite-rotate -90))
                (list ite-right-rotate-slower '(ite-rotate -10))
                (list ite-right-rotate-slowest '(ite-rotate -1))
                (list ite-magnify '(ite-scale 1.2))
                (list ite-magnify-slower '(ite-scale 1.04))
                (list ite-magnify-slowest '(ite-scale 1.01))
                (list ite-shrink '(ite-scale (/ 1 1.2)))
                (list ite-shrink-slower '(ite-scale (/ 1 1.04)))
                (list ite-shrink-slowest '(ite-scale (/ 1 1.01)))))
    
    (while iter
      (let ((l (car iter)))
	(define-key ite-keymap-B
	  (nth 0 l) `(lambda () (interactive) ,(nth 1 l)))
	(setq iter (cdr iter))))))

(defun ite-insert-keys (l)
  (while l
    (let ((e (car l)) p)
      (setq p (point))
      (insert (prin1-to-string (nth 1 e)))
      (setq p (- 20 (- (point) p)))
      (if (> p 0) (insert (make-string p ?\ )))
      (insert " --> " (nth 0 e) "\n")
      (setq l (cdr l)))))


(defun ite-browse-docs ()
  (browse-url url browse-url-new-window-p))

(defun ite-template ()
  (if (boundp 'browser-frame)
      (make-frame-visible (select-frame (next-frame))))
  (switch-to-buffer (generate-new-buffer "template.tex"))
  (insert-file-contents (expand-file-name "template.tex" ite-lib-path))
  (tex-mode)
  (save-buffer)
  (ite-menubar))

; Install menu.
(defun ite-menubar ()
  (let ((ite-menu
         (list "iTe"
               ["Start"      ite :keys "M-x ite" :active (not ite-current-object)]
               ["Stop"       ite-kill :keys "q" :active ite-current-object]
               (list "Templates"
                     ["Insert fixed block" 
                      (insert "\\begin{iteblock}(5cm,6cm)\n\\ITE(0 0 0 1) Hello\n\\ITE(0 0 0 1) World\n\\end{iteblock}\n") t]
                     ["Insert variable block" 
                      (insert "%Variable blocks need the trig-package\n\\begin{iteblock}\n\\ITE(0 0 0 1) Hello\n\\ITE(0 0 0 1) World\n\\end{iteblock}\n") t]
                     ["Insert header" 
                      (insert "\\usepackage{ite}\n%\\usepackage{trig}\n") t]
                     ["Create LaTeX file" 
                      (ite-template) :active t]
                     )
               ["Browse docs" (ite-browse-docs) :active t]
               ["Reload configuration"
                (progn 
                  (ite-read-configuration) 
                  (ite-dialog "Configuration read" '(("Ok" . nil))))
                :active t]
               ["Show log" ite-show-log :active t]
               ["Version" (ite-dialog (concat "iTe Version " ite-version) '(("Ok" . nil))) :active t])))

    (if running-xemacs
        (progn
          (add-submenu nil ite-menu)
          (set-menubar-dirty-flag))
      (progn
        (easy-menu-do-define 'ite-menu (current-local-map) "iTe Menu" ite-menu)
        (redraw-frame (window-frame (get-buffer-window (current-buffer)))))))
  (setq ite-menu-p t))


(defun ite-check-path (path)
  (if (not (file-readable-p path))
      (ite-abort (format "The configured file %s is not readable." path))))

; Check if the specified file is executable. Not that this function is not very useful because
; it will not do any shell expansions.
(defun ite-check-executable (path)
  (if (not (memq t (mapcar 'file-executable-p (list path (concat path ".exe") (concat path ".bat")))))
      (ite-log (format "The configured file %s was not found or is not executable." path))))

(defun ite-start-process (name buffer path options)
  (ite-log (format "Starting %s with %s %s" name path (mapconcat 'concat options ", ")))
  (condition-case exception 
      ; This value is return from this function.
      (eval (append '(start-process name buffer path) options))
    (error (ite-abort (format "Process %s failed: %s" name exception)))))

(defun ite ()
  (interactive)
  ;; Clean up. No parallel iTe process allowed.
  (ite-kill)

  (if (not ite-menu-p) (ite-menubar))
  
  (if (not ite-configuration-read-p) (ite-read-configuration))
  
  ;; Erase log buffer
  (if (marker-position ite-log-marker)
      (save-excursion
	;; Function delete-region in XEmacs accepts optional buffer argument!
	(set-buffer (get-buffer-create ite-log-buffer-name))
	(delete-region (marker-position ite-log-marker) (point-max))))
  
  ;; Is buffer modified or is not visiting a file?
  (if (and (= ite-debug-level 0)
	   (or (buffer-modified-p) (not (buffer-file-name))))
      (ite-abort "Warning:\n    Buffer is modified or not visiting a file.\n    Please save and compile it before running iTe again."))
  
  (save-excursion
    (let ((this-name) (master-name) (master-dir) (dviname))
      
;;; master-dir:   absolute directory of TeX-master.
;;; master-name:  file name of TeX-master relative to master-dir.
;;; this-name:    buffer-file-name relative to master-dir.
;;;
;;; For example, if buffer-file-name is '/usr/people/jones/tex/sub/foo.tex', then
;;; master-dir is '/usr/people/jones/tex/', master-name is 'master.tex' and
;;; this-name is 'sub/foo.tex'.
      
      (if (and (fboundp 'TeX-master-file) ; Is AUCTeX running?
	       (boundp 'TeX-master)
	       (stringp TeX-master))
	  (progn
	    (setq master-name (TeX-master-file))
	    (setq master-dir 
		  (expand-file-name
		   (concat (file-name-directory buffer-file-name)
			   (file-name-directory master-name))))
	    (setq master-name (concat (file-name-nondirectory master-name) ".tex"))
	    (setq this-name (file-relative-name buffer-file-name master-dir))
	    ;; Is this the first run of iTe for this file?
	    (if (and (not (string= master-name ite-master-name))
		     ;; If yes, ask if we really found the right master.
		     (= 2 (ite-dialog (format "Do you want to use %s as master file?" master-name)
				      '(("Yes" . 1) ("No" . 2)))))
		(setq master-name nil))))

      (if (not master-name)
	  ;; No AUC-TeX multifile document
	  (progn
	    (setq this-name (file-name-nondirectory buffer-file-name)
		  master-name this-name
		  master-dir default-directory)))
      
      (setq ite-master-name master-name)
      (setq dviname (concat (file-name-sans-extension master-name)
			    ".dvi"))
      
      ;; Switch to log buffer.
      (set-buffer (get-buffer-create ite-log-buffer-name))
      (cd master-dir)

      ;; Is dvi file up to date
      (if (or (not (file-exists-p dviname))
	      (file-newer-than-file-p this-name dviname))
	  (ite-abort (format "File %s does not exist or is outdated: Please recompile." dviname)))
      
      (setq ite-dir (make-temp-name (expand-file-name "ite" ite-temp-path)))
      (make-directory ite-dir)
      
      (setq ite-perl-process
            (ite-start-process
             "Perl"
             (get-buffer-create ite-log-buffer-name)
             ite-perl-path
             (append ite-perl-options (list (expand-file-name "ite.pl" ite-lib-path)))))

      (process-send-string ite-perl-process 
			   (concat ite-version "\n"
				   master-name "\n"
				   this-name "\n"
				   (ite-backslash-to-slash ite-dir) "\n"
				   ite-dvips-command "\n"))))
  
  (setq ite-log-string "")
  (set-process-filter ite-perl-process 'ite-filter)
  (set-process-sentinel ite-perl-process 'ite-process-listener))

(defun ite-process-listener (process event)
  (ite-log (process-name process) ": " event)
  (process-command process)
  
  
  (if (not (string-match "finished" event)) ; not "broken\\|exited\\|terminated"
      (ite-exit (concat "Process " (process-name process) " failed.\nSee ite log buffer for detail"))))

(defun ite-perl-callback ()
  (set-face-foreground 'ite-unselect-face ite-unselect-color)
  (set-face-background 'ite-select-face ite-select-color)

  (save-excursion
    (set-buffer (get-buffer-create ite-log-buffer-name))
    (cd ite-dir)

    (load-file "info.el")

    (setq ite-font-lock
	  (cond
	   ((memq 'font-lock-after-change-function after-change-functions)
	    'font-lock-after-change-function)
	   ((memq 'lazy-lock-after-change-function after-change-functions)
	    'lazy-lock-after-change-function)
	   (t nil)))
    
    (if ite-font-lock
	(remove-hook 'after-change-functions ite-font-lock t))
    
    ;; The '-c /iTe_GhostScriptStarted == flush' option makes appear the token 
    ;; in the gs process output and tells iTe that gs has started up.
    ;; The option '-' tells gs to read from stdin; it must be the last option. 
    
    (setq ite-gs-process 
	  (ite-start-process
	   "Ghostscript"
	   (current-buffer) 
	   ite-gs-path
	   (append ite-gs-options (list "-c" "/iTe_GhostScriptStarted == flush" "-"))))
    
    (setq ite-log-string "")
    (set-process-filter ite-gs-process 'ite-filter)
    (set-process-sentinel ite-gs-process 'ite-process-listener)))

(defun ite-gs-callback ()
  (ite-parse)
  (set-keymap-parent ite-select-keymap ite-keymap-A)
  
  (setq ite-bb-only t)

  ;; Send prolog
  (ite-send-string-nodict (format "(prolog.ps) run"))
  
  ;; We need a path without backslash to send to gs.
  (let ((lib-path-slash (ite-backslash-to-slash (expand-file-name ite-lib-path))))
    (ite-send-string-nodict (format "(Loading file %s/ite.ps) == flush\n" lib-path-slash))
    ;; Note that PostScript operator 'run' wants absolute file names.
    (ite-send-string-nodict (format "(%s/ite.ps) run\n" lib-path-slash)))

  (ite-send-string (format "/epslimit %d def" ite-eps-limit))
  
  (let ((p))
    (setq p (point))
    (if (not (get-text-property p 'ite-number))
	(progn
	  (setq dn (next-single-property-change p 'ite-number))
	  (setq dp (previous-single-property-change p 'ite-number))
	  (setq p (cond ((not dn) (ite-prev p nil))
			((not dp) (ite-next p nil))
			((< (- dn p) (- p dp)) (ite-next p nil))
			(t (ite-prev p nil))))))
    (ite-select p)))

(defconst ite-log-buffer-name "ite log")
(get-buffer-create ite-log-buffer-name)

(make-face 'ite-unselect-face)
(make-face 'ite-select-face)

;; Declare global variables.
(defconst ite-master-name nil)
(defconst ite-configuration-read-p nil "*was iTe already loaded successfully?")
(defconst ite-font-lock nil)
(defconst ite-log-string nil)
(defconst ite-current-object nil)
(defconst ite-filter nil)
(defconst ite-gs-process nil)
(defconst ite-dir nil)

(defconst ite-keymap (make-sparse-keymap))
(define-key ite-keymap (if running-xemacs [button2] [mouse-2]) 
  'ite-mouse-select)

(defconst ite-select-keymap (make-sparse-keymap))
(suppress-keymap ite-select-keymap)
(define-key ite-select-keymap (if running-xemacs [button2] [mouse-2])
  'ite-mouse-select)

(setq ite-keymap-A (make-sparse-keymap))
(setq ite-keymap-B (make-sparse-keymap))

;; Is menu installed?
(setq ite-menu-p nil)

;; Set default values for optional configuration items.
(defconst ite-temp-path (temp-directory))


;; The parent of the location from which this library was loaded.
(setq ite-home-path 
      (file-name-as-directory 
       (expand-file-name ".." (file-name-directory ite-load-path))))

; Note that in the following, 'defvar' only sets of not already set.

(defvar ite-lib-path (expand-file-name "lib" ite-home-path))

(defvar ite-doc-path (expand-file-name "doc" ite-home-path))

(defvar ite-config-files (list (expand-file-name "ite.conf" ite-home-path)))
