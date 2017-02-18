(defconst org-wiki-base
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst default-mousetrap-file
  (expand-file-name "mousetrap.min.js" org-wiki-base))

(defconst default-style-file
  (expand-file-name "style.css" org-wiki-base))

(defconst default-404-file
  (expand-file-name "404.html" org-wiki-base))

;; TODO: Make this better
(defconst head-extra
  (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />"
	  "<script type=\"text/javascript\" src=\"/mousetrap.min.js\"></script>"
	  "<script type=\"text/javascript\">
             Mousetrap.bind('ctrl+x ctrl+f', function() {
               var xmlHttp = new XMLHttpRequest();
               xmlHttp.open(\"GET\", window.location.href + \"?edit\", true);
               xmlHttp.send(null);
             });
           </script>"
	  "<link rel=\"stylesheet\" type=\"text/css\" href=\"/theme.css\" />"))

(defvar wiki-extra-export-options '((org-html-doctype "html5")
				    (org-html-head-include-default-style nil)
				    (org-export-with-section-numbers nil)
				    (org-html-htmlize-output-type css)
				    (org-html-scripts nil)))
(defvar org-wiki/instances nil)

;; Convenience function
;; TODO: Hide these
(defun org-wiki/render-file (path)
  (let ((org-html-head-extra head-extra)
	(org-html-link-org-files-as-html nil))
    (with-current-buffer (find-file-noselect path)
      (progv
	  (mapcar 'first wiki-extra-export-options)
	  (mapcar 'second wiki-extra-export-options)
	(org-export-as 'html)))))

(defun org-wiki/generate-css ()
  (with-temp-buffer
    (require 'helm)
    (compilation-mode))

  (window-configuration-to-register :org-wiki/html)
  (org-html-htmlize-generate-css)

  (goto-char (point-min))
  (if (re-search-forward "<!--" nil t)
      (delete-region (point-min) (+ (match-beginning 0) 4)))
  (if (re-search-forward "^\s*a\s*{" nil t)
      (delete-region (- (match-end 0) 3) (point-max)))
  (let ((s (buffer-string)))
    (jump-to-register :org-wiki/html)
    (replace-regexp-in-string "\\(^\s*\\)body\\(\s*{\\)" "\\1.src\\2" s)))

(defun org-wiki/process-path (path)
  (cond ((file-directory-p path)
	 (concat (file-name-as-directory path) "index.org"))
	((file-regular-p (concat path ".org"))
	 (concat path ".org"))
	((file-regular-p path) path)))

(defun org-wiki/connection-port (httpcon)
  (let ((host (elnode-server-info httpcon)))
    (string-match "\\([^:]+\\)\\(:\\([0-9]+\\)\\)*" host)
    (match-string-no-properties 3 host)))

(defun org-wiki/root-for (httpcon)
  (let ((port (org-wiki/connection-port httpcon)))
    (cdr (assoc (string-to-number port) org-wiki/instances))))

;; Handlers
(defun org-wiki/dispatch (httpcon)
  (if (equal (elnode-http-params httpcon) '(("edit")))
      (org-wiki/edit httpcon)
    (org-wiki/render httpcon)))

;; TODO: Implement this manually
(defun org-wiki/render (httpcon)
  (elnode-docroot-for (org-wiki/root-for httpcon)
    with path
    on httpcon
    do (let ((html (org-wiki/render-file (org-wiki/process-path path))))
	 (elnode-send-html httpcon html))))

(defun org-wiki/render (httpcon)
  (let* ((file (elnode-get-targetfile httpcon (org-wiki/root-for httpcon)))
	(path (org-wiki/process-path file)))
    (if path
	(progn
	  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
	  (elnode-send-html httpcon (org-wiki/render-file path)))
      (progn
	(elnode-http-start httpcon 200 '("Content-type" . "text/html"))
	(elnode-send-file httpcon default-404-file)))))

(defun org-wiki/edit (httpcon)
  (with-selected-frame (make-frame '((window-system . ns)
				     (client . nowait)))
    (let ((path (elnode-get-targetfile httpcon (org-wiki/root-for httpcon))))
      (find-file (org-wiki/process-path path)))
    (x-focus-frame nil))

  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon " "))

;; Static file handlers
;; TODO: Automate this
(defun org-wiki/style (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/css"))
  (elnode-send-file httpcon default-style-file))

(defun org-wiki/mousetrap (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/javascript"))
  (elnode-send-file httpcon default-mousetrap-file))

(defun org-wiki/theme (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/css"))
  (with-selected-frame (make-frame '((window-system . ns)
				     (client . nowait)
				     (visibility . nil)))
    (let ((css (org-wiki/generate-css)))
      (delete-frame)
      (elnode-send-html httpcon css))))

;; Root handler
(defconst org-wiki/routes `(("^.*//style.css" . org-wiki/style)
			    ("^.*//theme.css" . org-wiki/theme)
			    ("^.*//mousetrap.min.js" . org-wiki/mousetrap)
			    ("^.*//\\(.*\\)" . org-wiki/dispatch)))

(defun org-wiki/root (httpcon)
  (elnode-hostpath-dispatcher httpcon org-wiki/routes))

;; API functions
;;;###autoload
(defun org-wiki/start (root &optional port)
  (let ((p (or port 8000)))
    (unless (assoc port org-wiki/instances)
      (push `(,p . ,root) org-wiki/instances)
      (elnode-start 'org-wiki/root :port p))))

;;;###autoload
(defun org-wiki/stop (ref)
  (cond ((and (numberp ref) (assoc ref org-wiki/instances))
	 (elnode-stop ref)
	 (setq org-wiki/instances
	       (delq (assoc ref org-wiki/instances)
		     org-wiki/instances)))
	((and (stringp ref) (rassoc ref org-wiki/instances))
	 (elnode-stop (car (rassoc ref org-wiki/instances)))
	 (setq org-wiki/instances
	       (delq (rassoc ref org-wiki/instances)
		     org-wiki/instances)))))

;; Module provisions
(provide 'org-wiki)
