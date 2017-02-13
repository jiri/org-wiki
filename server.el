;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; TODO: Make this better
(setq org-html-head-extra
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

(defvar wiki-directory "~/Org/wiki")
(defvar wiki-extra-export-options '((org-html-doctype "html5")
				    (org-html-head-include-default-style nil)
				    (org-export-with-section-numbers nil)
				    (org-html-htmlize-output-type css)))
(defvar wiki-installation-directory (file-name-directory
				     (or load-file-name buffer-file-name)))

(defun render-org-file (path)
  (with-current-buffer (find-file-noselect path)
    (progv
	(mapcar 'first wiki-extra-export-options)
	(mapcar 'second wiki-extra-export-options)
      (org-export-as 'html))))

(defun process-path (path)
  (if (file-directory-p path)
      (concat (file-name-as-directory path) "index.org")
    path))

(defun my-elnode-dispatch (httpcon)
  (if (equal (elnode-http-params httpcon) '(("edit")))
      (my-elnode-edit-handler httpcon)
    (my-elnode-org-handler httpcon)))

(defun my-elnode-org-handler (httpcon)
  (elnode-docroot-for wiki-directory
    with path
    on httpcon
    do (let ((html (render-org-file (process-path path))))
    	 (elnode-send-html httpcon html))))

(defun my-elnode-edit-handler (httpcon)
  (with-selected-frame (make-frame '((window-system . ns)
				     (client . nowait)))
    (let ((path (elnode-get-targetfile httpcon wiki-directory)))
      (if (file-directory-p path)
	  (find-file (concat (file-name-as-directory path) "index.org"))
	(find-file path)))
    (x-focus-frame nil))

  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon " "))

;; TODO: Automate this
(defun style (httpcon)
  (let* ((file (concat wiki-installation-directory "style.css")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/css"))
    (elnode-send-file httpcon file)))

(defun theme (httpcon)
  (let* ((file (concat wiki-installation-directory "theme.css")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/css"))
    (elnode-send-file httpcon file)))

(defun mousetrap (httpcon)
  (let* ((file (concat wiki-installation-directory "mousetrap.min.js")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/javascript"))
    (elnode-send-file httpcon file)))

(defvar my-app-routes `(("^.*//style.css" . style)
			("^.*//theme.css" . theme)
			("^.*//mousetrap.min.js" . mousetrap)
			("^.*//\\(.*\\)" . my-elnode-dispatch)))

;; TODO: Export this
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))

(elnode-start 'root-handler :port 8009)
;; (elnode-stop 8009)

;; (elnode-start 'my-elnode-org-handler :port 8002 :host "0.0.0.0")
;; (elnode-stop 8002)
