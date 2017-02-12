;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq org-html-head-extra "
<script type=\"text/javascript\" src=\"/mousetrap.min.js\"></script>
<script type=\"text/javascript\">
Mousetrap.bind('ctrl+x ctrl+f', function() {
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.open(\"GET\", window.location.protocol + \"//\" + window.location.host + \"/edit\" + window.location.pathname + \"?edit\", true);
    xmlHttp.send(null);
});
</script>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\" />
")

;; file-relative-name filename &optional directory

(setq org-html-head-include-default-style nil)
(setq org-html-doctype "html5")

(defvar my-org-dir "~/Org/wiki")
(defvar extra-export-options '((org-html-doctype "html5")
			       (org-html-head-include-default-style nil)))

(defun render-org-file (path)
  (with-current-buffer (find-file-noselect path)
    (progv
	(mapcar 'first extra-export-options)
	(mapcar 'second extra-export-options)
      (org-export-as 'html))))

(defun my-elnode-org-handler (httpcon)
  (elnode-docroot-for my-org-dir
    with path
    on httpcon
    do (cond ((file-directory-p path)
	      (let* ((path (concat (file-name-as-directory path) "index.org"))
		    (html (render-org-file path)))
		(elnode-send-html httpcon html)))
	     ((file-regular-p path)
	      (let ((html (render-org-file path)))
		(elnode-send-html httpcon html))))))

(defun my-elnode-edit-handler (httpcon)
  ;; (message "%s" (elnode-http-params httpcon))

  (with-selected-frame (make-frame '((window-system . ns)
				     (client . nowait)))
    (let ((path (elnode-get-targetfile httpcon my-org-dir)))
      (if (file-directory-p path)
	  (find-file (concat (file-name-as-directory path) "index.org"))
	(find-file path)))
    (x-focus-frame nil))

  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon " "))

(defun style (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/css"))
  (elnode-send-file httpcon "~/style.css"))

(defun mousetrap (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/javascript"))
  (elnode-send-file httpcon "~/Downloads/mousetrap.min.js"))

(defvar my-app-routes `(("^.*//style.css" . style)
			("^.*//mousetrap.min.js" . mousetrap)
			("^.*//edit/\\(.*\\)" . my-elnode-edit-handler)
			("^.*//\\(.*\\)" . my-elnode-org-handler)))

(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))

(elnode-start 'root-handler :port 8009)
;; (elnode-stop 8009)

;; (elnode-start 'my-elnode-org-handler :port 8002 :host "0.0.0.0")
;; (elnode-stop 8002)
