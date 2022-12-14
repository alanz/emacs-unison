;;; ucm.el --- Access Unison API in Emacs

;;; Commentary:

;; This code initially from chatGPT

;;; Code:

;; Note: See
;; https://github.com/unisonweb/unison/blob/trunk/unison-src/transcripts/api-getDefinition.output.md
;; The concatenation of "segment" fields reproduces the source code,
;; but annotated by type.

(require 'comint)
(require 'json)

(defvar api-endpoint nil)

(defun ucm-get-api-endpoint ()
  "Get the Unison API endpoint out of the *shell* buffer."

  (with-current-buffer "*shell*"
    (let* ((point-max-before (point-max))
           (output (progn
                     (goto-char (process-mark (get-buffer-process (current-buffer))))
                     (comint-send-string (get-buffer-process (current-buffer)) "api\n")
                     (accept-process-output (get-buffer-process (current-buffer)))
                     (buffer-substring point-max-before (point-max)))))

      ;; Extract the API endpoint URL from the output
      (let ((url-regexp "API: \\(http://[^\s]+\\)/api"))
        (string-match url-regexp output)
        (setq api-endpoint (substring-no-properties (match-string 1 output))))

      (message "api-endpoint:%s" api-endpoint)
      )
    )
  api-endpoint
  )

(defun ucm-call-api-endpoint ()
  "Access the API endpoint using the URL."
  (let ((url (concat api-endpoint "/api/find?query=nonexistent"))
        ;; (let ((url (concat api-endpoint "/api/"))
        (headers '(("Content-Type" . "application/json")
                   ("Accept" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (message "response: %s" (buffer-substring (point-min) (point-max)))
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        (let ((json-object-type 'hash-table))
          (json-read))))))

(provide 'ucm)
;;; ucm.el ends here
