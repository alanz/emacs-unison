;;; ucm.el --- Access Unison API in Emacs

;;; Commentary:

;; This code initially from chatGPT

;;; Code:

;; Note: See
;; https://github.com/unisonweb/unison/blob/trunk/unison-src/transcripts/api-getDefinition.output.md
;; The concatenation of "segment" fields reproduces the source code,
;; but annotated by type.

(require 'cl-generic)
(require 'cl-lib)
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

(defun ucm-call-api-endpoint (verb)
  "Access the API endpoint VERB using the URL."
  (let ((url (concat api-endpoint "/api/" verb))
        (headers '(("Content-Type" . "application/json")
                   ("Accept" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (message "response: %s" (buffer-substring (point-min) (point-max)))
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        (let ((json-object-type 'hash-table))
          (json-read))))))

;; ---------------------------------------------------------------------
;; Data structures

(cl-defstruct ucm-codebase
  (list nil))

(defalias 'make-lsp-client 'make-lsp--client)

(defvar ucm-result (make-ucm-codebase))

;; ----------------------------------------------------------------------

;; State of the world

(defun ucm-namespace-fqn ()
  (gethash "namespaceListingFQN" (ucm-codebase-list ucm-result)))

(defun ucm-namespace-hash ()
  (gethash "namespaceListingHash" (ucm-codebase-list ucm-result)))

(defun ucm-namespace-children ()
  (gethash "namespaceListingChildren" (ucm-codebase-list ucm-result)))

(defun ucm-list ()
  (let* ((root (ucm-namespace-fqn))
         (children (ucm-namespace-children)))
    (message "> %s" root)
    (mapc (lambda (child )
            (message "  : %s" child))
          children)
    ;; (maphash (lambda (key value)
    ;;        (message "%s: %s" key value))
    ;;      child)
    )
  ;; (maphash (lambda (key value)
  ;;        (message "%s: %s" key value))
  ;;      children)
  )

;; ---------------------------------------------------------------------
;; API calls


(defmacro with-ucm-api-endpoint (&rest body)
  "Execute BODY if the ucm API endpoint, is defined."
  `(if api-endpoint
      (progn
        ,@body)
    (message "api-endpoint not set")))

(defun ucm-api-list()
  "Call the LIST endpoint."
  (with-ucm-api-endpoint
   (let ((result (ucm-call-api-endpoint "list")))
     (setf (ucm-codebase-list ucm-result) result))))

;; get-projects
(defun ucm-api-get-projects()
  "Call the PROJECTS endpoint."
  (with-ucm-api-endpoint
   (let ((result (ucm-call-api-endpoint "projects")))
     (setq ucm-result result))))

(defun show-ucm-result ()
  (maphash (lambda (key value)
             (message "%s: %s" key value))
           ucm-result))

(provide 'ucm)
;;; ucm.el ends here
