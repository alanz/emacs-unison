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
(require 'unisonlang-mode)

(defvar api-endpoint nil)

(defun ucm-start ()
  "Do the first things. To help with debugging."
  (interactive)
  (ucm-get-api-endpoint)
  (ucm-api-list)
  (ucm-list))

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
  "Get the UCM fully qualified namespace."
  (gethash "namespaceListingFQN" (ucm-codebase-list ucm-result)))

(defun ucm-namespace-hash ()
  "Get the hash of the UCM namespace."
  (gethash "namespaceListingHash" (ucm-codebase-list ucm-result)))

(defun ucm-namespace-children ()
  (gethash "namespaceListingChildren" (ucm-codebase-list ucm-result)))

(defun ucm-list ()
  (let* ((root (ucm-namespace-fqn))
         (children (ucm-namespace-children)))
    (message "> %s" root)
    (mapc (lambda (child )
            ;; (message "  : %s" child)
            ;; (message "  : %s" (gethash "tag" child))
            ;; (message "  : %s" (gethash "contents" child))
            (let ((tag (gethash "tag" child))
                  (contents (gethash "contents" child)))
                  ;; (message "  : %s" (gethash "tag" child))
                  ;; (message "  : %s" (gethash "contents" child))
                  (message "  : %s (%s entries)"
                           (gethash "namespaceName" contents)
                           (gethash "namespaceSize" contents))
                  )
            )
          children)
    ;; (maphash (lambda (key value)
    ;;        (message "%s: %s" key value))
    ;;      child)
    nil
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

;; ---------------------------------------------------------------------
;; REPL
;; Via chatGPT and https://www.masteringemacs.org/article/comint-writing-command-interpreter


(defun init-repl-buffer ()
  "Initialize the REPL buffer and set the major mode to `comint-mode`."
  (switch-to-buffer (get-buffer-create "*ucm-repl*"))
  (comint-mode))

(defun repl-loop ()
  "A simple REPL loop implemented in Elisp using `comint-mode`."
  (while t
    (message "Top of loop")
    (comint-send-input)
    (let ((input (comint-get-old-input)))
      (if (string= input "exit")
          (cl-return)
        (let ((result (eval (read input))))
          (comint-output-filter (current-buffer) (format "%s\n" result)))))))

;; (defun ucm-repl ()
;;   "Start a REPL in a new buffer using `comint-mode`."
;;   (interactive)
;;   (init-repl-buffer)
;;   (repl-loop))

;; ---------------------------------------------------------------------

(defcustom ucm-prompt "UCM> "
  "Prompt used in UCM.
Setting this variable does not affect existing UCM runs.

Interrupting the UCM process with \\<ielm-map>\\[comint-interrupt-subjob],
and then restarting it using \\[ielm], makes the then current
default value affect _new_ prompts.  Unless the new prompt
differs only in text properties from the old one, UCM will no
longer recognize the old prompts.  However, executing \\[ielm]
does not update the prompt of an *ielm* buffer with a running process.
For UCM buffers that are not called `*ielm*', you can execute
\\[inferior-emacs-lisp-mode] in that UCM buffer to update the value,
for new prompts.  This works even if the buffer has a running process."
  :type 'string)

(defvar ucm-prompt-internal "UCM> "
  "Stored value of `ucm-prompt' in the current UCM buffer.
This is an internal variable used by UCM.  Its purpose is to
prevent a running UCM process from being messed up when the user
customizes `ucm-prompt'.")


(defcustom ucm-dynamic-return t
  "Controls whether \\<ucm-map>\\[ucm-return] has intelligent behavior in UCM.
If non-nil, \\[ucm-return] evaluates input for complete sexps, or inserts a newline
and indents for incomplete sexps.  If nil, always inserts newlines."
  :type 'boolean)

(defcustom ucm-dynamic-multiline-inputs t
  "Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs."
  :type 'boolean)


(defcustom ucm-prompt-read-only t
  "If non-nil, the UCM prompt is read only.
The read only region includes the newline before the prompt.
Setting this variable does not affect existing UCM runs.
This works by setting the buffer-local value of `comint-prompt-read-only'.
Setting that value directly affects new prompts in the current buffer.

If this option is enabled, then the safe way to temporarily
override the read-only-ness of UCM prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your init file:

\(add-hook \\='ucm-mode-hook
          (lambda ()
             (define-key ucm-map \"\\C-w\" \\='comint-kill-region)
             (define-key ucm-map [C-S-backspace]
               \\='comint-kill-whole-line)))

If you set `comint-prompt-read-only' to t, you might wish to use
`comint-mode-hook' and `comint-mode-map' instead of
`ucm-mode-hook' and `ucm-map'.  That will affect all comint
buffers, including UCM buffers.  If you sometimes use UCM on
text-only terminals or with `emacs -nw', you might wish to use
another binding for `comint-kill-whole-line'."
  :type 'boolean
  :version "22.1")

;; ---------------------------------------------------------------------

;;; System variables

(defvar ucm-working-buffer nil
  "Buffer in which UCM expressions will be evaluated.
This variable is buffer-local.")

(defvar ucm-header
  "*** Welcome to UCM ***  Type (describe-mode) for help.\n"
  "Message to display when UCM is started.")

(defvaralias 'inferior-ucm-mode-map 'ucm-map)
(defvar ucm-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\t" 'ucm-tab)
    (define-key map "\C-m" 'ucm-return)
    (define-key map "\e\C-m" 'ucm-return-for-effect)
    (define-key map "\C-j" 'ucm-send-input)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    (define-key map "\e\t" 'completion-at-point)  ; lisp-interaction-mode
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; Some convenience bindings for setting the working buffer
    (define-key map "\C-c\C-b" 'ucm-change-working-buffer)
    (define-key map "\C-c\C-f" 'ucm-display-working-buffer)
    (define-key map "\C-c\C-v" 'ucm-print-working-buffer)
    map)
  "Keymap for UCM mode.")

;; ---------------------------------------------------------------------

(defvar ucm-input)

(defun ucm-input-sender (_proc input)
  "Set the variable ucm-input to INPUT.
ucm-input is in the scope of `ucm-send-input's call."
  (setq ucm-input input))

(defun ucm-send-input (&optional for-effect)
  "Evaluate the Emacs Lisp expression after the prompt.
Blah blah FOR-EFFECT"
  (interactive)
  (let (ucm-input)                     ; set by ucm-input-sender
    (comint-send-input)                ; update history, markers etc.
    (ucm-eval-input ucm-input for-effect)))

;;; Other bindings

;; ---------------------------------------------------------------------
(defun ucm-return (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ucm-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if ucm-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (ucm-pm)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (ucm-send-input for-effect)
          (when (and ucm-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (ucm-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))
;; ---------------------------------------------------------------------

(defun ucm-eval-input (input-string &optional for-effect)
  "Evaluate the Unisone expression INPUT-STRING, and pretty-print the result.
Do something with FOR-EFFECT."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (message "ucm-eval-input: %s" input-string)
  (let (
        (output "")                  ; result to display
        )

    (setq output (format "ucm-eval-input: %s\n" input-string))
    (setq output (concat output ucm-prompt-internal))
    (comint-output-filter (ucm-process) output)))

;; ---------------------------------------------------------------------

;;; Process and marker utilities

(defun ucm-process nil
  "Return the current buffer's process."
  (get-buffer-process (current-buffer)))

(defun ucm-pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (current-buffer))))

(defun ucm-set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;; ---------------------------------------------------------------------
;;; Major mode
(define-derived-mode inferior-ucm-mode comint-mode "UCM"
  "Major mode for interactively evaluating Unison expressions."
  :syntax-table unisonlang-mode-syntax-table

  (setq comint-prompt-regexp (concat "^" (regexp-quote ucm-prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'ucm-input-sender)
  (setq comint-process-echoes nil)

  (setq-local ucm-prompt-internal ucm-prompt)
  (setq-local comint-prompt-read-only ucm-prompt-read-only)
  (setq comint-get-old-input 'ucm-get-old-input)

  (setq mode-line-process '(":%s on " (:eval (buffer-name ielm-working-buffer))))

  (setq-local ielm-working-buffer (current-buffer))

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "ucm" (current-buffer) "hexl")
      (file-error (start-process "ucm" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (ucm-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    ;; (setq-local comint-inhibit-carriage-motion t)

    ;; Add a silly header
    (insert ucm-header)
    (ucm-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (ucm-process) ucm-prompt-internal)
    (set-marker comint-last-input-start (ucm-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun ucm-get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;; ---------------------------------------------------------------------
;;;###autoload

(defun ucm-repl (&optional buf-name)
  "Interactively evaluate Unison expressions.
Switches to the buffer named BUF-NAME if provided (`*ucm*' by default),
or creates it if it does not exist."
  (interactive)
  (let (old-point
        (buf-name (or buf-name "*ucm*")))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (inferior-ucm-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point (push-mark old-point))))

;; ---------------------------------------------------------------------

(provide 'ucm)
;;; ucm.el ends here
