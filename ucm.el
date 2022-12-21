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
(require 'parsec)

(defvar api-endpoint nil)

(defun ucm-start ()
  "Do the first things. To help with debugging."
  (interactive)
  (ucm-get-api-endpoint)
  (ucm--api-list)
  )

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

(defun ucm-call-api-endpoint (verb &optional params)
  "Access the API endpoint VERB using the PARAMS."
  (let ((url (concat api-endpoint "/api/" verb (params-string params)))
        (headers '(("Content-Type" . "application/json")
                   ("Accept" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (message "response: %s" (buffer-substring (point-min) (point-max)))
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        (let ((json-object-type 'hash-table))
          (json-read))))))

(defun params-string (params)
  "Turn the list of PARAMS into a query string."
  (if params
      (let ((res "?")
            (not-first nil))
        (dolist (p params)
          (message "p:%s" p)
          (if not-first
              (setq res (concat res (format "&%s=%s" (car p) (cadr p))))
            (setq res (concat res (format "%s=%s" (car p) (cadr p))))
            (setq not-first t)))
        res)
    ""))


;; ---------------------------------------------------------------------
;; Closer to what mastodon.el does


(defun ucm--response ()
  "Capture response buffer content as string."
  (with-current-buffer (current-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ucm--response-body (pattern)
  "Return substring matching PATTERN from `ucm--response'."
  (let ((resp (ucm--response)))
    (string-match pattern resp)
    (match-string 0 resp)))

(defun ucm--status ()
  "Return HTTP Response Status Code from `ucm--response'."
  (let* ((status-line (ucm--response-body "^HTTP/1.*$")))
    (string-match "[0-9][0-9][0-9]" status-line)
    (match-string 0 status-line)))


(defun ucm--http-triage (response success)
  "Determine if RESPONSE was successful. Call SUCCESS if successful.

Message status and JSON error from RESPONSE if unsuccessful."
  (let ((status (with-current-buffer response
                  (ucm--status))))
    (if (string-prefix-p "2" status)
        (funcall success)
      ;; don't switch to buffer, just with-current-buffer the response:
      ;; (switch-to-buffer response)
      ;; 404 sometimes returns http response so --process-json fails:
      (if (string-prefix-p "404" status)
          (message "Error %s: page not found" status)
        (let ((json-response (with-current-buffer response
                               (ucm--process-json))))
          (message "Error %s: %s" status (alist-get 'error json-response)))))))


(defun ucm-call-api-endpoint2 (verb &optional params)
  "Access the API endpoint VERB using the PARAMS."
  (let ((url (concat api-endpoint "/api/" verb (ucm--build-params-string params)))
        (headers '(("Content-Type" . "application/json")
                   ("Accept" . "application/json"))))
    (url-retrieve-synchronously url)))

(defun ucm--build-params-string (params)
  "Build a request parameters string from parameters alist PARAMS."
  ;; (url-build-query-string args nil))
  ;; url-build-query-string adds 'nil' to empty params so lets stay with our
  ;; own:
  (if params
      (concat "?"
              (mapconcat (lambda (p)
                           (concat (url-hexify-string (car p))
                                   "="
                                   (url-hexify-string (cdr p))))
                         params
                         "&"))
    ""))

(defun ucm--get-response (verb &optional params no-headers silent vector)
  "Make synchronous GET request using VERB. Return JSON and response headers.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
NO-HEADERS means don't collect http response headers.
VECTOR means return json arrays as vectors."
  ;; (with-current-buffer (ucm-call-api-endpoint2 verb params silent)
  (with-current-buffer (ucm-call-api-endpoint2 verb params)
    (ucm--process-response no-headers vector)))

(defun ucm--get-json (verb &optional params silent vector)
  "Return only JSON data from VERB api request.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
VECTOR means return json arrays as vectors."
  (car (ucm--get-response verb params :no-headers silent vector)))

(defun ucm--process-response (&optional no-headers vector)
  "Process http response.
Return a cons of JSON list and http response headers.
If NO-HEADERS is non-nil, just return the JSON.
VECTOR means return json arrays as vectors."
  ;; view raw response:
  ;; (switch-to-buffer (current-buffer))
  (message "status:%s" (ucm--status))
  (let ((status (ucm--status)))
    (if (string-prefix-p "2" status)
        (let ((headers (unless no-headers
                         (ucm--process-headers))))
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let ((json-array-type (if vector 'vector 'list))
                (json-string
                 (decode-coding-string
                  (buffer-substring-no-properties (point) (point-max))
                  'utf-8)))
            (kill-buffer)
            (unless (or (string-empty-p json-string) (null json-string))
              `(,(json-read-from-string json-string) . ,headers))))
      (message "got status:%s" status))))

(defun ucm--process-headers ()
  "Return an alist of http response headers."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let* ((head-str (buffer-substring-no-properties
                    (point-min)
                    (re-search-forward "^$" nil 'move)))
         (head-list (split-string head-str "\n")))
    (mapcar (lambda (x)
              (let ((list (split-string x ": ")))
                (cons (car list) (cadr list))))
            head-list)))

;; ---------------------------------------------------------------------
;; Data structures

(cl-defstruct ucm-codebase
  (list nil)
  (projects nil))

(defvar ucm-result (make-ucm-codebase))

;; ---------------------------------------------------------------------
;; API

(defun ucm--api-list (&optional params)
  "Call the UCM LIST endpoint with optional PARAMS.

PARAMETERS

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace
         root. If left absent, the most recent root will be used.
- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names
         will be resolved and displayed. If left absent, the root
         namespace will be used.E.g. base.List
- namespace
     - **Values**: **
     - **Description**: The namespace required by the endpoint.If
         left absent, the relativeTo namespace will be used.E.g.
         base.List"
  (let ((result (ucm--get-json "list" params)))
    (message "result:%s" result)
    (setf (ucm-codebase-list ucm-result) result)))


;; get-projects
(defun ucm--api-projects(&optional params)
  "Call the PROJECTS endpoint with optional PARAMS."
  (message "ucm--api-projects:params=%s" params)
  (with-ucm-api-endpoint
   (let ((result (ucm--get-json "projects" params)))
     (setf (ucm-codebase-projects ucm-result) result))))


;; ----------------------------------------------------------------------

;; State of the world

(defun ucm-namespace-fqn ()
  "Get the UCM fully qualified namespace."
  (alist-get 'namespaceListingFQN (ucm-codebase-list ucm-result)))

(defun ucm-namespace-hash ()
  "Get the hash of the UCM namespace."
  (alist-get 'namespaceListingHash (ucm-codebase-list ucm-result)))

(defun ucm-namespace-children ()
  "Get the hash of the UCM namespace children."
  (alist-get 'namespaceListingChildren (ucm-codebase-list ucm-result)))


;; ---------------------------------------------------------------------
;; API calls


(defmacro with-ucm-api-endpoint (&rest body)
  "Execute BODY if the ucm API endpoint, is defined."
  `(if api-endpoint
      (progn
        ,@body)
    (message "api-endpoint not set")))

;; ---------------------------------------------------------------------

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
  (let ((output "")                  ; result to display
        )

    (setq output (pcase (parse-command input-string)
                   (`("list" ,target) (do-list-command target))
                   (`("cd"   ,target) (do-cd-command target))
                   (_ "unknown command\n")))
    (setq output (concat output ucm-prompt-internal))
    (comint-output-filter (ucm-process) output)))

;; ---------------------------------------------------------------------
;; Commmand parser

(defun parse-command (input)
  "Parse the INPUT, returning a command or an error."
  (message "parse-command:input=[%s]" input)
  (message "parse-command:output=[%s]" (parsec-with-input input
                                         (parse-one-command)))
  (parsec-with-input input
    (parse-one-command)))

(defun parse-one-command ()
  "Parse a single REPL command."
  (parsec-or (parse-list-command)
             (parse-cd-command)
             ))

(defun parse-list-command ()
  "Parse the LIST command."
  (parsec-or (parsec-str "ls")
             (parsec-str "list"))
  (let ((target (parsec-optional
                 (progn
                   (parsec-many1 (parsec-str " "))
                   (parse-namespace)))))
    (list "list" target)))

(defun parse-cd-command ()
  "Parse the CD command."
  (parsec-str "cd")
  (let ((target (parsec-optional
                 (progn
                   (parsec-many1 (parsec-str " "))
                   (parse-namespace)))))
    (list "cd" target)))

(defun parse-namespace ()
  "Parse a name space."
  (parsec-many1-as-string
   (parsec-or (parsec-letter)
              (parsec-digit)
              (parsec-str ".")
              (parsec-str "_"))))

;; ---------------------------------------------------------------------
;; Pulling out data

  ;; 14. d0302_fun      (Nat -> Set Char -> [Text] -> [Set Char])

  ;; ((tag . TermObject)
  ;;  (contents
  ;;   (termHash . #i4n5lohv63j2rn6io5mjc19pg925p7skk5palm80i0mmodmksecbt8l0nu68lvi7c3uh15k9b89popfdbhr01gmqrv427kvhrlak2pg)
  ;;   (termName . d0302_fun)
  ;;   (termTag . Plain)
  ;;   (termType
  ;;    ((annotation (contents . ##Nat) (tag . HashQualifier)) (segment . ##Nat))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (tag . TypeOperator)) (segment . ->))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (contents . #prrhin67ce) (tag . HashQualifier)) (segment . #prrhin67ce))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (contents . ##Char) (tag . HashQualifier)) (segment . ##Char))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (tag . TypeOperator)) (segment . ->))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (tag . DelimiterChar)) (segment . [))
  ;;     ((annotation (contents . ##Text) (tag . HashQualifier)) (segment . ##Text))
  ;;     ((annotation (tag . DelimiterChar)) (segment . ]))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (tag . TypeOperator)) (segment . ->))
  ;;    ((annotation) (segment .  ))
  ;;    ((annotation (tag . DelimiterChar)) (segment . [)) ((annotation (contents . #prrhin67ce) (tag . HashQualifier)) (segment . #prrhin67ce)) ((annotation) (segment .  )) ((annotation (contents . ##Char) (tag . HashQualifier)) (segment . ##Char)) ((annotation (tag . DelimiterChar)) (segment . ])))))

(defun extract-segments (contents)
  "Extract all the `segment` markers from CONTENTS, recursively."
  (message "extract-segments:contents=%s" contents)
  (let (res)
    (dolist (item contents)
      (message "item:%s" item)
      (pcase item
        (`(,annotation (segment . ,segment)) (progn
                                 (message "extract-segments: matched:1:[%s]" segment)
                                 (push segment res)))
        (_ (message "item pcase:%s" item))
        )
      ;; (let ((segment (alist-get 'segment item)))
      ;;   (message "segment:%s" segment)
      ;;   (if segment
      ;;       (push segment res))
      ;; )
    )
    (message "extract-segments:res=%s" res)
    res)
  )

;; ---------------------------------------------------------------------
;; REPL commands

(defun do-list-command (&optional target)
  "List the current directory, or TARGET if given."
  (message "do-list-command:target=%s" target)
  (when target
    (let ((namespace target))
      (ucm--api-list `(("namespace" . ,target)))))
  (let ((output ""))
    (let* ((root (ucm-namespace-fqn))
           (children (ucm-namespace-children)))
      (setq output (concat output (format "> %s\n" root)))
      (mapc (lambda (child )
              (let ((tag (alist-get 'tag child))
                    (contents (alist-get 'contents child)))
                (message "tag:%s" tag)
                (pcase tag
                  ("Subnamespace" (setq output (concat output (format "  %s (%s entries)\n"
                                                    (alist-get 'namespaceName contents)
                                                    (alist-get 'namespaceSize contents)))))
                  ("TermObject" (setq output
                                      (concat output (format "  %s (%s)(%s)\n"
                                                             (alist-get 'termName contents)
                                                             (alist-get 'segment
                                                                        (car
                                                                         (alist-get 'termType contents)))
                                                             (extract-segments (alist-get 'termType contents))
                                                             ))))
                  ("TypeObject"    (setq output
                                         (concat output
                                                 (format "TypeObject:%s\n"
                                                         (extract-segments (alist-get 'contents contents))))))
                  ("Subnamespace"  (setq output (concat output (format "Subnamespace\n" ))))
                  ("HashQualifier" (setq output (concat output (format "HashQualifier\n" ))))
                  ("DelimiterChar" (setq output (concat output (format "DelimiterChar\n" ))))
                  (_               (setq output (concat output (format "unknown tag :[%s]\n" tag)))))))
            children)
      )
    (concat output "\n")))


;; tag:TypeObject
;; tag:Subnamespace
;; tag:TermObject
;; tag:TypeObject
;; tag:Subnamespace
;; tag:TermObject [14 times]
;; tag:Subnamespace [25 times]
;; tag:TermObject [6 times]
;; tag:Subnamespace
;; tag:TermObject [7 times]
;; tag:PatchObject
;; tag:TermObject [17 times]

(defun do-cd-command (&optional target)
  "Change namespace to the root, or to TARGET if given."
  (message "do-cd-command:target=%s" target)
  (format "do-cd-command:target=%s\n" target)
  )

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
  (ucm-start)
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
