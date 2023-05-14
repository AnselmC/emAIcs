;;; emAIcs --- Integrating Large Language Models into emacs for superpowered productivity -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'request)

;;; Custom variables
(defcustom emaics-api-key "" "Set the API key for the emAIcs backend, i.e. OpenAI.")

;; Constant variables
(defconst emaics--client-buffer-name "*emAIcs*")
(defconst emaics--server-buffer-name "*emAIcs server*")
(defconst emaics--pkg-directory (file-name-directory (or load-file-name buffer-file-name)))

;; Global variables
(defvar emaics--request-id 0)

(defvar emaics--client-buffer nil)
(defvar emaics--prompt-history '())

(defvar emaics--server nil)
(defvar emaics--server-buffer nil)

(defvar emaics--major-mode-to-language-string
  (let ((hash-table (make-hash-table :test 'equal)))
    (puthash 'python-mode "python" hash-table)
    (puthash 'c-mode "c" hash-table)
    (puthash 'java-mode "java" hash-table)
    (puthash 'ruby-mode "ruby" hash-table)
    (puthash 'rust-mode "rust" hash-table)
    (puthash 'fortran-mode "fortran" hash-table)
    (puthash 'emacs-lisp-mode "elisp" hash-table)
    (puthash 'clojure-mode "clojure" hash-table)
    hash-table))

(defvar emaics--default-prompts
  '("Implement this function"
    "Write a docstring for this function"
    "Write a test for this function"
    "Improve this function"))

(defun emaics--handle-error-from-successfull-call (error-data response-request-id)
  "Handle request error from 200 response for RESPONSE-REQUEST-ID from ERROR-DATA."
  (let ((err-code (assoc-default 'code error-data))
        (err-message (assoc-default 'message error-data)))
    (message "Request failed: %S (%S)" err-message err-code)))

(defun emaics--wrap-inside-org-src-block (src-code lang)
  (format "#+BEGIN_SRC %s \n%s \n#+END_SRC" lang src-code))


(defun emaics--send-result-to-buffer (result response-request-id lang)
  "Display RESULT for RESPONSE-REQUEST-ID from emaics server in dedicated buffer."
  (let ((buffer (get-buffer-create emaics--client-buffer-name))
        (formatted-result (emaics--wrap-inside-org-src-block result lang)))
    (with-current-buffer buffer
      (message "hello")
      (switch-to-buffer-other-window buffer)
      (org-mode)
      (setq emaics--client-buffer buffer)
      (goto-char (point-max))
      (insert (format "\n\n* Result for request id %s:\n%s" response-request-id formatted-result)))))


(defun emaics--handle-success (response lang)
  "Callback for handling RESPONSE from 200 call from `emaics--send-request-to-server'."
  (let* ((response-data (request-response-data response))
         (response-request-id (assoc-default 'id response-data))
         (err (assoc-default 'error response-data))
         (result (assoc-default 'result response-data)))
    (if err
        (emaics--handle-error-from-successfull-call err response-request-id))
    (emaics--send-result-to-buffer result response-request-id lang)))


(defun emaics--increment-request-id ()
  "Increments local variable REQUEST-ID."
  (setq emaics--request-id (1+ emaics--request-id)))

(defun emaics--send-request-to-server (method-name params lang)
  "Send request to LLM server for METHOD-NAME with PARAMS."
  (let* ((server-url "http://localhost")
         (server-port "4000")
         (data (json-encode `(("method" . ,method-name)
                              ("id" . ,emaics--request-id)
                              ("jsonrpc" . "2.0")
                              ("params" . ,params))))
         (request-url (concat server-url ":" server-port)))
    (progn
      (emaics--increment-request-id)
      (request request-url
        :parser 'json-read
        :headers '(("Content-Type" . "application/json"))
        :data data
        :success (cl-function
                  (lambda (&key response &allow-other-keys)
                    (emaics--handle-success response lang)))))))





(defun emaics--ask-user-for-prompt ()
  "Interactively ask for a prompt.

  Select from `emaics--prompt-history' to easily choose previous prompts."
  (interactive)
  (let ((prompt (completing-read
                 "Select prompt for active region: "
                 (append emaics--prompt-history emaics--default-prompts))))
    (add-to-list 'emaics--prompt-history prompt)
    prompt))




;;;###autoload
(defun emaics-start-server ()
  "Start the emAIcs server if it hasn't started yet."
  (interactive)
  (if (not (get-buffer-process emaics--server-buffer))
      (progn
        (message "Starting emAIcs server...")
        (setq emaics--server-buffer (get-buffer-create emaics--server-buffer-name))
        (with-current-buffer emaics--server-buffer
          (erase-buffer)
          (setq emaics--server (make-process
                                :name "emaics-server"
                                :buffer emaics--server-buffer
                                :connection-type 'pipe
                                :command `("python"
                                           ,(expand-file-name "server.py" emaics--pkg-directory)
                                           "--api-key"
                                           ,emaics-api-key)))))
    (message "Server already running!")))


;;;###autoload
(defun emaics-stop-server ()
  "Stop the emAIcs server if it's running."
  (interactive)
  (if emaics--server
      (progn
        (message "Stopping emAIcs server...")
        (stop-process emaics--server)
        (setq emaics--server nil)
        (kill-buffer emaics--server-buffer))
    (message "emAICs server isn't running. Start with `M-x emaics-start-server'")))

;;;###autoload
(defun emaics-restart-server()
  "(Re)start the emAIcs server."
  (interactive)
  (emaics-stop-server)
  (emaics-start-server))



;;;###autoload
(defun emaics-add-lang-string-for-major-mode (mmode lang)
  (puthash mmode lang emaics--major-mode-to-language-string))

(defun emaics--get-language-string-for-major-mode ()
  (let ((lang (gethash major-mode emaics--major-mode-to-language-string)))
    (if lang
        lang
      (progn
        (message (format "No language string for %s. Set with `emaics-add-lang-string-for-major-mode'" major-mode))
        ""))))



;;;###autoload
(defun emaics-execute-prompt-on-region ()
  "Send prompt with active region to emaics server."
  (interactive)
  (if (use-region-p)
      (let* ((bounds (cons (region-beginning) (region-end)))
             (buffer (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (lang (emaics--get-language-string-for-major-mode))
             (prompt (emaics--ask-user-for-prompt)))
        (emaics--send-request-to-server "execute_prompt" `(("prompt" . ,prompt) ("buffer" . ,buffer)) lang))
    (message "Select a region before calling `emaics-execute-prompt-on-region'")))

(provide 'emaics)
;;; emaics.el ends here
