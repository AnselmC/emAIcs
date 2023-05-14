;;; package --- Summary

;;; Commentary:

;;; Code
(require 'request)


(defun emaics--handle-error-from-successfull-call (error-data response-request-id)
  "Handle request error from 200 response for RESPONSE-REQUEST-ID from ERROR-DATA."
  (let ((err-code (assoc-default 'code error-data))
        (err-message (assoc-default 'message error-data)))
    (message "Request failed: %S (%S)" err-message err-code)))

(defconst emaics--client-buffer-name "*emAIcs*")
(setq emaics--client-buffer nil)


(defun emaics--send-result-to-buffer (result response-request-id)
  "Display RESULT for RESPONSE-REQUEST-ID from emaics server in dedicated buffer."
  (let ((buffer (get-buffer-create emaics--client-buffer-name)))
    (switch-to-buffer-other-window buffer)
    (setq emaics--client-buffer buffer)
    (goto-char (point-max))
    (insert (format "\nResult for request id %s:\n%s" response-request-id result))))


(cl-defun emaics--handle-success (&key response &allow-other-keys)
  "Callback for handling RESPONSE from 200 call from `emaics--send-request-to-server'."
  (let* ((response-data (request-response-data response))
         (response-request-id (assoc-default 'id response-data))
         (err (assoc-default 'error response-data))
         (result (assoc-default 'result response-data)))
    (if err
        (emaics--handle-error-from-successfull-call err response-request-id))
    (emaics--send-result-to-buffer result response-request-id)))

(setq emaics--request-id 0)

(defun emaics--increment-request-id ()
  "Increments local variable REQUEST-ID."
  (setq emaics--request-id (1+ emaics--request-id)))

(defun emaics--send-request-to-server (method-name params)
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
        :success 'emaics--handle-success))))


(setq emaics--prompt-history '())

(defun emaics--ask-user-for-prompt ()
  "Interactively ask for a prompt.

  Select from `emaics--prompt-history' to easily choose previous prompts."
  (interactive)
  (let ((prompt (completing-read
                 "Select prompt for active region: "
                 emaics--prompt-history)))
    (add-to-list 'emaics--prompt-history prompt)
    prompt))


(setq emaics--server nil)
(defconst emaics--server-buffer-name "*emAIcs server*")
(setq emaics--server-buffer nil)

(defconst emaics--pkg-directory (file-name-directory (or load-file-name buffer-file-name)))


;;;###autoload
(defun emaics-start-server ()
  "Start the emAIcs server if it hasn't started yet."
  (interactive)
  (if (not emaics--server)
      (progn
        (message "Starting emAIcs server...")
                 (setq emaics--server-buffer (get-buffer-create emaics--server-buffer-name))
                 (with-current-buffer emaics--server-buffer
                   (erase-buffer)
                   (setq emaics--server (make-process
                                         :name "emaics-server"
                                         :buffer emaics--server-buffer
                                         :connection-type 'pipe
                                         :command `("python" ,(expand-file-name "server.py" emaics--pkg-directory))))
                   ))
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
(defun emaics-execute-prompt-on-region ()
  "Send prompt with active region to emaics server."
  (interactive)
  (if (use-region-p)
      (let* ((bounds (cons (region-beginning) (region-end)))
             (buffer (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (prompt (emaics--ask-user-for-prompt)))
        (emaics--send-request-to-server "execute_prompt" `(("prompt" . ,prompt) ("buffer" . ,buffer))))
    (message "Select a region before calling `emaics-execute-prompt-on-region'")))

(provide 'emaics)
;;; emaics.el ends here
