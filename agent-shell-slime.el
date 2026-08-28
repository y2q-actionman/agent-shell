;;; agent-shell-slime.el --- SLIME/Common Lisp integration for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2026 y2q-actionman

;; Author: y2q-actionman
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Three-way sync between SLIME, CL image, and agent-shell (Claude Code):
;;
;;   Direction ①  C-c C-c → CL image (SLIME) + cl-mcp HTTP endpoint
;;   Direction ②  Claude edits file → CL image (auto compile-and-load)
;;   Direction ③  M-x agent-shell-slime-ask → send defun to agent-shell
;;
;; Requires cl-mcp v2.2.0+ running on HTTP transport:
;;   (cl-mcp:start-http-server :port 3000)
;;
;; Optionally requires agent-shell-context-sources for slime-eval source:
;;   (agent-shell-context-sources-enable-slime-eval)

;;; Code:

(require 'map)
(require 'url)

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function slime-compile-and-load-file "slime")
(declare-function slime-connected-p "slime")

;;; Direction ①: user edit (C-c C-c) → CL image + cl-mcp

(defvar agent-shell-slime-cl-mcp-port 3000
  "Port of the cl-mcp HTTP server started with (cl-mcp:start-http-server).")

(defun agent-shell-slime--send-to-cl-mcp (form)
  "POST FORM to the cl-mcp repl-eval tool asynchronously."
  (let* ((url (format "http://127.0.0.1:%d/mcp" agent-shell-slime-cl-mcp-port))
         (payload (json-encode
                   `((:jsonrpc . "2.0")
                     (:method . "tools/call")
                     (:id . 1)
                     (:params . ((:name . "repl-eval")
                                 (:arguments . ((:code . ,form)))))))))
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data (encode-coding-string payload 'utf-8)))
      (url-retrieve url #'ignore nil t))))

(defun agent-shell-slime--after-compile-defun (&rest _)
  "After slime-compile-defun, also send the form to cl-mcp."
  (when-let* ((form (thing-at-point 'defun t)))
    (agent-shell-slime--send-to-cl-mcp form)))

(defun agent-shell-slime-enable-cl-mcp-sync ()
  "Advise `slime-compile-defun' to mirror compiled forms to cl-mcp."
  (advice-add 'slime-compile-defun :after #'agent-shell-slime--after-compile-defun))

(defun agent-shell-slime-disable-cl-mcp-sync ()
  "Remove the cl-mcp sync advice."
  (advice-remove 'slime-compile-defun #'agent-shell-slime--after-compile-defun))

;;; Direction ②: Claude edits .lisp file → auto compile-and-load into CL image

(defun agent-shell-slime--auto-load-on-save ()
  "On save of a lisp-mode buffer with an active SLIME connection, compile and load it."
  (when-let* (((derived-mode-p 'lisp-mode))
              ((slime-connected-p))
              ((buffer-file-name)))
    (slime-compile-and-load-file)))

(defun agent-shell-slime-enable-auto-load ()
  "Enable automatic compile-and-load when a .lisp buffer is saved."
  (add-hook 'after-save-hook #'agent-shell-slime--auto-load-on-save))

(defun agent-shell-slime-disable-auto-load ()
  "Disable automatic compile-and-load on save."
  (remove-hook 'after-save-hook #'agent-shell-slime--auto-load-on-save))

;;; Direction ③: editor → agent-shell (ask Claude about defun at point)

(defvar agent-shell-slime-shell-buffer nil
  "Target agent-shell buffer for `agent-shell-slime-ask'.
When nil, falls back to copying the form to the kill ring.")

(defun agent-shell-slime-ask ()
  "Send the defun at point to agent-shell, or copy it to the kill ring.

Bind to C-c C-' in `lisp-mode-map'."
  (interactive)
  (when-let* ((form (thing-at-point 'defun t)))
    (if (and agent-shell-slime-shell-buffer
             (buffer-live-p agent-shell-slime-shell-buffer))
        (agent-shell--insert-to-shell-buffer
         :shell-buffer agent-shell-slime-shell-buffer
         :text (format "次の Common Lisp フォームを確認・改善してください:\n\n```lisp\n%s\n```"
                       form))
      (kill-new form)
      (message "agent-shell-slime: no shell buffer set, copied form to kill ring"))))

;;; slime-eval source for agent-shell-context-sources

(defun agent-shell-slime--after-slime-eval (&rest _)
  "Push the defun at point to the change queue after a SLIME eval command."
  (when-let* ((form (thing-at-point 'defun t)))
    (agent-shell-context-sources--queue-push 'slime-eval form)))

(defun agent-shell-slime-enable-context-source ()
  "Advise SLIME eval commands to enqueue the evaluated defun.

Requires `agent-shell-context-sources' to be loaded."
  (require 'agent-shell-context-sources)
  (dolist (cmd '(slime-compile-defun slime-eval-last-expression slime-eval-defun))
    (advice-add cmd :after #'agent-shell-slime--after-slime-eval)))

(defun agent-shell-slime-disable-context-source ()
  "Remove the SLIME eval advice."
  (dolist (cmd '(slime-compile-defun slime-eval-last-expression slime-eval-defun))
    (advice-remove cmd #'agent-shell-slime--after-slime-eval)))

;;; Optional: MCP tool registration via claude-code-ide.el

(defun agent-shell-slime-register-mcp-tools ()
  "Register slime_eval_form and slime_load_current_file as MCP tools.

Requires claude-code-ide.el."
  (require 'claude-code-ide)
  (claude-code-ide-make-tool
   "slime_eval_form"
   "Evaluate a Common Lisp form via SLIME"
   '(("form" "string" "CL form string to evaluate"))
   (lambda (args)
     (slime-eval `(swank:eval-and-grab-output ,(map-elt args "form")))))
  (claude-code-ide-make-tool
   "slime_load_current_file"
   "Compile and load the current .lisp buffer into the CL image"
   '()
   (lambda (_args)
     (slime-compile-and-load-file))))

(provide 'agent-shell-slime)
;;; agent-shell-slime.el ends here
