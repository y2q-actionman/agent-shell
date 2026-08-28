;;; agent-shell-context-sources.el --- Pluggable context sources for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2026 y2q-actionman

;; Author: y2q-actionman
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; A change queue that collects eval/file-change events from pluggable
;; sources and injects them as context into agent-shell prompts.
;;
;; Usage:
;;
;;   (require 'agent-shell-context-sources)
;;   (agent-shell-context-sources-enable)
;;   (agent-shell-context-sources-setup-self-write-tracking shell-buffer)

;;; Code:

(require 'map)
(require 'seq)

(declare-function agent-shell-subscribe-to "agent-shell")

;;; Change queue

(defvar agent-shell-context-sources--change-queue nil
  "Pending change notices.  Each element is an alist with :source, :content, :timestamp.")

(defun agent-shell-context-sources--queue-push (source content)
  "Add SOURCE (symbol) and CONTENT (string) to the change queue."
  (push `((:source . ,source)
          (:content . ,content)
          (:timestamp . ,(current-time)))
        agent-shell-context-sources--change-queue))

(defun agent-shell-context-sources--queue-drain ()
  "Return all queued changes and clear the queue."
  (prog1 (nreverse agent-shell-context-sources--change-queue)
    (setq agent-shell-context-sources--change-queue nil)))

;;; Context source function

(defun agent-shell-context-sources--from-queue ()
  "Return queued changes as a context string, or nil if queue is empty."
  (when-let* ((changes (agent-shell-context-sources--queue-drain)))
    (mapconcat
     (lambda (change)
       (format "[%s]\n%s"
               (map-elt change :source)
               (map-elt change :content)))
     changes
     "\n\n")))

(defun agent-shell-context-sources-enable ()
  "Register the change queue as the highest-priority agent-shell context source.

Pushed to the front of `agent-shell-context-sources' so that when the
queue has content it becomes the context for the next prompt; when empty,
the built-in sources (files, region, error, line) take over via `seq-some'."
  (push #'agent-shell-context-sources--from-queue
        agent-shell-context-sources))

;;; Self-write exclusion (agent-shell's own file writes)

(defvar agent-shell-context-sources--pending-self-writes
  (make-hash-table :test 'equal)
  "Files currently being written by agent-shell, excluded from file-watcher.")

(defun agent-shell-context-sources-setup-self-write-tracking (shell-buffer)
  "Track agent-shell file-write events in SHELL-BUFFER to suppress false positives.

Call once after the shell buffer is ready."
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'file-write
   :on-event (lambda (event)
               (when-let* ((data (map-elt event :data))
                           (path (map-elt data :path)))
                 (puthash path t agent-shell-context-sources--pending-self-writes)
                 (run-with-timer 0.5 nil
                                 (lambda ()
                                   (remhash path agent-shell-context-sources--pending-self-writes)))))))

;;; file-watcher source (auto-revert-based)

(defun agent-shell-context-sources--on-file-revert ()
  "Push an external file change notice when a buffer is reverted.

Skips files currently being written by agent-shell."
  (when-let* ((path (buffer-file-name))
              ((not (gethash path agent-shell-context-sources--pending-self-writes))))
    (agent-shell-context-sources--queue-push
     'file-watcher
     (format "%s\n%s" path (buffer-string)))))

(defun agent-shell-context-sources-enable-file-watcher ()
  "Enable detection of externally-reverted buffers as context changes."
  (add-hook 'after-revert-hook #'agent-shell-context-sources--on-file-revert))

(defun agent-shell-context-sources-disable-file-watcher ()
  "Disable the file-watcher source."
  (remove-hook 'after-revert-hook #'agent-shell-context-sources--on-file-revert))

;;; mtime-based detection (buffers not currently open)

(defvar agent-shell-context-sources--file-read-log
  (make-hash-table :test 'equal)
  "mtime recorded when agent last read each file.
key: file path string, value: mtime from `file-attribute-modification-time'.")

(defun agent-shell-context-sources-record-file-read (path)
  "Record the current mtime of PATH as the agent's last-read time."
  (when-let* ((attrs (file-attributes path)))
    (puthash path (file-attribute-modification-time attrs)
             agent-shell-context-sources--file-read-log)))

(defun agent-shell-context-sources-file-changed-since-read-p (path)
  "Return t if PATH has been modified since the agent last read it."
  (when-let* ((recorded (gethash path agent-shell-context-sources--file-read-log))
              (attrs (file-attributes path))
              (current (file-attribute-modification-time attrs)))
    (time-less-p recorded current)))

;;; elisp-eval source

(defun agent-shell-context-sources--after-elisp-eval (&rest _)
  "Push the evaluated sexp to the change queue after an Emacs Lisp eval command."
  (when-let* ((form (thing-at-point 'sexp t)))
    (agent-shell-context-sources--queue-push 'elisp-eval form)))

(defun agent-shell-context-sources-enable-elisp-eval ()
  "Advise Emacs Lisp eval commands to enqueue the evaluated form."
  (dolist (cmd '(eval-defun eval-last-sexp eval-region eval-buffer))
    (advice-add cmd :after #'agent-shell-context-sources--after-elisp-eval)))

(defun agent-shell-context-sources-disable-elisp-eval ()
  "Remove the elisp-eval advice."
  (dolist (cmd '(eval-defun eval-last-sexp eval-region eval-buffer))
    (advice-remove cmd #'agent-shell-context-sources--after-elisp-eval)))

(provide 'agent-shell-context-sources)
;;; agent-shell-context-sources.el ends here
