;;  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Gabriel Ebner
;;

(require 'cl-lib)
(require 'json)
(require 'lean-debug)
(require 'lean-leanpkg)
(require 'dash)

(defcustom lean-server-show-message-hook '(lean-message-boxes-display)
  "Hook run on messages from Lean, allowing custom display.

Each hook is a function that receives a list of message objects
for the current buffer.  Each message object is a plist with at
least the following keys:
 - :pos_line  is the line number of the message, a number
 - :pos_col is the column of the start of the message, a number
 - :caption is a category of message, a string
 - :text is the text to display, a string."
  :group 'lean
  :type 'hook
  :options '(lean-message-boxes-display))

(cl-defstruct lean-server-session
  path-file        ; the leanpkg.path file of this lean server
  process          ; process object of lean --server
  seq-num          ; sequence number
  callbacks        ; alist of (seq_num . (success_cb . error_cb))
  current-roi      ; alist of (file_name (begin_line . end_line) ...)
  tasks            ; last deserialized current_tasks message
  messages)        ; list of messages in deserialized json

(defun lean-server-session-proc-buffer (sess)
  (process-buffer (lean-server-session-process sess)))

(defun lean-server-session-pop-callback (sess seq-num)
  (let ((cbp (assoc seq-num (lean-server-session-callbacks sess))))
    (setf (lean-server-session-callbacks sess)
          (delete cbp (lean-server-session-callbacks sess)))
    (if cbp (cdr cbp) (cons nil nil))))

(defun lean-server-process-response (sess res)
  (pcase (plist-get res :response)
    ("additional_message"
     (setf (lean-server-session-messages sess)
           (cons (plist-get res :msg) (lean-server-session-messages sess)))
     (lean-server-notify-messages-changed sess))
    ("all_messages"
     (setf (lean-server-session-messages sess)
           (plist-get res :msgs))
     (lean-server-notify-messages-changed sess))
    ("current_tasks"
     (let ((old-tasks (lean-server-session-tasks sess)))
       (setf (lean-server-session-tasks sess) res)
       (lean-server-notify-tasks-changed sess old-tasks)))
    ("error"
     (message "error: %s" (plist-get res :message))
     ;; TODO(gabriel): maybe even add the error as a message
     (when (plist-get res :seq_num)
       (let ((cb (lean-server-session-pop-callback sess (plist-get res :seq_num))))
         (when (cdr cb) (funcall (cdr cb) res)))))
    ("ok"
     (let ((cb (lean-server-session-pop-callback sess (plist-get res :seq_num))))
       (when (car cb) (funcall (car cb) res))))))

(defun lean-server-process-line (sess line)
  (condition-case-unless-debug err
      (progn
        (lean-debug "server=> %s" line)
        (let* ((json-array-type 'list)
               (json-object-type 'plist)
               (json-false nil)
               (response (json-read-from-string line)))
          (lean-server-process-response sess response)))
    (error (message "error in lean-server command handler: %s\nServer response was:\n%s" err (buffer-string)))))

(defun lean-server-process-buffer (sess)
  (goto-char (point-min))
  (when (search-forward "\n" nil t)
    (let ((line (buffer-substring (point-min) (point))))
      (delete-region (point-min) (point))
      (lean-server-process-line sess line)
      (lean-server-process-buffer sess))))

(defun lean-server-filter (sess string)
  (when (buffer-live-p (lean-server-session-proc-buffer sess))
    (with-current-buffer (lean-server-session-proc-buffer sess)
      (goto-char (point-max))
      (insert string)
      (lean-server-process-buffer sess))))

(defun lean-server-handle-signal (_process event)
  "Handle signals for lean-server-process"
  (force-mode-line-update)
  (let ((event-string (s-trim event)))
    (lean-debug "lean-server-handle-signal: %s"
                (propertize event-string 'face '(:foreground "red")))
    (if (s-contains? "abnormally" event)
        (message (concat "Lean server died. See lean-server stderr buffer for details; "
                         "use lean-server-restart to restart it")))))

(defun lean-server-session-create (path-file)
  "Creates a new server session"
  (let* ((default-directory (f--traverse-upwards (f-dir? it) path-file))
         (exe (lean-get-executable lean-executable-name))
         (exe (if (assoc path-file lean-server-overrides)
                  (if (f-file? (lean-get-executable "elan"))
                      (list (lean-get-executable "elan") "run" "--install" (cdr (assoc path-file lean-server-overrides)) lean-executable-name)
                    (progn
                      (warn "Lean version override set but `elan` was not found; ignoring")
                      (list exe)))
                (list exe)))
         ; Setting process-connection-type is necessary, otherwise
         ; emacs truncates lines with >4096 bytes:
         ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24531
         (process-connection-type nil)
         (cmd `(,@exe
                "--server"
                ,(format "-M%i" lean-memory-limit)
                ,(format "-T%i" lean-timeout-limit)
                ,@lean-extra-arguments
                ,(format "*%s*" path-file)))
         (proc (if (and (fboundp 'make-process) (fboundp 'make-pipe-process))
                   (make-process ;; emacs >= 25 lets us redirect stderr
                    :name "lean-server"
                    :buffer (format " *lean-server (%s)*" path-file)
                    :command cmd
                    :coding 'utf-8
                    :noquery t
                    :sentinel #'lean-server-handle-signal
                    :stderr (make-pipe-process
                             :name "lean-server stderr"
                             :buffer (format "*lean-server stderr (%s)*" path-file)
                             :noquery t))
                 (progn
                   ; emacs 24 loves directory separators, without it
                   ; the server gets started in the parent directory....
                   (setq default-directory (format "%s/" default-directory))
                   (apply #'start-process "lean-server"
                          (format " *lean-server (%s)*" (buffer-name))
                          cmd))))
         (sess (make-lean-server-session
                :path-file path-file
                :process proc
                :seq-num 0
                :current-roi 'not-yet-sent
                :callbacks nil
                :messages nil)))
    (set-process-filter proc (lambda (_proc string) (lean-server-filter sess string)))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-query-on-exit-flag proc nil)
    sess))

(defun lean-server-session-send-command (sess cmd-name params &optional cb error-cb)
  (let* ((seq-num (lean-server-session-seq-num sess))
         (req `(:seq_num ,seq-num :command ,cmd-name . ,params))
         (json-array-type 'list)
         (json-object-type 'plist)
         (json-false :json-false)
         (json-req (json-encode req))
         (cur-buf (current-buffer))
         (wrapped-cb (and cb
                          (lambda (res)
                            (and cur-buf
                                 (with-current-buffer cur-buf
                                   (apply cb :allow-other-keys t res))))))
         (wrapped-err-cb (and error-cb
                              (lambda (res)
                                (and cur-buf
                                     (with-current-buffer cur-buf
                                       (apply error-cb :allow-other-keys t res)))))))
    (setf (lean-server-session-seq-num sess) (1+ seq-num))
    (if (or cb error-cb)
        (setf (lean-server-session-callbacks sess)
              (cons (cons seq-num (cons wrapped-cb wrapped-err-cb)) (lean-server-session-callbacks sess))))
    (lean-debug "server<= %s" json-req)
    (process-send-string (lean-server-session-process sess)
                         (concat json-req "\n"))))

(defvar lean-server-sessions nil
  "list of all running lean-server-sessions")

(defun lean-server-session-alive-p (sess)
  (and sess
       (lean-server-session-process sess)
       (equal 'run (process-status (lean-server-session-process sess)))))

(defun lean-server-session-kill (sess)
  (ignore-errors (delete-process (lean-server-session-process sess)))
  (ignore-errors (kill-buffer (lean-server-session-proc-buffer sess)))
  (setf (lean-server-session-process sess) nil)
  (setq lean-server-sessions (delete sess lean-server-sessions)))

(defun lean-server-session-get (path-file)
  (setq lean-server-sessions
        (cl-remove-if-not #'lean-server-session-alive-p
                          lean-server-sessions))
  (or (cl-find path-file lean-server-sessions
               :test (lambda (d s) (equal d (lean-server-session-path-file s))))
      (let ((sess (lean-server-session-create path-file)))
        (setq lean-server-sessions (cons sess lean-server-sessions))
        sess)))

(defvar-local lean-server-session nil
  "Lean server session for the current buffer")

(defvar lean-server-overrides nil
  "alist of (path file . toolchain name) pairs defined by `lean-server-switch-version'.")

(defun lean-server-session-running-p (sess)
  (and sess (plist-get (lean-server-session-tasks sess) :is_running)))

(defun lean-server-status-string ()
  (if (not (lean-server-session-alive-p lean-server-session)) " ☠"
    (if (lean-server-session-running-p lean-server-session) " ⌛"
      " ✓")))

(defvar-local lean-server-flycheck-delay-timer nil)

(defvar-local lean-server-task-overlays nil)

(defun lean-server-task-region (task)
  (let ((bl (1- (plist-get task :pos_line)))
        (bc (plist-get task :pos_col))
        (el (1- (plist-get task :end_pos_line)))
        (ec (plist-get task :end_pos_col)))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (forward-line bl)
      (if (equal (cons bl bc) (cons el ec))
          (progn
            (let ((beg (point)))
              (forward-line 1)
              (cons beg (point))))
        (ignore-errors (forward-char bc))
        (let ((beg (point)))
          (goto-char (point-min))
          (forward-line el)
          (ignore-errors (forward-char ec))
          (cons beg (point)))))))

(defface lean-server-task-face
  nil
  "Face to highlight pending Lean tasks."
  :group 'lean)

(if (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lean-server-fringe-bitmap
    (vector) 16 8))

(defface lean-server-task-fringe-face
  '((((class color) (background light))
     :background "chocolate1")
    (((class color) (background dark))
     :background "navajo white")
    (t :inverse-video t))
  "Face to highlight the fringe of pending Lean tasks."
  :group 'lean)

(defun lean-server-update-task-overlays ()
  (dolist (ov lean-server-task-overlays) (delete-overlay ov))
  (setq lean-server-task-overlays nil)
  (when (and lean-server-show-pending-tasks lean-server-session)
    (let* ((tasks (lean-server-session-tasks lean-server-session))
           (cur-fn (buffer-file-name))
           (roi (cdr (assq cur-fn (lean-server-session-current-roi lean-server-session)))))
      (dolist (task (plist-get tasks :tasks))
        (if (and (equal (plist-get task :file_name) cur-fn)
                 (--any? (<= (max (car it) (plist-get task :pos_line))
                             (min (cdr it) (plist-get task :end_pos_line)))
                         roi))
            (let* ((reg (lean-server-task-region task))
                   (ov (make-overlay (car reg) (cdr reg))))
              (setq lean-server-task-overlays (cons ov lean-server-task-overlays))
              (overlay-put ov 'face 'lean-server-task-face)
              (overlay-put ov 'line-prefix
                           (propertize " " 'display
                                       '(left-fringe lean-server-fringe-bitmap lean-server-task-fringe-face)))
              (overlay-put ov 'help-echo (format "%s..." (plist-get task :desc)))))))))

(defun lean-server-toggle-show-pending-tasks ()
  "Toggles highlighting of pending tasks"
  (interactive)
  (setq lean-server-show-pending-tasks (not lean-server-show-pending-tasks))
  (dolist (sess lean-server-sessions)
    (lean-server-notify-tasks-changed sess nil)))

(defvar-local lean-server-flycheck-delay-timer nil)
(defvar-local lean-server-flycheck-delayed-update nil)

(defun lean-server-show-messages (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (save-match-data
      (when (and (eq buf flycheck-error-list-source-buffer)
                 (get-buffer-window buf))
        (if (memq lean-server-flycheck-delay-timer timer-list)
            (setq lean-server-flycheck-delayed-update t) ; arm timer
          (flycheck-buffer)
          (setq lean-server-flycheck-delay-timer
                (run-at-time "100 milliseconds" nil
                             (lambda (buf)
                               (with-current-buffer buf
                                 (when lean-server-flycheck-delayed-update
                                   (setq lean-server-flycheck-delayed-update nil)
                                   (flycheck-buffer))))
                             (current-buffer))))))
    (when lean-server-session
      (let ((relevant-msgs
             (cl-remove-if-not (lambda (msg)
                                 (equal (buffer-file-name buf)
                                        (plist-get msg :file_name)))
                               (lean-server-session-messages lean-server-session))))
        (dolist (hook lean-server-show-message-hook)
          (funcall hook relevant-msgs))))))

(defvar-local lean-server-show-tasks-delay-timer nil)

(defun lean-server-show-tasks (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (save-match-data
      (when (not (memq lean-server-show-tasks-delay-timer timer-list))
        (setq lean-server-show-tasks-delay-timer
              (run-at-time "300 milliseconds" nil
                           (lambda (buf)
                             (with-current-buffer buf
                               (lean-server-update-task-overlays)))
                           (current-buffer)))))))

(defun lean-server-notify-messages-changed (sess)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and lean-server-session
                 (eq sess lean-server-session))
        (lean-server-show-messages)))))

(defun lean-server-notify-tasks-changed (sess old-tasks)
  (force-mode-line-update)
  (when (and (not lean-server-show-pending-tasks)
             (or (plist-get old-tasks :tasks)
                 (plist-get (lean-server-session-tasks sess) :tasks)))
    ; update task flycheck messages only if the task list is non-empty
    (lean-server-notify-messages-changed sess))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq sess lean-server-session)
        (lean-server-show-tasks)))))

(defun lean-server-stop ()
  "Stops the lean server associated with the current buffer"
  (interactive)
  (when lean-server-session
    (lean-server-session-kill lean-server-session)))

(defun lean-server-ensure-alive ()
  "Ensures that the current buffer has a lean server"
  (when (not (lean-server-session-alive-p lean-server-session))
    (setq lean-server-session (lean-server-session-get (lean-leanpkg-find-path-file)))
    (lean-server-show-tasks)
    (lean-server-show-messages)
    (lean-server-sync)))

(defun lean-server-restart ()
  "Restarts the lean server for the current buffer"
  (interactive)
  (lean-server-stop)
  (lean-server-ensure-alive)
  (flycheck-stop)
  (flycheck-buffer))

(defun lean-server-versions ()
  (unless (f-file? (lean-get-executable "elan"))
    (error "`bin/elan` was not found in the Lean root dir \"%s\"" (lean-get-rootdir)))
  (with-temp-buffer
    (call-process (lean-get-executable "elan") nil t nil "toolchain" "list")
    (let ((results (split-string (buffer-string) "\n" t)))
      ; strip " (default)" from versions
      (--map (car (split-string it " ")) results))))

(defun lean-server-switch-version ()
  "Restarts the lean server for the current buffer, using a specific version from elan prompted by `completing-read'."
  (interactive)
  (setq lean-server-overrides
        (cons
         (cons (lean-leanpkg-find-path-file)
               (completing-read "version: " (lean-server-versions) nil 'confirm))
         lean-server-overrides))
  (lean-server-restart))

(defun lean-server-send-command (cmd params &optional cb error-cb)
  "Sends a command to the lean server for the current buffer, with a callback to be called upon completion"
  (lean-server-ensure-alive)
  (lean-server-session-send-command lean-server-session cmd params cb error-cb))

(defvar lean-async-timeout 2
  "Maximum wait time for a value to be set during asynchronous call.")

(defvar lean-async-wait 0.03
  "Pause between checks to see if the value's been set when turning an
asynchronous call into synchronous.")

(defun lean-server-send-synchronous-command (cmd params)
  "Sends a command to the lean server for the current buffer, waiting for and returning the response"
  ;; inspired by company--force-sync
  (let ((res 'trash)
        (ok t)
        (start (time-to-seconds)))
    (lean-server-send-command cmd params
                              (lambda (&rest result) (setq res result))
                              (cl-function
                               (lambda (&key message)
                                 (setq ok nil)
                                 (setq res message))))
    (while (eq res 'trash)
      (if (> (- (time-to-seconds) start) lean-async-timeout)
          (error "Lean server timed out")
        (sleep-for lean-async-wait)))
    (if ok
        res
      (error res))))

(defun lean-server-sync (&optional buf)
  "Synchronizes the state of BUF (or the current buffer, if nil) with the lean server"
  (interactive)
  (with-demoted-errors "lean server sync: %s"
    (with-current-buffer (or buf (current-buffer))
      (lean-server-sync-roi)
      (lean-server-send-command
       'sync (list :file_name (buffer-file-name)
                   :content (buffer-string))))))

(defvar-local lean-server-sync-timer nil)

(defvar lean-server-sync-on-change t
  "When the value is t, sync the server when the buffer is changed.")

(defun lean-server-toggle-update-on-change ()
  "Toggle whether the server should be synced when the buffer is changed."
  (interactive)
  (setq lean-server-sync-on-change (not lean-server-sync-on-change))
  (when lean-server-sync-on-change
    (lean-server-sync)))

(defvar lean-server-change-hook-delay "50 milliseconds"
  "The amount of time to wait before syncing the lean server.

This should be a string giving a relative time like \"90\" or \"2 hours 35 minutes\"
(the acceptable forms are a number of seconds without units or
some combination of values using units in timer-duration-words).
")

(defun lean-server-change-hook (_begin _end _len)
  (when lean-server-sync-on-change
    (save-match-data
      (when lean-server-sync-timer (cancel-timer lean-server-sync-timer))
      (setq lean-server-sync-timer
            (run-at-time lean-server-change-hook-delay nil #'lean-server-sync (current-buffer))))))

(defun lean-server-compute-roi (sess)
  "Compute the region of interest for the session SESS."
  (--mapcat (with-current-buffer it
              (when (eq lean-server-session sess)
                (list (cons (buffer-file-name)
                            (--map (cons (line-number-at-pos (window-start it))
                                         (line-number-at-pos (window-end it t)))
                                   (get-buffer-window-list))))))
            (buffer-list)))

(defun lean-server-session-send-roi (sess roi)
  (setf (lean-server-session-current-roi sess) roi)
  (lean-server-send-command
   'roi (list :mode lean-server-check-mode
              :files (--map (list (cons :file_name (car it))
                                  (cons :ranges (--map (list (cons :begin_line (car it))
                                                             (cons :end_line (cdr it)))
                                                       (cdr it))))
                            roi))))

(defun lean-server-roi-subset-p (as bs)
  (--all? (let ((b (cdr (assq (car it) bs))))
            (and b (-all? (lambda (ar) (--any? (and (<= (car it) (car ar))
                                                    (<= (cdr ar) (cdr it)))
                                               b))
                          (cdr it))))
          as))

(defun lean-server-roi-extend (roi delta)
  (--map `(,(car it) .
           ,(--map `(,(max 1 (- (car it) delta)) . ,(+ (cdr it) delta)) (cdr it)))
         roi))

(defun lean-server-roi-ok (old-roi new-roi)
  (and (lean-server-roi-subset-p new-roi old-roi)
       (lean-server-roi-subset-p old-roi (lean-server-roi-extend new-roi 10))))

(defun lean-server-sync-roi (&optional force)
  (when lean-server-session
    (let ((old-roi (lean-server-session-current-roi lean-server-session))
          (new-roi (lean-server-compute-roi lean-server-session)))
      (when (or force (eq old-roi 'not-yet-sent) (not (lean-server-roi-ok old-roi new-roi)))
        (lean-server-session-send-roi lean-server-session
                                      (lean-server-roi-extend new-roi 5))))))

(defun lean-server-window-scroll-function-hook (wnd _new-start-pos)
  (let ((buf (window-buffer wnd)))
    (with-demoted-errors "lean scroll hook: %s"
      (with-current-buffer buf
        (lean-server-ensure-alive)
        (lean-server-sync-roi)))))

(defun lean-set-check-mode (mode)
  (setq lean-server-check-mode mode)
  (lean-server-sync-roi t))

(defun lean-check-nothing ()
  "Check nothing"
  (interactive)
  (lean-set-check-mode 'nothing))

(defun lean-check-visible-lines ()
  "Check visible lines"
  (interactive)
  (lean-set-check-mode 'visible-lines))

(defun lean-check-visible-lines-and-above ()
  "Check visible lines and above"
  (interactive)
  (lean-set-check-mode 'visible-lines-and-above))

(defun lean-check-visible-files ()
  "Check visible files"
  (interactive)
  (lean-set-check-mode 'visible-files))

(defun lean-check-open-files ()
  "Check visible files"
  (interactive)
  (lean-set-check-mode 'open-files))

(provide 'lean-server)
