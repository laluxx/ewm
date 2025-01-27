;;; ewm.el --- Window management utilities for Emacs

;;; Commentary:

;; TODO foxus the correct window when exiting from monocle
;; TODO M-l in dired on a directory should open that directory in the other window

;; We all know about EXWM, where Emacs becomes an X window manager. 
;; EWM does not try to do the same thing. Instead, EWM is a window manager 
;; inside Emacs, not Emacs becoming a window manager.


;;             1 2 3 4 5 6 7 8 9 (workspaces)
;;                        ^ UP
;;                  (split/previous)
;;              (quit) q  k  TAB (last-workspace)
;;                      \ ^ /
;;   < LEFT (s/res) h  <- M -> l  (split/resize) > RIGHT
;;                        v \
;;                        j  SPC (Monocle)
;;                  (split/next)
;;                        v DOWN

;; Almost every keybind behaves differently if there is one window in the current workspace or more than one.

;;; Code:

(defvar ewm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'ewm-down)
    (define-key map (kbd "M-k") 'ewm-up)
    (define-key map (kbd "M-h") 'ewm-left)
    (define-key map (kbd "M-l") 'ewm-right)
    (define-key map (kbd "M-J") 'ewm-swap-with-next-window)
    (define-key map (kbd "M-K") 'ewm-swap-with-prev-window)
    (define-key map (kbd "M-q") 'ewm-delete-window)
    ;; (define-key map (kbd "M-SPC") 'ewm-monocle)
    map)
  "Keymap for `ewm-mode`.")

(defvar ewm-skip-windows t
  "If non-nil, use smart navigation behavior skipping windows.")

(defvar ewm-override-map (make-sparse-keymap)
  "Keymap for overriding major mode keybindings.")

(define-key ewm-override-map (kbd "M-j") 'ewm-down)
(define-key ewm-override-map (kbd "M-k") 'ewm-up)
(define-key ewm-override-map (kbd "M-h") 'ewm-left)
(define-key ewm-override-map (kbd "M-l") 'ewm-right)
(define-key ewm-override-map (kbd "M-J") (lambda () (interactive) (ewm-rotate-stack 'next)))
(define-key ewm-override-map (kbd "M-K") (lambda () (interactive) (ewm-rotate-stack 'prev)))
(define-key ewm-override-map (kbd "M-q") 'ewm-delete-window)
(define-key ewm-override-map (kbd "M-SPC") 'ewm-monocle)

(defun ewm-activate-override-map ()
  "Activate the override keymap for EWM."
  (add-to-list 'emulation-mode-map-alists `((ewm-mode . ,ewm-override-map))))

(add-hook 'ewm-mode-hook 'ewm-activate-override-map)


(defun ewm-movable-window-list ()
  "Return a list of movable windows, excluding fixed-width side windows."
  (seq-filter (lambda (window)
                (not (or (window-parameter window 'window-side)
                         (window-parameter window 'window-fixed-size))))
              (window-list)))


(defun ewm-down ()
  "Cycle through buffers or split the window down depending on the mode.
   If ewm-skip-windows is non-nil, use advanced behavior."
  (interactive)
  (if ewm-monocle-state
      (ewm-monocle-cycle-buffer 'next)
    (if ewm-skip-windows
        (let ((movable-windows (ewm-movable-window-list))
              (all-windows (window-list)))
          (cond
           ;; Only one movable window
           ((= (length movable-windows) 1)
            (if (= (length all-windows) 1)
                (progn  ; truly only one window
                  (split-window-below)
                  (windmove-down)
                  (ewm-open-header))
              (progn  ; one movable and one fixed (e.g., Treemacs)
                (other-window 1))))
           ;; Multiple movable windows
           (t
            (let ((next-window (next-window)))
              (while (and (not (eq next-window (selected-window)))
                          (not (member next-window movable-windows)))
                (setq next-window (next-window next-window)))
              (select-window next-window)))))
      (if (= (length (window-list)) 1)
          (progn
            (split-window-below)
            (windmove-down)
            (ewm-open-header))
        (other-window 1)))))

(defun ewm-up ()
  "Cycle through buffers or split the window up depending on the mode.
   If ewm-skip-windows is non-nil, use advanced behavior."
  (interactive)
  (if ewm-monocle-state
      (ewm-monocle-cycle-buffer 'prev)
    (if ewm-skip-windows
        (let ((movable-windows (ewm-movable-window-list))
              (all-windows (window-list)))
          (cond
           ;; Only one movable window
           ((= (length movable-windows) 1)
            (if (= (length all-windows) 1)
                (progn  ; truly only one window
                  (split-window-below)
                  (ewm-open-header))
              (progn  ; one movable and one fixed (e.g., Treemacs)
                (other-window -1))))
           ;; Multiple movable windows
           (t
            (let ((prev-window (previous-window)))
              (while (and (not (eq prev-window (selected-window)))
                          (not (member prev-window movable-windows)))
                (setq prev-window (previous-window prev-window)))
              (select-window prev-window)))))
      (if (= (length (window-list)) 1)
          (progn
            (split-window-below)
            (ewm-open-header))
        (other-window -1)))))

(defun ewm-left ()
  "If there is only one movable window, split vertically and focus left.
   If ewm-skip-windows is non-nil, use advanced behavior."
  (interactive)
  (if ewm-skip-windows
      (let ((movable-windows (ewm-movable-window-list)))
        (if (= (length movable-windows) 1)
            (progn
              (split-window-right)
              (ewm-open-header))
          (let ((current-window (selected-window)))
            (if (or (not (member current-window movable-windows))
                    (= (window-pixel-left current-window)
                       (window-pixel-left (car (remove current-window movable-windows)))))
                ;; Vertical arrangement or non-movable window - resize height
                (if (window-at-side-p current-window 'bottom)
                    (enlarge-window 5)
                  (shrink-window 5))
              ;; Horizontal arrangement - resize width
              (if (window-at-side-p current-window 'right)
                  (enlarge-window-horizontally 5)
                (shrink-window-horizontally 5))))))
    (if (= (length (window-list)) 1)
        (progn
          (split-window-right)
          (ewm-open-header))
      (if (= (window-pixel-left (selected-window))
             (window-pixel-left (next-window)))
          ;; Vertical arrangement - resize height
          (if (window-at-side-p (selected-window) 'bottom)
              (enlarge-window 5)
            (shrink-window 5))
        ;; Horizontal arrangement - resize width
        (if (window-at-side-p (selected-window) 'right)
            (enlarge-window-horizontally 5)
          (shrink-window-horizontally 5))))))

(defun ewm-right ()
  "If there is only one movable window, split vertically and focus right.
   If ewm-skip-windows is non-nil, use advanced behavior."
  (interactive)
  (if ewm-skip-windows
      (let ((movable-windows (ewm-movable-window-list)))
        (if (= (length movable-windows) 1)
            (progn
              (split-window-right)
              (windmove-right)
              (ewm-open-header))
          (let ((current-window (selected-window)))
            (if (or (not (member current-window movable-windows))
                    (= (window-pixel-left current-window)
                       (window-pixel-left (car (remove current-window movable-windows)))))
                ;; Vertical arrangement or non-movable window - resize height
                (if (window-at-side-p current-window 'bottom)
                    (shrink-window 5)
                  (enlarge-window 5))
              ;; Horizontal arrangement - resize width
              (if (window-at-side-p current-window 'right)
                  (shrink-window-horizontally 5)
                (enlarge-window-horizontally 5))))))
    (if (= (length (window-list)) 1)
        (progn
          (split-window-right)
          (windmove-right)
          (ewm-open-header))
      (if (= (window-pixel-left (selected-window))
             (window-pixel-left (next-window)))
          ;; Vertical arrangement - resize height
          (if (window-at-side-p (selected-window) 'bottom)
              (shrink-window 5)
            (enlarge-window 5))
        ;; Horizontal arrangement - resize width
        (if (window-at-side-p (selected-window) 'right)
            (shrink-window-horizontally 5)
          (enlarge-window-horizontally 5))))))

(defun ewm-toggle-advanced-navigation ()
  "Toggle between advanced and basic navigation in ewm."
  (interactive)
  (setq ewm-skip-windows (not ewm-skip-windows))
  (message "EWM advanced navigation %s"
           (if ewm-skip-windows "enabled" "disabled")))


(defun ewm-delete-window ()
  "Delete the current window, or kill the buffer if it's the only window.
Also kills the buffer if it's named '*haskell*' even if there are multiple windows."
  (interactive)
  (if (or (string= (buffer-name) "*haskell*")
          (= (length (window-list)) 1))
      (kill-buffer (current-buffer))
    (delete-window)))

(defun ewm-rotate-stack (direction)
  "Swap the current window with the window in DIRECTION ('next or 'prev).
When there's only one window, split and open scratch-buffer for 'prev
or eshell for 'next."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (split-window-below)
        (other-window 1)
        (if (eq direction 'prev)
            (scratch-buffer)
          (eshell)))
    (let* ((current (selected-window))
           (other-window (if (eq direction 'prev)
                             (previous-window)
                           (next-window)))
           (current-buf (window-buffer current))
           (other-buf (window-buffer other-window))
           (current-pos (window-start current))
           (other-pos (window-start other-window)))
      (unless (eq current other-window)
        (set-window-buffer current other-buf)
        (set-window-buffer other-window current-buf)
        (set-window-start current other-pos)
        (set-window-start other-window current-pos)
        (select-window other-window)))))


(defun ewm-open-header ()
  "If the current file is a C source file, check if a matching header file exists, 
   and if so, open it in the other window after splitting."
  (let* ((current-file (buffer-file-name))
         (file-base (and current-file
                         (string-match "\\(.+\\)\\.c\\'" current-file)
                         (match-string 1 current-file)))
         (header-file (and file-base (concat file-base ".h"))))
    (when (and header-file (file-exists-p header-file))
      (find-file header-file))))



;;;###autoload
(define-minor-mode ewm-mode
  "Minor mode for window management utilities.
Provides various functions for window splitting, swapping, and deleting."
  :lighter " EWM"
  :keymap ewm-mode-map
  :global nil)

;;;###autoload
(define-global-minor-mode global-ewm-mode
  ewm-mode
  (lambda () (ewm-mode 1))
  :group 'ewm
  :global t)

;;; EYEBROWSE CONFIGURATION
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-style nil)
  (setq eyebrowse-tagged-slot-format "%t")
  (eyebrowse-mode t)

  (define-key ewm-override-map (kbd "M-1") (lambda () (interactive) (eyebrowse-switch-to-window-config 1)))
  (define-key ewm-override-map (kbd "M-2") (lambda () (interactive) (eyebrowse-switch-to-window-config 2)))
  (define-key ewm-override-map (kbd "M-3") (lambda () (interactive) (eyebrowse-switch-to-window-config 3)))
  (define-key ewm-override-map (kbd "M-4") (lambda () (interactive) (eyebrowse-switch-to-window-config 4)))
  (define-key ewm-override-map (kbd "M-5") (lambda () (interactive) (eyebrowse-switch-to-window-config 5)))
  (define-key ewm-override-map (kbd "M-6") (lambda () (interactive) (eyebrowse-switch-to-window-config 6)))
  (define-key ewm-override-map (kbd "M-7") (lambda () (interactive) (eyebrowse-switch-to-window-config 7)))
  (define-key ewm-override-map (kbd "M-8") (lambda () (interactive) (eyebrowse-switch-to-window-config 8)))
  (define-key ewm-override-map (kbd "M-9") (lambda () (interactive) (eyebrowse-switch-to-window-config 9)))
  (define-key ewm-override-map (kbd "M-TAB") 'eyebrowse-last-window-config))

;;; MONOCLE

(defvar ewm-monocle-state nil
  "Current window configuration.
Intended for use by `ewm-monocle'.")

(defvar ewm-monocle-buffers nil
  "List of buffers visible before entering monocle mode.")

(defun ewm-monocle ()
  "Toggle between multiple windows and single window.
   This is the equivalent of maximizing a window. Tiling window managers such as DWM, BSPWM refer to this state as 'monocle'."
  (interactive)
  (if (one-window-p)
      (when ewm-monocle-state
        (set-window-configuration ewm-monocle-state)
        (setq ewm-monocle-state nil)
        (setq ewm-monocle-buffers nil))
    (setq ewm-monocle-state (current-window-configuration))
    (setq ewm-monocle-buffers (mapcar 'window-buffer (window-list)))
    (delete-other-windows)))


(defun ewm-monocle-cycle-buffer (direction)
  "Cycle through buffers in MONOCLE mode based on DIRECTION.
DIRECTION should be 'next or 'prev."
  (let* ((current-buffer (window-buffer (selected-window)))
         (buffers ewm-monocle-buffers)
         (current-index (cl-position current-buffer buffers :test 'eq))
         (count (length buffers))
         (new-index (pcase direction
                      ('next (mod (1+ current-index) count))
                      ('prev (mod (1- current-index) count))))
         (new-buffer (nth new-index buffers)))
    (set-window-buffer (selected-window) new-buffer)))


(provide 'ewm)

;;; ewm.el ends here
