;;; ewm.el --- Window management utilities for Emacs

;;; Commentary:

;; TODO add window alist configuration
;; TODO curstom behaviour on M-h and M-l
;; if there are 2 windows one at the top, and one at the bottom
;; TODO fix M-q in monocle
;; TODO foxus the correct window when exiting from monocle

;; We all know about EXWM, where Emacs becomes an X window manager. 
;; EWM does not try to do the same thing. Instead, EWM is a window manager 
;; inside Emacs, not Emacs becoming a window manager.


;;      1 2 3 4 5 6 7 8 9 (workspaces)
;;    
;;         (split/previous)
;;     (quit) q  k  TAB (last-workspace)
;;             \ ^ /
;; (s/res) h  <- M -> l  (split/resize)
;;               v \
;;               j  SPC (Monocle)
;;         (split/next)

;; Almost every keybind behaves differently depending on whether there is 
;; only one window in the current workspace or more than one.

;;; Code:

(defvar ewm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'ewm-smart-split-down)
    (define-key map (kbd "M-k") 'ewm-smart-split-up)
    (define-key map (kbd "M-h") 'ewm-smart-split-left)
    (define-key map (kbd "M-l") 'ewm-smart-split-right)
    (define-key map (kbd "M-J") 'ewm-swap-with-next-window)
    (define-key map (kbd "M-K") 'ewm-swap-with-prev-window)
    (define-key map (kbd "M-q") 'ewm-smart-delete-window)
    (define-key map (kbd "M-SPC") 'ewm-monocle)
    map)
  "Keymap for `ewm-mode`.")

(defvar ewm-override-map (make-sparse-keymap)
  "Keymap for overriding major mode keybindings.")

(define-key ewm-override-map (kbd "M-j") 'ewm-smart-split-down)
(define-key ewm-override-map (kbd "M-k") 'ewm-smart-split-up)
(define-key ewm-override-map (kbd "M-h") 'ewm-smart-split-left)
(define-key ewm-override-map (kbd "M-l") 'ewm-smart-split-right)
(define-key ewm-override-map (kbd "M-J") 'ewm-swap-with-next-window)
(define-key ewm-override-map (kbd "M-K") 'ewm-swap-with-prev-window)
(define-key ewm-override-map (kbd "M-q") 'ewm-smart-delete-window)
(define-key ewm-override-map (kbd "M-SPC") 'ewm-monocle)

(defun ewm-activate-override-map ()
  "Activate the override keymap for EWM."
  (add-to-list 'emulation-mode-map-alists `((ewm-mode . ,ewm-override-map))))

(add-hook 'ewm-mode-hook 'ewm-activate-override-map)

(defun ewm-swap-with-prev-window ()
  "Swap the current window with the previous one,
   open 'scratch-buffer' if only one window."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (split-window-below)
        (other-window 1)
        (scratch-buffer))
    (let* ((current (selected-window))
           (prev (previous-window))
           (current-buf (window-buffer current))
           (prev-buf (window-buffer prev))
           (current-pos (window-start current))
           (prev-pos (window-start prev)))
      (unless (eq current prev)
        (set-window-buffer current prev-buf)
        (set-window-buffer prev current-buf)
        (set-window-start current prev-pos)
        (set-window-start prev current-pos)
        (select-window prev)))))

(defun ewm-swap-with-next-window ()
  "Swap the current window with the next one,
   open 'eshell' if only one window."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (split-window-below)
        (other-window 1)
        (eshell))
    (let* ((current (selected-window))
           (next (next-window))
           (current-buf (window-buffer current))
           (next-buf (window-buffer next))
           (current-pos (window-start current))
           (next-pos (window-start next)))
      (unless (eq current next)
        (set-window-buffer current next-buf)
        (set-window-buffer next current-buf)
        (set-window-start current next-pos)
        (set-window-start next current-pos)
        (select-window next)))))



(defun ewm-smart-split-open-header ()
  "If the current file is a C source file, check if a matching header file exists, 
   and if so, open it in the other window after splitting."
  (let* ((current-file (buffer-file-name))
         (file-base (and current-file
                         (string-match "\\(.+\\)\\.c\\'" current-file)
                         (match-string 1 current-file)))
         (header-file (and file-base (concat file-base ".h"))))
    (when (and header-file (file-exists-p header-file))
      (find-file header-file))))

(defun ewm-smart-split-down ()
  "Cycle through buffers or split the window down depending on the mode.
   If the current buffer is a C file, check if a matching header file exists and open it."
  (interactive)
  (if ewm-monocle-state
      (ewm-monocle-cycle-buffer 'next)
    (if (= (length (window-list)) 1)
        (progn
          (split-window-below)
          (windmove-down)
          (ewm-smart-split-open-header))
      (progn
        (other-window 1)))))

(defun ewm-smart-split-up ()
  "Cycle through buffers or split the window up depending on the mode.
   If the current buffer is a C file, check if a matching header file exists and open it."
  (interactive)
  (if ewm-monocle-state
      (ewm-monocle-cycle-buffer 'prev)
    (if (= (length (window-list)) 1)
        (progn
          (split-window-below)
          (ewm-smart-split-open-header))
      (progn
        (other-window -1)))))

(defun ewm-smart-split-left ()
  "If there is only one window, split vertically and focus left.
   If the current buffer is a C file, check if a matching header file exists and open it.
   Otherwise, shrink window horizontally."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (split-window-right)
        (ewm-smart-split-open-header))
    (if (window-at-side-p (selected-window) 'right)
        (enlarge-window-horizontally 5)
      (shrink-window-horizontally 5))))

(defun ewm-smart-split-right ()
  "If there is only one window, split vertically and focus right.
   If the current buffer is a C file, check if a matching header file exists and open it.
   Otherwise, enlarge window horizontally."
  (interactive)
  (if (= (length (window-list)) 1)
      (progn
        (split-window-right)
        (windmove-right)
        (ewm-smart-split-open-header))
    (if (window-at-side-p (selected-window) 'right)
        (shrink-window-horizontally 5)
      (enlarge-window-horizontally 5))))

(defun ewm-smart-delete-window ()
  "Delete the current window, or kill the buffer if it's the only window."
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-window)
    (kill-buffer (current-buffer))))

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
