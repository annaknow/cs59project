(defvar basic-hexl-mode-hook
  nil)

;;(load-file "elf.el")

(define-derived-mode basic-hexl-mode fundamental-mode "basic-hexl"
  "a totally editable but not very safe hexl mode" 
    (add-hook 'write-contents-functions 'basic-hexl-save-buffer nil t)
    (add-hook 'change-major-mode-hook 'basic-hexl-mode-exit nil t)
    (hexlify-buffer)
)


;;; NEW ;;; 
(defun basic-hexl-disassemble ()
  "Disassemble the binary using objdump, and show the output in a new buffer in a split window, with `elf-mode' syntax highlighting" 
  (interactive
   (progn 
     (dehexlify-buffer)
     (shell-command (concat "objdump -d " (buffer-name)) (concat (buffer-name) ".elf"))
     (with-current-buffer (concat (buffer-name) ".elf") 
       (funcall 'elf-mode))
     (hexlify-buffer))))

(defvar basic-hexl-in-save-buffer nil)

;; from hexl
(defun basic-hexl-save-buffer ()
  "Save a hexl format buffer as binary in visited file if modified.
Saving updates the ascii representation of the text" 
  (interactive)
  (if hexl-in-save-buffer nil
    (restore-buffer-modified-p
     (if (buffer-modified-p)
         (let ((buf (generate-new-buffer " hexl"))
               (name (buffer-name))
               (start (point-min))
               (end (point-max))
               modified)
           (with-current-buffer buf
             (insert-buffer-substring name start end)
             (set-buffer name)
             (dehexlify-buffer)
             ;; Prevent infinite recursion.
             (let ((hexl-in-save-buffer t))
               (save-buffer))
             (setq modified (buffer-modified-p))
             (delete-region (point-min) (point-max))
             (insert-buffer-substring buf start end)
             (kill-buffer buf)
             modified)
;;modified here 
	   (setq buffer-file-coding-system 'raw-text)
	   (revert-buffer t t)
	   (funcall 'basic-hexl-mode))
       (message "(No changes need to be saved)")
       nil))
    ;; Return t to indicate we have saved t
    t))


(defun basic-hexl-mode-exit ()
  "Exit Hexl mode, dehexlify" 
  (interactive "p")
  (dehexlify-buffer))



(provide 'basic-hexl)
