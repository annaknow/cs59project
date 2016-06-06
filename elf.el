
(defvar elf-mode-hook nil)

(defvar elf-mode-map 
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\C-j" 'newline-and-indent); define mappings in here
    kmap)
  "keymap for `elf-mode'.")

(defvar elf-mode-syntax-table
  (let ((elf-mode-syntax-table (make-syntax-table)))
     (modify-syntax-entry ?# "< b"elf-mode-syntax-table ) ;; begin comment
     (modify-syntax-entry ?\n "> b" elf-mode-syntax-table)  ;; end comment
     (modify-syntax-entry ?\" "w" elf-mode-syntax-table)  ;;" remove string delimiters
     (modify-syntax-entry ?< "(>" elf-mode-syntax-table)  
     (modify-syntax-entry ?> "(<" elf-mode-syntax-table) 
     elf-mode-syntax-table)
   "syntax table" )


(defconst elf-font-lock-keywords
   (append
      ;; memory (indented) 
      '(("^\\s-\\s-\\([[:xdigit:]]*\\b\\)" . font-lock-keyword-face)
	;; anything in <> 
	("<.*?>" . font-lock-function-name-face)
	;; constants
	("\$0x[[:xdigit:]]+" . font-lock-constant-face)
	;; %register
	("%\\sw\\sw\\sw?" . font-lock-variable-name-face))
      cpp-font-lock-keywords)
   "Additional expressions to highlight in Assembler mode.")

;; derived mode start up
(define-derived-mode elf-mode fundamental-mode "elf" 
  "a major mode for viewing elf files" 
;;   :syntax-table elf-mode-syntax-table
  (setq-local comment-start "# ") 
  (setq-local comment-end "")
  (setq-local font-lock-defaults
	      '(elf-font-lock-keywords)))


(provide 'elf) 

;; elf.el ends here
