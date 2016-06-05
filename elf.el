
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
     (modify-syntax-entry ?< "\"" elf-mode-syntax-table)
     (modify-syntax-entry ?> "\"" elf-mode-syntax-table)
     elf-mode-syntax-table)
   "syntax table" )

;; regex from the asm-mode 
 (defconst elf-font-lock-keywords
   (append
      ;; memory, with and without a colon
      '(("^\\s-*\\([[:xdigit:]]*\\b\\|:\\)" . font-lock-keyword-face)

	;; comments (should do this another way, but...) 
	("#.*" . font-lock-comment-face)

	;; anything in <> 
	("<.*?>" . font-lock-function-name-face)
  
	;; constants
	("\$0x[[:xdigit:]]+" . font-lock-constant-face)

	;; %register
	("%\\sw+" . font-lock-variable-name-face))
      cpp-font-lock-keywords)
   "Additional expressions to highlight in Assembler mode.")

(define-derived-mode elf-mode fundamental-mode "elf" 
  "a major mode for viewing elf files" 
  (setq-local comment-start "# ") 
  (setq-local comment-end "")
 ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
	      '(elf-font-lock-keywords)))


(provide 'elf) 

;; elf.el ends here
