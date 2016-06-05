
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
     elf-mode-syntax-table)
   "syntax table" )

;; regex from the asm-mode 
 (defconst elf-font-lock-keywords
   (append
    ;; this makes things blue 
   ;; '(("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
    ;;   (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
      ;; label started from ".". this has the effect of making the memory purple
      '(("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:"
       1 font-lock-function-name-face)

      ("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
       2 font-lock-keyword-face)

      ;; directive started from ".".
      ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
       1 font-lock-keyword-face)

      ;; anything in <> 
      ("<.*?>" . font-lock-function-name-face)

      ;; constants
      ("\$0x[0-9 a-f]+" . font-lock-constant-face)

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
