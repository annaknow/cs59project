;;; my-hexl.el --- edit a file in a hex dump format using the my-hexl filter -*- lexical-binding: t -*-

;; Copyright (C) 1989, 1994, 1998, 2001-2016 Free Software Foundation,
;; Inc.

;; Author: Keith Gabryelski <ag@wheaties.ai.mit.edu>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: data

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a major mode for editing binary files.  It uses
;; a program called my-hexl, supplied with the GNU Emacs distribution, that
;; can filter a binary into an editable format or from the format back into
;; binary.  For full instructions, invoke `my-hexl-mode' on an empty buffer and
;; do M-x `describe-mode'.
;;
;; NOTE: Remember to change `hexl-program' or `my-hexl-options' if needed.
;;
;; Currently my-hexl only supports big endian hex output with 16 bit
;; grouping.
;;
;; -iso in `my-hexl-options' will allow iso characters to display in the
;; ASCII region of the screen (if your Emacs supports this) instead of
;; changing them to dots.

;;; Code:

(require 'eldoc)
(eval-when-compile (require 'cl-lib))

;;
;; vars here
;;

(defgroup my-hexl nil
  "Edit a file in a hex dump format using the my-hexl filter."
  :group 'data)

(defcustom my-hexl-bits 16
  "The bit grouping that my-hexl will use."
  :type '(choice (const 8 )
                 (const 16)
                 (const 32)
                 (const 64))
  :group 'my-hexl
  :version "24.3")

(defcustom hexl-program "my-hexl"
  "The program that will my-hexlify and demy-hexlify its stdin.
`hexl-program' will always be concatenated with `my-hexl-options'
and \"-de\" when demy-hexlifying a buffer."
  :type 'string
  :group 'my-hexl)

(defcustom my-hexl-iso ""
  "If your Emacs can handle ISO characters, this should be set to
\"-iso\" otherwise it should be \"\"."
  :type 'string
  :group 'my-hexl)

(defcustom my-hexl-options (format "-hex %s" my-hexl-iso)
  "Space separated options to `hexl-program' that suit your needs.
Quoting cannot be used, so the arguments cannot themselves contain spaces.
If you wish to set the `-group-by-X-bits' options, set `my-hexl-bits' instead,
as that will override any bit grouping options set here."
  :type 'string
  :group 'my-hexl)

(defcustom my-hexl-follow-ascii t
  "If non-nil then highlight the ASCII character corresponding to point."
  :type 'boolean
  :group 'my-hexl
  :version "20.3")

(defcustom my-hexl-mode-hook '(my-hexl-follow-line my-hexl-activate-ruler)
  "Normal hook run when entering Hexl mode."
  :type 'hook
  :options '(my-hexl-follow-line my-hexl-activate-ruler eldoc-mode)
  :group 'my-hexl)

(defface my-hexl-address-region
  '((t (:inherit header-line)))
  "Face used in address area of Hexl mode buffer."
  :group 'my-hexl)

(defface my-hexl-ascii-region
  '((t (:inherit header-line)))
  "Face used in ASCII area of Hexl mode buffer."
  :group 'my-hexl)

(defvar my-hexl-max-address 0
  "Maximum offset into my-hexl buffer.")

(defvar my-hexl-mode-map
  (let ((map (make-keymap)))
    ;; Make all self-inserting keys go through my-hexl-self-insert-command,
    ;; because we need to convert them to unibyte characters before
    ;; inserting them into the buffer.
    (define-key map [remap self-insert-command] 'my-hexl-self-insert-command)

    (define-key map "\C-m" 'my-hexl-self-insert-command)
    (define-key map [left] 'my-hexl-backward-char)
    (define-key map [right] 'my-hexl-forward-char)
    (define-key map [up] 'my-hexl-previous-line)
    (define-key map [down] 'my-hexl-next-line)
    (define-key map [M-left] 'my-hexl-backward-short)
    (define-key map [?\e left] 'my-hexl-backward-short)
    (define-key map [M-right] 'my-hexl-forward-short)
    (define-key map [?\e right] 'my-hexl-forward-short)
    (define-key map [next] 'my-hexl-scroll-up)
    (define-key map [prior] 'my-hexl-scroll-down)
    (define-key map [home] 'my-hexl-beginning-of-line)
    (define-key map [end] 'my-hexl-end-of-line)
    (define-key map [C-home] 'my-hexl-beginning-of-buffer)
    (define-key map [C-end] 'my-hexl-end-of-buffer)
    (define-key map [deletechar] 'undefined)
    (define-key map [deleteline] 'undefined)
    (define-key map [insertline] 'undefined)
    (define-key map [S-delete] 'undefined)
    (define-key map "\177" 'undefined)

    (define-key map "\C-a" 'my-hexl-beginning-of-line)
    (define-key map "\C-b" 'my-hexl-backward-char)
    (define-key map "\C-d" 'undefined)
    (define-key map "\C-e" 'my-hexl-end-of-line)
    (define-key map "\C-f" 'my-hexl-forward-char)

    (if (not (memq (key-binding (char-to-string help-char))
		   '(help-command ehelp-command)))
	(define-key map (char-to-string help-char) 'undefined))

    (define-key map "\C-k" 'undefined)
    (define-key map "\C-n" 'my-hexl-next-line)
    (define-key map "\C-o" 'undefined)
    (define-key map "\C-p" 'my-hexl-previous-line)
    (define-key map "\C-q" 'my-hexl-quoted-insert)
    (define-key map "\C-t" 'undefined)
    (define-key map "\C-v" 'my-hexl-scroll-up)
    (define-key map "\C-w" 'undefined)
    (define-key map "\C-y" 'undefined)

    (fset 'my-hexl-ESC-prefix (copy-keymap 'ESC-prefix))
    (define-key map "\e" 'my-hexl-ESC-prefix)
    (define-key map "\e\C-a" 'my-hexl-beginning-of-512b-page)
    (define-key map "\e\C-b" 'my-hexl-backward-short)
    (define-key map "\e\C-d" 'my-hexl-insert-decimal-char)
    (define-key map "\e\C-e" 'my-hexl-end-of-512b-page)
    (define-key map "\e\C-f" 'my-hexl-forward-short)
    (define-key map "\e\C-i" 'undefined)
    (define-key map "\e\C-j" 'undefined)
    (define-key map "\e\C-k" 'undefined)
    (define-key map "\e\C-o" 'my-hexl-insert-octal-char)
    (define-key map "\e\C-q" 'undefined)
    (define-key map "\e\C-t" 'undefined)
    (define-key map "\e\C-x" 'my-hexl-insert-hex-char)
    (define-key map "\eb" 'my-hexl-backward-word)
    (define-key map "\ec" 'undefined)
    (define-key map "\ed" 'undefined)
    (define-key map "\ef" 'my-hexl-forward-word)
    (define-key map "\eg" 'my-hexl-goto-hex-address)
    (define-key map "\ei" 'undefined)
    (define-key map "\ej" 'my-hexl-goto-address)
    (define-key map "\ek" 'undefined)
    (define-key map "\el" 'undefined)
    (define-key map "\eq" 'undefined)
    (define-key map "\es" 'undefined)
    (define-key map "\et" 'undefined)
    (define-key map "\eu" 'undefined)
    (define-key map "\ev" 'my-hexl-scroll-down)
    (define-key map "\ey" 'undefined)
    (define-key map "\ez" 'undefined)
    (define-key map "\e<" 'my-hexl-beginning-of-buffer)
    (define-key map "\e>" 'my-hexl-end-of-buffer)

    (fset 'my-hexl-C-c-prefix (copy-keymap mode-specific-map))
    (define-key map "\C-c" 'my-hexl-C-c-prefix)
    (define-key map "\C-c\C-c" 'my-hexl-mode-exit)

    (fset 'my-hexl-C-x-prefix (copy-keymap 'Control-X-prefix))
    (define-key map "\C-x" 'my-hexl-C-x-prefix)
    (define-key map "\C-x[" 'my-hexl-beginning-of-1k-page)
    (define-key map "\C-x]" 'my-hexl-end-of-1k-page)
    (define-key map "\C-x\C-p" 'undefined)
    (define-key map "\C-x\C-s" 'my-hexl-save-buffer)
    (define-key map "\C-x\C-t" 'undefined)
    map))

;; Variable declarations for suppressing warnings from the byte-compiler.
(defvar ruler-mode)
(defvar ruler-mode-ruler-function)
(defvar hl-line-mode)
(defvar hl-line-range-function)
(defvar hl-line-face)

;; Variables where the original values are stored to.
(defvar my-hexl-mode--old-var-vals ())
(make-variable-buffer-local 'my-hexl-mode--old-var-vals)

(defvar my-hexl-ascii-overlay nil
  "Overlay used to highlight ASCII element corresponding to current point.")
(make-variable-buffer-local 'my-hexl-ascii-overlay)

(defvar my-hexl-font-lock-keywords
  '(("^\\([0-9a-f]+:\\).\\{40\\}  \\(.+$\\)"
     ;; "^\\([0-9a-f]+:\\).+  \\(.+$\\)"
     (1 'my-hexl-address-region t t)
     (2 'my-hexl-ascii-region t t)))
  "Font lock keywords used in `my-hexl-mode'.")

(defun my-hexl-rulerize (string bits)
  (let ((size (/ bits 4)) (strlen (length string)) (pos 0) (ruler ""))
    (while (< pos strlen)
      (setq ruler (concat ruler " " (substring string pos (+ pos size))))
      (setq pos (+ pos size)))
    (substring ruler 1) ))

(defvar my-hexl-rulers
  (mapcar
   (lambda (bits)
     (cons bits
           (concat " 87654321  "
                   (my-hexl-rulerize "00112233445566778899aabbccddeeff" bits)
                   "  0123456789abcdef")))
   '(8 16 32 64)))
;; routines

(put 'my-hexl-mode 'mode-class 'special)

;; 10 chars for the "address: "
;; 32 chars for the my-hexlified bytes
;; 1 char for the space
;; 16 chars for the character display
;; X chars for the spaces (128 bits divided by the my-hexl-bits)
;; 1 char for the newline.
(defun my-hexl-line-displen ()
  "The length of a my-hexl display line (varies with `my-hexl-bits')."
  (+ 60 (/ 128 (or my-hexl-bits 16))))

(defun my-hexl-mode--minor-mode-p (var)
  (memq var '(ruler-mode hl-line-mode)))

(defun my-hexl-mode--setq-local (var val)
  ;; `var' can be either a symbol or a pair, in which case the `car'
  ;; is the getter function and the `cdr' is the corresponding setter.
  (unless (or (member var my-hexl-mode--old-var-vals)
              (assoc var my-hexl-mode--old-var-vals))
    (push (if (or (consp var) (boundp var))
              (cons var
                    (if (consp var) (funcall (car var)) (symbol-value var)))
            var)
          my-hexl-mode--old-var-vals))
  (cond
   ((consp var) (funcall (cdr var) val))
   ((my-hexl-mode--minor-mode-p var) (funcall var (if val 1 -1)))
   (t (set (make-local-variable var) val))))

;;;###autoload
(defun my-hexl-mode (&optional arg)
  "\\<my-hexl-mode-map>A mode for editing binary files in hex dump format.
This is not an ordinary major mode; it alters some aspects
of the current mode's behavior, but not all; also, you can exit
Hexl mode and return to the previous mode using `my-hexl-mode-exit'.

This function automatically converts a buffer into the my-hexl format
using the function `my-hexlify-buffer'.

Each line in the buffer has an \"address\" (displayed in hexadecimal)
representing the offset into the file that the characters on this line
are at and 16 characters from the file (displayed as hexadecimal
values grouped every `my-hexl-bits' bits, and as their ASCII values).

If any of the characters (displayed as ASCII characters) are
unprintable (control or meta characters) they will be replaced by
periods.

If `my-hexl-mode' is invoked with an argument the buffer is assumed to be
in my-hexl format.

A sample format:

  HEX ADDR: 0011 2233 4455 6677 8899 aabb ccdd eeff     ASCII-TEXT
  --------  ---- ---- ---- ---- ---- ---- ---- ----  ----------------
  00000000: 5468 6973 2069 7320 6865 786c 2d6d 6f64  This is my-hexl-mod
  00000010: 652e 2020 4561 6368 206c 696e 6520 7265  e.  Each line re
  00000020: 7072 6573 656e 7473 2031 3620 6279 7465  presents 16 byte
  00000030: 7320 6173 2068 6578 6164 6563 696d 616c  s as hexadecimal
  00000040: 2041 5343 4949 0a61 6e64 2070 7269 6e74   ASCII.and print
  00000050: 6162 6c65 2041 5343 4949 2063 6861 7261  able ASCII chara
  00000060: 6374 6572 732e 2020 416e 7920 636f 6e74  cters.  Any cont
  00000070: 726f 6c20 6f72 206e 6f6e 2d41 5343 4949  rol or non-ASCII
  00000080: 2063 6861 7261 6374 6572 730a 6172 6520   characters.are
  00000090: 6469 7370 6c61 7965 6420 6173 2070 6572  displayed as per
  000000a0: 696f 6473 2069 6e20 7468 6520 7072 696e  iods in the prin
  000000b0: 7461 626c 6520 6368 6172 6163 7465 7220  table character
  000000c0: 7265 6769 6f6e 2e0a                      region..

Movement is as simple as movement in a normal Emacs text buffer.
Most cursor movement bindings are the same: use \\[my-hexl-backward-char], \\[my-hexl-forward-char], \\[my-hexl-next-line], and \\[my-hexl-previous-line]
to move the cursor left, right, down, and up.

Advanced cursor movement commands (ala \\[my-hexl-beginning-of-line], \\[my-hexl-end-of-line], \\[my-hexl-beginning-of-buffer], and \\[my-hexl-end-of-buffer]) are
also supported.

There are several ways to change text in my-hexl mode:

ASCII characters (character between space (0x20) and tilde (0x7E)) are
bound to self-insert so you can simply type the character and it will
insert itself (actually overstrike) into the buffer.

\\[my-hexl-quoted-insert] followed by another keystroke allows you to insert the key even if
it isn't bound to self-insert.  An octal number can be supplied in place
of another key to insert the octal number's ASCII representation.

\\[my-hexl-insert-hex-char] will insert a given hexadecimal value (if it is between 0 and 0xFF)
into the buffer at the current point.

\\[my-hexl-insert-octal-char] will insert a given octal value (if it is between 0 and 0377)
into the buffer at the current point.

\\[my-hexl-insert-decimal-char] will insert a given decimal value (if it is between 0 and 255)
into the buffer at the current point.

\\[my-hexl-mode-exit] will exit `my-hexl-mode'.

Note: saving the file with any of the usual Emacs commands
will actually convert it back to binary format while saving.

You can use \\[my-hexl-find-file] to visit a file in Hexl mode.

\\[describe-bindings] for advanced commands."
  (interactive "p")
  (unless (eq major-mode 'my-hexl-mode)
    (let ((modified (buffer-modified-p))
	  (inhibit-read-only t)
	  (original-point (- (point) (point-min))))
      (and (eobp) (not (bobp))
	   (setq original-point (1- original-point)))
      ;; If `my-hexl-mode' is invoked with an argument the buffer is assumed to
      ;; be in my-hexl format.
      (when (memq arg '(1 nil))
	;; If the buffer's EOL type is -dos, we need to account for
	;; extra CR characters added when my-hexlify-buffer writes the
	;; buffer to a file.
        ;; FIXME: This doesn't take into account multibyte coding systems.
	(when (eq (coding-system-eol-type buffer-file-coding-system) 1)
          (setq original-point (+ (count-lines (point-min) (point))
				  original-point))
	  (or (bolp) (setq original-point (1- original-point))))
        (my-hexlify-buffer)
        (restore-buffer-modified-p modified))
      (set (make-local-variable 'my-hexl-max-address)
           (+ (* (/ (1- (buffer-size)) (my-hexl-line-displen)) 16) 15))
      (condition-case nil
	  (my-hexl-goto-address original-point)
	(error nil)))

    ;; We do not turn off the old major mode; instead we just
    ;; override most of it.  That way, we can restore it perfectly.

    (my-hexl-mode--setq-local '(current-local-map . use-local-map) my-hexl-mode-map)

    (my-hexl-mode--setq-local 'mode-name "Hexl")
    (my-hexl-mode--setq-local 'isearch-search-fun-function
                           'my-hexl-isearch-search-function)
    (my-hexl-mode--setq-local 'major-mode 'my-hexl-mode)

    (my-hexl-mode--setq-local '(syntax-table . set-syntax-table)
                           (standard-syntax-table))

    (add-hook 'write-contents-functions 'my-hexl-save-buffer nil t)

    (my-hexl-mode--setq-local 'require-final-newline nil)


    (my-hexl-mode--setq-local 'font-lock-defaults '(my-hexl-font-lock-keywords t))

    (my-hexl-mode--setq-local 'revert-buffer-function
                           #'my-hexl-revert-buffer-function)
    (add-hook 'change-major-mode-hook 'my-hexl-maybe-demy-hexlify-buffer nil t)

    ;; Set a callback function for eldoc.
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'my-hexl-print-current-point-info)
    (eldoc-add-command-completions "my-hexl-")
    (eldoc-remove-command "my-hexl-save-buffer"
			  "my-hexl-current-address")

    (if my-hexl-follow-ascii (my-hexl-follow-ascii 1)))
  (run-mode-hooks 'my-hexl-mode-hook))


(defun my-hexl-isearch-search-function ()
  (if (and (not isearch-regexp) (not isearch-regexp-function))
      (lambda (string &optional bound noerror count)
	(funcall
	 (if isearch-forward 're-search-forward 're-search-backward)
         (let ((textre
                (if (> (length string) 80)
                    (regexp-quote string)
                  (mapconcat (lambda (c) (regexp-quote (string c))) string
                             "\\(?:\n\\(?:[:a-f0-9]+ \\)+ \\)?"))))
           (if (string-match "\\` ?\\([a-f0-9]+ \\)*[a-f0-9]+ ?\\'" string)
               (concat textre "\\|"
                       (mapconcat 'regexp-quote (split-string string " ")
                                  " \\(?: .+\n[a-f0-9]+: \\)?"))
             textre))
	 bound noerror count))
    (isearch-search-fun-default)))

(defvar my-hexl-in-save-buffer nil)

(defun my-hexl-save-buffer ()
  "Save a my-hexl format buffer as binary in visited file if modified."
  (interactive)
  (if my-hexl-in-save-buffer nil
    (restore-buffer-modified-p
     (if (buffer-modified-p)
         (let ((buf (generate-new-buffer " my-hexl"))
               (name (buffer-name))
               (start (point-min))
               (end (point-max))
               modified)
           (with-current-buffer buf
             (insert-buffer-substring name start end)
             (set-buffer name)
             (demy-hexlify-buffer)
             ;; Prevent infinite recursion.
             (let ((my-hexl-in-save-buffer t))
               (save-buffer))
             (setq modified (buffer-modified-p))
             (delete-region (point-min) (point-max))
             (insert-buffer-substring buf start end)
             (kill-buffer buf)
             modified))
       (message "(No changes need to be saved)")
       nil))
    ;; Return t to indicate we have saved t
    t))

;;;###autoload
(defun my-hexl-find-file (filename)
  "Edit file FILENAME as a binary file in hex dump format.
Switch to a buffer visiting file FILENAME, creating one if none exists,
and edit the file in `my-hexl-mode'."
  (interactive
   (list
    (let ((completion-ignored-extensions nil))
      (read-file-name "Filename: " nil nil 'ret-must-match))))
  ;; Ignore the user's setting of default major-mode.
  (cl-letf (((default-value 'major-mode) 'fundamental-mode))
    (find-file-literally filename))
  (if (not (eq major-mode 'my-hexl-mode))
      (my-hexl-mode)))

;;; NEW ;;; 
(defun my-hexl-disassemble ()
  "Disassemble the binary using objdump, and show the output in a new buffer in a split window, with `elf-mode' syntax highlighting" 
  (interactive
   (progn (shell-command (concat "objdump -d " (buffer-name)) (concat (buffer-name) ".elf"))
	  (with-current-buffer (concat (buffer-name) ".elf") 
	    (funcall 'elf-mode)))))

(defun my-hexl-revert-buffer-function (_ignore-auto _noconfirm)
  (let ((coding-system-for-read 'no-conversion)
	revert-buffer-function)
    ;; Call the original `revert-buffer' without code conversion; also
    ;; prevent it from changing the major mode to normal-mode, which
    ;; calls `set-auto-mode'.
    (revert-buffer nil nil t)
    ;; A couple of hacks are necessary here:
    ;; 1. change the major-mode to one other than my-hexl-mode since the
    ;; function `my-hexl-mode' does nothing if the current major-mode is
    ;; already my-hexl-mode.
    ;; 2. reset change-major-mode-hook in case that `my-hexl-mode'
    ;; previously added my-hexl-maybe-demy-hexlify-buffer to it.
    (remove-hook 'change-major-mode-hook 'my-hexl-maybe-demy-hexlify-buffer t)
    (setq major-mode 'fundamental-mode)
    (my-hexl-mode)))

(defun my-hexl-mode-exit (&optional arg)
  "Exit Hexl mode, returning to previous mode.
With arg, don't unmy-hexlify buffer."
  (interactive "p")
  (if (or (eq arg 1) (not arg))
      (let ((modified (buffer-modified-p))
	    (inhibit-read-only t)
	    (original-point (1+ (my-hexl-current-address))))
	(demy-hexlify-buffer)
	(remove-hook 'write-contents-functions 'my-hexl-save-buffer t)
	(restore-buffer-modified-p modified)
	(goto-char original-point)
	;; Maybe adjust point for the removed CR characters.
	(when (eq (coding-system-eol-type buffer-file-coding-system) 1)
	  (setq original-point (- original-point
				  (count-lines (point-min) (point))))
	  (or (bobp) (setq original-point (1+ original-point))))
	(goto-char original-point)))

  (remove-hook 'change-major-mode-hook 'my-hexl-maybe-demy-hexlify-buffer t)
  (remove-hook 'post-command-hook 'my-hexl-follow-ascii-find t)
  (setq my-hexl-ascii-overlay nil)

  (let ((mms ()))
    (dolist (varval my-hexl-mode--old-var-vals)
      (let* ((bound (consp varval))
             (var (if bound (car varval) varval))
             (val (cdr-safe varval)))
        (cond
         ((consp var) (funcall (cdr var) val))
         ((my-hexl-mode--minor-mode-p var) (push (cons var val) mms))
         (bound (set (make-local-variable var) val))
         (t (kill-local-variable var)))))
    (kill-local-variable 'my-hexl-mode--old-var-vals)
    ;; Enable/disable minor modes.  Do it after having reset the other vars,
    ;; since some of them may affect the minor modes.
    (dolist (mm mms)
      (funcall (car mm) (if (cdr mm) 1 -1))))

  (force-mode-line-update))

(defun my-hexl-maybe-demy-hexlify-buffer ()
  "Convert a my-hexl format buffer to binary.
Ask the user for confirmation."
  (if (y-or-n-p "Convert contents back to binary format? ")
      (let ((modified (buffer-modified-p))
	    (inhibit-read-only t)
	    (original-point (1+ (my-hexl-current-address))))
	(demy-hexlify-buffer)
	(remove-hook 'write-contents-functions 'my-hexl-save-buffer t)
	(restore-buffer-modified-p modified)
	(goto-char original-point))))

(defun my-hexl-current-address (&optional validate)
  "Return current my-hexl-address."
  (interactive)
  (let ((current-column
         (- (% (- (point) (point-min) -1) (my-hexl-line-displen)) 11))
	(my-hexl-address 0))
    (if (< current-column 0)
	(if validate
	    (error "Point is not on a character in the file")
	  (setq current-column 0)))
    (setq my-hexl-address
          (+ (* (/ (- (point) (point-min) -1)
                   (my-hexl-line-displen)) 16)
	     (if (>= current-column (- (my-hexl-ascii-start-column) 10))
		 (- current-column (- (my-hexl-ascii-start-column) 10))
               (/ (- current-column
                     (/ current-column (1+ (/ my-hexl-bits 4)))) 2))))
    (when (called-interactively-p 'interactive)
      (message "Current address is %d/0x%08x" my-hexl-address my-hexl-address))
    my-hexl-address))

(defun my-hexl-print-current-point-info ()
  "Return current my-hexl-address in string.
This function is intended to be used as eldoc callback."
  (let ((addr (my-hexl-current-address)))
    (format "Current address is %d/0x%08x" addr addr)))

(defun my-hexl-ascii-start-column ()
  "Column at which the ASCII portion of the my-hexl display starts."
  (+ 43 (/ 128 my-hexl-bits)))

(defun my-hexl-address-to-marker (address)
  "Return buffer position for ADDRESS."
  (interactive "nAddress: ")
  (let ((N (* (% address 16) 2)))
    (+ (* (/ address 16) (my-hexl-line-displen)) ; my-hexl line no * display length
       10                      ; 10 chars for the "address: " prefix
       (point-min)             ; base offset (point usually starts at 1, not 0)
       (+ N (/ N (/ my-hexl-bits 4))) )) ) ; char offset into my-hexl display line

(defun my-hexl-goto-address (address)
  "Go to my-hexl-mode (decimal) address ADDRESS.
Signal error if ADDRESS is out of range."
  (interactive "nAddress: ")
  (if (or (< address 0) (> address my-hexl-max-address))
      (error "Out of my-hexl region"))
  (goto-char (my-hexl-address-to-marker address)))

(defun my-hexl-goto-hex-address (hex-address)
  "Go to Hexl mode address (hex string) HEX-ADDRESS.
Signal error if HEX-ADDRESS is out of range."
  (interactive "sHex Address: ")
  (my-hexl-goto-address (my-hexl-hex-string-to-integer hex-address)))

(defun my-hexl-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING."
  (interactive "sHex number: ")
  (let ((hex-num 0))
    (while (not (equal hex-string ""))
      (setq hex-num (+ (* hex-num 16)
		       (my-hexl-hex-char-to-integer (string-to-char hex-string))))
      (setq hex-string (substring hex-string 1)))
    hex-num))

(defun my-hexl-octal-string-to-integer (octal-string)
  "Return decimal integer for OCTAL-STRING."
  (interactive "sOctal number: ")
  (let ((oct-num 0))
    (while (not (equal octal-string ""))
      (setq oct-num (+ (* oct-num 8)
		       (my-hexl-oct-char-to-integer
			(string-to-char octal-string))))
      (setq octal-string (substring octal-string 1)))
    oct-num))

;; move point functions

(defun my-hexl-backward-char (arg)
  "Move to left ARG bytes (right if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-goto-address (- (my-hexl-current-address) arg)))

(defun my-hexl-forward-char (arg)
  "Move to right ARG bytes (left if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-goto-address (+ (my-hexl-current-address) arg)))

(defun my-hexl-backward-short (arg)
  "Move to left ARG shorts (right if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-goto-address (let ((address (my-hexl-current-address)))
		       (if (< arg 0)
			   (progn
			     (setq arg (- arg))
			     (while (> arg 0)
                               (setq address
                                     (if (> address my-hexl-max-address)
                                         (progn
                                           (message "End of buffer.")
                                           my-hexl-max-address)
                                       (if (equal address (logior address 3))
                                           (+ address 4)
                                         (logior address 3))))
			       (setq arg (1- arg)))
                             (setq address
                                   (if (> address my-hexl-max-address)
                                       (progn
                                         (message "End of buffer.")
                                         my-hexl-max-address)
                                     (logior address 3))))
			 (while (> arg 0)
			   (if (not (equal address (logand address -4)))
			       (setq address (logand address -4))
			     (if (not (equal address 0))
				 (setq address (- address 4))
			       (message "Beginning of buffer.")))
			   (setq arg (1- arg))))
		       address)))

(defun my-hexl-forward-short (arg)
  "Move to right ARG shorts (left if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-backward-short (- arg)))

(defun my-hexl-backward-word (arg)
  "Move to left ARG words (right if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-goto-address (let ((address (my-hexl-current-address)))
		       (if (< arg 0)
			   (progn
			     (setq arg (- arg))
			     (while (> arg 0)
                               (setq address
                                     (if (> address my-hexl-max-address)
                                         (progn
                                           (message "End of buffer.")
                                           my-hexl-max-address)
                                       (if (equal address (logior address 7))
                                           (+ address 8)
                                         (logior address 7))))
			       (setq arg (1- arg)))
                             (setq address
                                   (if (> address my-hexl-max-address)
                                       (progn
                                         (message "End of buffer.")
                                         my-hexl-max-address)
                                     (logior address 7))))
			 (while (> arg 0)
			   (if (not (equal address (logand address -8)))
			       (setq address (logand address -8))
			     (if (not (equal address 0))
				 (setq address (- address 8))
			       (message "Beginning of buffer.")))
			   (setq arg (1- arg))))
		       address)))

(defun my-hexl-forward-word (arg)
  "Move to right ARG words (left if ARG negative) in Hexl mode."
  (interactive "p")
  (my-hexl-backward-word (- arg)))

(defun my-hexl-previous-line (arg)
  "Move vertically up ARG lines [16 bytes] (down if ARG negative) in Hexl mode.
If there is no byte at the target address move to the last byte in that line."
  (interactive "p")
  (my-hexl-next-line (- arg)))

(defun my-hexl-next-line (arg)
  "Move vertically down ARG lines [16 bytes] (up if ARG negative) in Hexl mode.
If there is no byte at the target address move to the last byte in that line."
  (interactive "p")
  (my-hexl-goto-address (let ((address (+ (my-hexl-current-address) (* arg 16))))
		       (if (and (< arg 0) (< address 0))
				(progn (message "Out of my-hexl region.")
				       (setq address
					     (% (my-hexl-current-address) 16)))
			 (if (and (> address my-hexl-max-address)
				  (< (% my-hexl-max-address 16) (% address 16)))
			     (setq address my-hexl-max-address)
			   (if (> address my-hexl-max-address)
			       (progn (message "Out of my-hexl region.")
				      (setq
				       address
				       (+ (logand my-hexl-max-address -16)
					  (% (my-hexl-current-address) 16)))))))
		       address)))

(defun my-hexl-beginning-of-buffer (arg)
  "Move to the beginning of the my-hexl buffer.
Leaves `my-hexl-mark' at previous position.
With prefix arg N, puts point N bytes of the way from the true beginning."
  (interactive "p")
  (push-mark (point))
  (my-hexl-goto-address (+ 0 (1- arg))))

(defun my-hexl-end-of-buffer (arg)
  "Go to `my-hexl-max-address' minus ARG."
  (interactive "p")
  (push-mark (point))
  (my-hexl-goto-address (- my-hexl-max-address (1- arg))))

(defun my-hexl-beginning-of-line ()
  "Goto beginning of line in Hexl mode."
  (interactive)
  (goto-char (+ (* (/ (point) (my-hexl-line-displen)) (my-hexl-line-displen)) 11)))

(defun my-hexl-end-of-line ()
  "Goto end of line in Hexl mode."
  (interactive)
  (my-hexl-goto-address (let ((address (logior (my-hexl-current-address) 15)))
		       (if (> address my-hexl-max-address)
			   (setq address my-hexl-max-address))
		       address)))

(defun my-hexl-scroll-down (arg)
  "Scroll my-hexl buffer window upward ARG lines; or near full window if no ARG."
  (interactive "P")
  (setq arg (if (null arg)
                (1- (window-height))
              (prefix-numeric-value arg)))
  (my-hexl-scroll-up (- arg)))

(defun my-hexl-scroll-up (arg)
  "Scroll my-hexl buffer window upward ARG lines; or near full window if no ARG.
If there's no byte at the target address, move to the first or last line."
  (interactive "P")
  (setq arg (if (null arg)
                (1- (window-height))
              (prefix-numeric-value arg)))
  (let* ((movement (* arg 16))
	 (address (my-hexl-current-address))
	 (dest (+ address movement)))
    (cond
     ;; If possible, try to stay at the same offset from the beginning
     ;; of the 16-byte group, even if we move to the first or last
     ;; group.
     ((and (> dest my-hexl-max-address)
	   (>= (% my-hexl-max-address 16) (% address 16)))
      (setq dest (+ (logand my-hexl-max-address -16) (% address 16))))
     ((> dest my-hexl-max-address)
      (setq dest my-hexl-max-address))
     ((< dest 0)
      (setq dest (% address 16))))
    (if (/= dest (+ address movement))
	(message "Out of my-hexl region."))
    (my-hexl-goto-address dest)
    (recenter 0)))

(defun my-hexl-beginning-of-1k-page ()
  "Go to beginning of 1KB boundary."
  (interactive)
  (my-hexl-goto-address (logand (my-hexl-current-address) -1024)))

(defun my-hexl-end-of-1k-page ()
  "Go to end of 1KB boundary."
  (interactive)
  (my-hexl-goto-address
   (max my-hexl-max-address (logior (my-hexl-current-address) 1023))))

(defun my-hexl-beginning-of-512b-page ()
  "Go to beginning of 512 byte boundary."
  (interactive)
  (my-hexl-goto-address (logand (my-hexl-current-address) -512)))

(defun my-hexl-end-of-512b-page ()
  "Go to end of 512 byte boundary."
  (interactive)
  (my-hexl-goto-address
   (max my-hexl-max-address (logior (my-hexl-current-address) 511))))

(defun my-hexl-quoted-insert (arg)
  "Read next input character and insert it.
Useful for inserting control characters and non-ASCII characters given their
numerical code.
You may also type octal digits, to insert a character with that code."
  (interactive "p")
  (my-hexl-insert-multibyte-char (read-quoted-char) arg))

;00000000: 0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789ABCDEF

(defun my-hexl-options (&optional test)
  "Combine `my-hexl-bits' with `my-hexl-options', altering `my-hexl-options' as needed
to produce the command line options to pass to the my-hexl command."
  (let ((opts (or test my-hexl-options)))
    (when (memq my-hexl-bits '(8 16 32 64))
      (when (string-match "\\(.*\\)-group-by-[0-9]+-bits\\(.*\\)" opts)
        (setq opts (concat (match-string 1 opts)
                           (match-string 2 opts))))
      (setq opts (format "%s -group-by-%d-bits " opts my-hexl-bits)) )
    opts))

;;;###autoload
(defun my-hexlify-buffer ()
  "Convert a binary buffer to my-hexl format.
This discards the buffer's undo information."
  (interactive)
  (and (consp buffer-undo-list)
       (or (y-or-n-p "Converting to my-hexl format discards undo info; ok? ")
	   (error "Aborted"))
       (setq buffer-undo-list nil))
  ;; Don't decode text in the ASCII part of `my-hexl' program output.
  (let ((coding-system-for-read 'raw-text)
	(coding-system-for-write buffer-file-coding-system)
	(buffer-undo-list t))
    (apply 'call-process-region (point-min) (point-max)
	   (expand-file-name hexl-program exec-directory)
	   t t nil
           ;; Manually encode the args, otherwise they're encoded using
           ;; coding-system-for-write (i.e. buffer-file-coding-system) which
           ;; may not be what we want (e.g. utf-16 on a non-utf-16 system).
           (mapcar (lambda (s)
                     (if (not (multibyte-string-p s)) s
                       (encode-coding-string s locale-coding-system)))
                   (split-string (my-hexl-options))))
    (if (> (point) (my-hexl-address-to-marker my-hexl-max-address))
	(my-hexl-goto-address my-hexl-max-address))))

(defun demy-hexlify-buffer ()
  "Convert a my-hexl format buffer to binary.
This discards the buffer's undo information."
  (interactive)
  (and (consp buffer-undo-list)
       (or (y-or-n-p "Converting from my-hexl format discards undo info; ok? ")
	   (error "Aborted"))
       (setq buffer-undo-list nil))
  (let ((coding-system-for-write 'raw-text)
	(coding-system-for-read buffer-file-coding-system)
	(buffer-undo-list t))
    (apply 'call-process-region (point-min) (point-max)
	   (expand-file-name hexl-program exec-directory)
	   t t nil "-de" (split-string (my-hexl-options)))))

(defun my-hexl-char-after-point ()
  "Return char for ASCII hex digits at point."
  (my-hexl-htoi (char-after (point))
	     (char-after (1+ (point)))))

(defun my-hexl-htoi (lh rh)
  "Hex (char) LH (char) RH to integer."
    (+ (* (my-hexl-hex-char-to-integer lh) 16)
       (my-hexl-hex-char-to-integer rh)))

(defun my-hexl-hex-char-to-integer (character)
  "Take a char and return its value as if it was a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error "Invalid hex digit `%c'" ch)))))

(defun my-hexl-oct-char-to-integer (character)
  "Take a char and return its value as if it was a octal digit."
  (if (and (>= character ?0) (<= character ?7))
      (- character ?0)
    (error "Invalid octal digit `%c'" character)))

(defun my-hexl-printable-character (ch)
  "Return a displayable string for character CH."
  (format "%c" (if (equal my-hexl-iso "")
		   (if (or (< ch 32) (>= ch 127))
		       46
		     ch)
		 (if (or (< ch 32) (and (>= ch 127) (< ch 160)))
		     46
		   ch))))

(defun my-hexl-insert-multibyte-char (ch num)
  "Insert a possibly multibyte character CH NUM times.

Non-ASCII characters are first encoded with `buffer-file-coding-system',
and their encoded form is inserted byte by byte."
  (let ((charset (char-charset ch))
	(coding (if (or (null buffer-file-coding-system)
			;; coding-system-type equals t means undecided.
			(eq (coding-system-type buffer-file-coding-system) t))
		    (default-value 'buffer-file-coding-system)
		  buffer-file-coding-system)))
    (cond ((and (> ch 0) (< ch 256))
	   (my-hexl-insert-char ch num))
	  ((eq charset 'unknown)
	   (error
	    "0x%x -- invalid character code; use \\[my-hexl-insert-hex-string]"
	    ch))
	  (t
	   (let ((encoded (encode-coding-char ch coding))
		 (internal (string-as-unibyte (char-to-string ch)))
		 internal-hex)
	     ;; If encode-coding-char returns nil, it means our character
	     ;; cannot be safely encoded with buffer-file-coding-system.
	     ;; In that case, we offer to insert the internal representation
	     ;; of that character, byte by byte.
	     (when (null encoded)
	       (setq internal-hex
		     (mapconcat (function (lambda (c) (format "%x" c)))
				internal " "))
	       (if (yes-or-no-p
		    (format-message
		     "Insert char 0x%x's internal representation \"%s\"? "
		     ch internal-hex))
		   (setq encoded internal)
		 (error
		  "Can't encode `0x%x' with this buffer's coding system; %s"
		  ch
		  (substitute-command-keys "try \\[my-hexl-insert-hex-string]"))))
	     (while (> num 0)
	       (mapc
		(function (lambda (c) (my-hexl-insert-char c 1))) encoded)
	       (setq num (1- num))))))))

(defun my-hexl-self-insert-command (arg)
  "Insert this character.
Interactively, with a numeric argument, insert this character that many times.

Non-ASCII characters are first encoded with `buffer-file-coding-system',
and their encoded form is inserted byte by byte."
  (interactive "p")
  (my-hexl-insert-multibyte-char last-command-event arg))


(defvar my-hexl-allow-direct-hex nil
  "allow direct insertion of hex values")
(make-variable-buffer-local 'my-hexl-allow-direct-hex)

(defun my-hexl-direct-hex-insert ()
  "toggle insertion directly into the hex bytes
customize the variable `my-hexl-allow-direct-hex' to disable"
  (interactive "P") 
  (setq my-hexl-allow-direct-hex (not my-hexl-allow-direct-hex)))
 

(defun my-hexl-insert-char (ch num)
  "Insert the character CH NUM times in a my-hexl buffer.

CH must be a unibyte character whose value is between 0 and 255."
    (if (or (< ch 0) (> ch 255))
	(error "Invalid character 0x%x -- must be in the range [0..255]" ch))
    (let ((address (my-hexl-current-address t)))
      (while (> num 0)
	(let ((hex-position (my-hexl-address-to-marker address))
	      (ascii-position
	       (+ (* (/ address 16) (my-hexl-line-displen))
		  (my-hexl-ascii-start-column)
		  (point-min)
		  (% address 16)))
	      at-ascii-position)
	  (if (= (point) ascii-position)
	      (setq at-ascii-position t))
	  (goto-char hex-position)
	  (delete-char 2) 
	  (insert (format "%02x" ch))
	  (goto-char ascii-position)
	  (delete-char 1)
	  (insert (my-hexl-printable-character ch))
	  (or (eq address my-hexl-max-address)
	      (setq address (1+ address)))
	  (my-hexl-goto-address address)
	  (if at-ascii-position
	      (progn
		(beginning-of-line)
		(forward-char (my-hexl-ascii-start-column))
		(forward-char (% address 16)))))
	(setq num (1- num)))))

;; hex conversion

(defun my-hexl-insert-hex-char (arg)
  "Insert a character given by its hexadecimal code ARG times at point."
  (interactive "p")
  (let ((num (my-hexl-hex-string-to-integer (read-string "Hex number: "))))
    (if (< num 0)
	(error "Hex number out of range")
      (my-hexl-insert-multibyte-char num arg))))

(defun my-hexl-insert-hex-string (str arg)
  "Insert hexadecimal string STR at point ARG times.
Embedded whitespace, dashes, and periods in the string are ignored."
  (interactive "sHex string: \np")
  (setq str (replace-regexp-in-string "[- \t.]" "" str))
  (let ((chars '()))
    (let ((len (length str))
	  (idx 0))
      (if (eq (logand len 1) 1)
	  (let ((num (my-hexl-hex-string-to-integer (substring str 0 1))))
	    (setq chars (cons num chars))
	    (setq idx 1)))
      (while (< idx len)
	(let* ((nidx (+ idx 2))
	       (num (my-hexl-hex-string-to-integer (substring str idx nidx))))
	  (setq chars (cons num chars))
	  (setq idx nidx))))
    (setq chars (nreverse chars))
    (while (> arg 0)
      (let ((chars chars))
	(while chars
	  (my-hexl-insert-char (car chars) 1)
	  (setq chars (cdr chars))))
      (setq arg (- arg 1)))))

(defun my-hexl-insert-decimal-char (arg)
  "Insert a character given by its decimal code ARG times at point."
  (interactive "p")
  (let ((num (string-to-number (read-string "Decimal Number: "))))
    (if (< num 0)
	(error "Decimal number out of range")
      (my-hexl-insert-multibyte-char num arg))))

(defun my-hexl-insert-octal-char (arg)
  "Insert a character given by its octal code ARG times at point."
  (interactive "p")
  (let ((num (my-hexl-octal-string-to-integer (read-string "Octal Number: "))))
    (if (< num 0)
	(error "Decimal number out of range")
      (my-hexl-insert-multibyte-char num arg))))

(defun my-hexl-follow-ascii (&optional arg)
  "Toggle following ASCII in Hexl buffers.
With prefix ARG, turn on following if and only if ARG is positive.
When following is enabled, the ASCII character corresponding to the
element under the point is highlighted.
Customize the variable `my-hexl-follow-ascii' to disable this feature."
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
	       (not my-hexl-ascii-overlay))))

    (if on-p
      ;; turn it on
      (if (not my-hexl-ascii-overlay)
	  (progn
	    (setq my-hexl-ascii-overlay (make-overlay 1 1)
		  my-hexl-follow-ascii t)
	    (overlay-put my-hexl-ascii-overlay 'face 'highlight)
	    (add-hook 'post-command-hook 'my-hexl-follow-ascii-find nil t)))
      ;; turn it off
      (if my-hexl-ascii-overlay
	  (progn
	    (delete-overlay my-hexl-ascii-overlay)
	    (setq my-hexl-ascii-overlay nil
		  my-hexl-follow-ascii nil)
	    (remove-hook 'post-command-hook 'my-hexl-follow-ascii-find t)
	    )))))

(defun my-hexl-activate-ruler ()
  "Activate `ruler-mode'."
  (require 'ruler-mode)
  (my-hexl-mode--setq-local 'ruler-mode-ruler-function
                         #'my-hexl-mode-ruler)
  (my-hexl-mode--setq-local 'ruler-mode t))

(defun my-hexl-follow-line ()
  "Activate `hl-line-mode'."
  (require 'hl-line)
  (my-hexl-mode--setq-local 'hl-line-range-function
                         #'my-hexl-highlight-line-range)
  (my-hexl-mode--setq-local 'hl-line-face 'highlight)
  (my-hexl-mode--setq-local 'hl-line-mode t))

(defun my-hexl-highlight-line-range ()
  "Return the range of address region for the point.
This function is assumed to be used as callback function for `hl-line-mode'."
  (cons
   (line-beginning-position)
   ;; 9 stands for (length "87654321:")
   (+ (line-beginning-position) 9)))

(defun my-hexl-follow-ascii-find ()
  "Find and highlight the ASCII element corresponding to current point."
  (let ((pos (+ (my-hexl-ascii-start-column)
		(- (point) (current-column))
		(mod (my-hexl-current-address) 16))))
    (move-overlay my-hexl-ascii-overlay pos (1+ pos))
    ))

(defun my-hexl-mode-ruler ()
  "Return a string ruler for Hexl mode."
  (let* ((highlight (mod (my-hexl-current-address) 16))
	 (s (cdr (assq my-hexl-bits my-hexl-rulers)))
	 (pos 0))
    (set-text-properties 0 (length s) nil s)
    ;; Turn spaces in the header into stretch specs so they work
    ;; regardless of the header-line face.
    (while (string-match "[ \t]+" s pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0) pos 'display
			 ;; Assume fixed-size chars
			 `(space :align-to ,(1- pos))
			 s))
    ;; Highlight the current column.
    (let ( (offset (+ (* 2 highlight) (/ (* 8 highlight) my-hexl-bits))) )
      (put-text-property (+ 11 offset) (+ 13 offset) 'face 'highlight s))
    ;; Highlight the current ascii column
    (put-text-property (+ (my-hexl-ascii-start-column) highlight 1)
                       (+ (my-hexl-ascii-start-column) highlight 2)
                       'face 'highlight s)
    s))

;; startup stuff.

(easy-menu-define my-hexl-menu my-hexl-mode-map "Hexl Mode menu"
  `("Hexl"
    :help "Hexl-specific Features"

    ["Backward short" my-hexl-backward-short
     :help "Move to left a short"]
    ["Forward short" my-hexl-forward-short
     :help "Move to right a short"]
    ["Backward word" my-hexl-backward-short
     :help "Move to left a word"]
    ["Forward word" my-hexl-forward-short
     :help "Move to right a word"]
    "-"
    ["Beginning of 512b page" my-hexl-beginning-of-512b-page
     :help "Go to beginning of 512 byte boundary"]
    ["End of 512b page" my-hexl-end-of-512b-page
     :help "Go to end of 512 byte boundary"]
    ["Beginning of 1K page" my-hexl-beginning-of-1k-page
     :help "Go to beginning of 1KB boundary"]
    ["End of 1K page" my-hexl-end-of-1k-page
     :help "Go to end of 1KB boundary"]
    "-"
    ["Go to address" my-hexl-goto-address
     :help "Go to my-hexl-mode (decimal) address"]
    ["Go to address" my-hexl-goto-hex-address
     :help "Go to my-hexl-mode (hex string) address"]
    "-"
    ["Insert decimal char" my-hexl-insert-decimal-char
     :help "Insert a character given by its decimal code"]
    ["Insert hex char" my-hexl-insert-hex-char
     :help "Insert a character given by its hexadecimal code"]
    ["Insert octal char" my-hexl-insert-octal-char
     :help "Insert a character given by its octal code"]
    "-"
    ["Exit my-hexl mode" my-hexl-mode-exit
     :help "Exit my-hexl mode returning to previous mode"]))

(provide 'my-hexl)

;;; my-hexl.el ends here
