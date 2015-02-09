
(defvar hased-mode-hook nil)

(defvar hased-mode-map
  (let ((hased-mode-map (make-sparse-keymap)))
    (define-key hased-mode-map "\C-j" 'newline-and-indent)
    hased-mode-map)
  "Keymap for Hased major mode")

(add-to-list 'auto-mode-alist '("\\.has\\'" . hased-mode))


(defconst hased-font-lock-keywords-1
  (list
   '("<\\([^<>]*\\)>"  . font-lock-keyword-face)
   '("\"[^\"]*\"" . font-lock-constant-face)
   '("\\<\\([a-z][a-zA-Z0-9_-]*\\)\\>" . font-lock-function-name-face)
   )
  "Minimal highlighting expressions for Hased mode")

(defvar hased-font-lock-keywords hased-font-lock-keywords-1
  "Default highlighting expressions for Hased mode")

(defvar hased-mode-syntax-table
  (let ((hased-mode-syntax-table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" hased-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" hased-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" hased-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" hased-mode-syntax-table)
    (modify-syntax-entry ?< "(>" hased-mode-syntax-table)    
    (modify-syntax-entry ?> ")<" hased-mode-syntax-table)
    hased-mode-syntax-table)
  "Syntax table for hased-mode")
        

(defun hased-mode ()
  "Major mode for editing Hased Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table hased-mode-syntax-table)
  (use-local-map hased-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hased-font-lock-keywords))
  (setq major-mode 'hased-mode)
  (setq mode-name "Hased")
  (run-hooks 'hased-mode-hook))

(provide 'hased-mode)
