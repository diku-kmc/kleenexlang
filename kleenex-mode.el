
(defvar kleenex-mode-hook nil)

(defvar kleenex-mode-map
  (let ((kleenex-mode-map (make-sparse-keymap)))
    (define-key kleenex-mode-map "\C-j" 'newline-and-indent)
    kleenex-mode-map)
  "Keymap for Kleenex major mode")

(add-to-list 'auto-mode-alist '("\\.kex\\'" . kleenex-mode))


(defconst kleenex-font-lock-keywords-1
  (list
   '("<\\([^<>]*\\)>" . 1)
   '("\"[^\"]*\"" . font-lock-string-face)
   '("\\<\\([a-z][a-zA-Z0-9_-]*\\)\\>" . font-lock-function-name-face)
   )
  "Minimal highlighting expressions for Kleenex mode")

(defvar kleenex-font-lock-keywords kleenex-font-lock-keywords-1
  "Default highlighting expressions for Kleenex mode")

(defvar kleenex-mode-syntax-table
  (let ((kleenex-mode-syntax-table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" kleenex-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" kleenex-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" kleenex-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" kleenex-mode-syntax-table)
    (modify-syntax-entry ?< "(>" kleenex-mode-syntax-table)    
    (modify-syntax-entry ?> ")<" kleenex-mode-syntax-table)
    kleenex-mode-syntax-table)
  "Syntax table for kleenex-mode")
        

(defun kleenex-mode ()
  "Major mode for editing Kleenex Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table kleenex-mode-syntax-table)
  (use-local-map kleenex-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(kleenex-font-lock-keywords))
  (setq major-mode 'kleenex-mode)
  (setq mode-name "Kleenex")
  (run-hooks 'kleenex-mode-hook))

(provide 'kleenex-mode)
