;; based on
;; https://gist.github.com/tenpn/2367513

;; ;; eg when in a text file, (execute-shell-command-on-buffer "p4 edit %s") will check out the file in perforce
;; (defun execute-shell-command-on-buffer (shell-command-text)
;;   (interactive "MShell command:")
;;   (shell-command (format shell-command-text (shell-quote-argument buffer-file-name)))
;;   )


;; runKnitr-2.sh is my script
(defun run-knit-synctex-on-buffer () 
  (interactive)
  (shell-command (format "runKnitr-2.sh %s" (shell-quote-argument buffer-file-name)))
  )

; will bind to M-n r, substituting ess-swv-knit
