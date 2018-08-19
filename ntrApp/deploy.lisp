;; (defun runapp ()
;;   "run app"
;;   (interactive)
;;   (shell-command "Rscript ~/Git-work/traume/ntrApp/deployApp.R")
;;   )

;; (global-set-key (kbd "C-c s") 'runapp)


;; Bedre måte
(setq deploy-file "deployApp.R")

(defvar app-path
  (expand-file-name deploy-file (file-name-directory (buffer-file-name))))

(defun run-app ()
  "run shiny"
  (interactive)
  (shell-command-to-string (format "Rscript  %s" app-path)))

(global-set-key (kbd "C-c s") 'run-app)
