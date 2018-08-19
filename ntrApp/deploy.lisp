(defun runapp ()
  "run app"
  (interactive)
  (shell-command "Rscript ~/Git-work/traume/ntrApp/deployApp.R")
  )

(global-set-key (kbd "C-c s") 'runapp)
