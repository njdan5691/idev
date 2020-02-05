;;; idev.el --- INC development elisp

;; Copyright (C) 2013-2017 Daniel Nuzzo
;;
;; Author: Daniel <dnuzzo@lightriver.com> and contributors
;; Package-Requires: ((cl-lib "0.6.1"))
;; Version: 0.1


(require 'cl-lib)
(require 'dired)
(require 'bind-key)

;;;###autoload                                                                                      
(defun idev:inc-files ()
  (interactive)
  (ivy-read "file:" (process-lines "incfiles")
            :action (lambda (x)
                      (find-file x)))
            :caller 'idev:inc-files)  

;;;###autoload
(defun idev:base-files ()
  (interactive)
  (ivy-read "file:" (process-lines "basefiles")
            :action (lambda (x)
                      (find-file x)))
  :caller 'idev:base-files)


;;;###autoload
(defun idev:sget ()
	(interactive)
	(ivy-read "sget file:" (process-lines "vls")
						:action (lambda (x)
											(shell-command (format "sg %s" x)))
						:caller 'idev:sget))
;;;###autoload
(defun idev:sget-project-files ()
	(interactive)
	(ivy-read "sget file:" (process-lines "vfiles")
						:action (lambda (x)
											(shell-command (format "getfile %s" x)))
						:caller 'idev:sget-project-files))

;;;###autoload
(defun idev:file-history-old ()
	(interactive)
	(ivy-read "history for:" (process-lines "vfiles")
						:action (lambda (x)
											(shell-command (format "file-history %s" x)))
						:caller 'idev:file-history))

;;;###autoload
(defun idev:file-history ()
	""
	(interactive)
	(let* ((gen (read-string "Gen:" (getenv "sabGEN")))
				 (path (ivy-read "File:" (process-lines "vfiles")))
				 (diff-buffer-name (format "%s-%s.patch" gen path)))
		(shell-command (format "file-history %s %s" gen path) diff-buffer-name)
		(pop-to-buffer diff-buffer-name)
		(diff-mode)))

;;;###autoload
(defun idev:list-mrs (status &optional dev)
	(if dev
			(sort (process-lines "idev-mrlist" "-d" dev "-s" status "-g" (getenv "sabGEN")) 'string>)
		(sort (process-lines "idev-mrlist" "-s" status "-g" (getenv "sabGEN")) 'string>)))

;;;###autoload
(defun idev:list-all-mrs (&optional dev)
	(if dev
			(sort (process-lines "idev-mrlist" "-d" dev "-g" (getenv "sabGEN")) 'string>)
		(sort (process-lines "idev-mrlist" "-g" (getenv "sabGEN")) 'string>)))


;;;###autoload
(defun idev:mr-action (action &optional args)
	(let* ((command (if args (concat "idev-mrlist " args) "idev-mrlist"))
				 (command-args (split-string command "\\s-+")))
		(ivy-read  "MR:" (sort (apply 'process-lines command-args) 'string>)
							 :action action
							 :caller 'idev:mr-action)))
;;;###autoload
(defun idev:set-mr (mr)
	(setenv "mr" mr)
	(process-file-shell-command (concat "echo " mr " >~/.dotfiles/mr")))

;;;###autoload
(defun idev:from-line (line)
	(nth 0 (split-string line "\\s-+")))

;;;###autoload
(defun idev:choose-mr ()
	(interactive)
	(let ((def-args (format "-d dan -g %s" (getenv "sabGEN"))))
		(idev:mr-action (lambda (x) (idev:set-mr (idev:from-line x))) def-args)))

;;;###autoload
(defun idev:show-off ()
	(interactive)
	(message (format "OFF=%s" (getenv "OFF"))))

;;;###autoload
(defun idev:get-mr-number (my-string)
	"Match the MR number if sable command output"
	(message "Asdfasdfasdf")
	(let ((my-regexp "\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]"))
		(and (string-match my-regexp my-string)
				 (list (match-string 1 my-string) (match-string 3 my-string)))))

;;;###autoload
(defun idev:edit-string (value)
  "Edits string and returns it"
  (let ((this-buffer (buffer-name))
        (new-value value)
        (buffy "*edit-string*"))
    (save-excursion
      (switch-to-buffer buffy)
      (set-buffer buffy)
      (text-mode)
      (local-set-key (kbd "C-c C-c") 'exit-recursive-edit)
      (if (stringp value) (insert value))
      (message "When you're done editing press C-c C-c or C-M-c to continue.")
      (unwind-protect
          (recursive-edit)
        (if (get-buffer-window buffy)
            (progn
              (setq new-value (buffer-substring (point-min) (point-max)))
              (kill-buffer buffy))))
      (switch-to-buffer this-buffer)
      new-value)))


;;;###autoload
(defun idev:fcreate ()
	"Create MR"
	(interactive)
	(let* ((rel (read-string "Release Detected: " (getenv "sabGEN")))
				 (sev (read-string "Request Severity:" "2"))
				 (class (read-string "MRG Class:" "software"))
				 (g (read-string "Generic:" (getenv "sabGEN")))
				 (sys (read-string "Product System:" "core"))
				 (abst (read-string "Abstract:" "DEV:"))
				 (qabst (shell-quote-argument abst))
				 ;;(desc (read-string "Description:" abst))
				 (desc (idev:edit-string (idev:string-from-file "/home/dan/var/def-mr-description")))
				 (base-cmd (format "%s/fcreate prompt=n site=\"LightRiver Software\" pd=development cat=dev_found" (getenv "sabLCB")))
				 (desc-file (make-temp-file "sablime"))
				 (fcreate-format "%s rel=%s sev=%s class=%s g=%s sys=%s abst=%s desc=%s")
				 (fcreate-command (format fcreate-format base-cmd rel sev class g sys qabst desc-file)))
		(write-region desc nil desc-file t)
		(set-file-modes desc-file #o644)
		(shell-command fcreate-command)
		(delete-file desc-file)))

;;;###autoload
(defun idev:freeze-project ()
	"Select an Active Project to freeze."
	(interactive)
	(ivy-read "Project:" (process-lines "inc-projects")
						:action (lambda (x) (shell-command (format "freeze %s" x)))
						:caller 'idev:freeze-project))

;;;###autoload
(defun idev:unfreeze-project ()
	"Select a Frozen Project to unfreeze."
	(interactive)
	(ivy-read "Project:" (process-lines "frozen")
						:action (lambda (x) (shell-command (format "unfreeze %s" x)))
						:caller 'idev:unfreeze-project))

;;;###autoload
(defun idev:toggle-tab-width ()
	(interactive)
	(cond ((= 8 tab-width) (setq tab-width 1))
				((= 1 tab-width) (setq tab-width 2))
				((= 2 tab-width) (setq tab-width 4))
				((= 4 tab-width) (setq tab-width 6))
				((= 6 tab-width) (setq tab-width 8))
				))



;;;###autoload
(defun idev:inc-dir ()
  "Select directory under dir"
  (interactive)
  (let ((dir (ivy-read "Top Dir:" (split-string (getenv "VPATH") ":"))))
    (ivy-read "Dir:" (process-lines "fd" "--type" "d" "-d" "2" "." dir))))

;;;###autoload
(defun idev:inc-grep (&optional dir)
  "Search for a pattern in $OFF directory using ag.
    INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((dir (idev:inc-dir)))
    (counsel-rg (thing-at-point 'symbol) dir "" "Search:")))


;;;###autoload
(defun idev:find-at-point ()
	(interactive)
	(counsel-grep-or-swiper (format "\\<%s\\>" (thing-at-point 'symbol))))

;;;###autoload
(defun idev:ask-emacs (&optional initial-input)
	"Search for a pattern in emacs 'info/' directory using ag.
    INITIAL-INPUT can be given as the initial minibuffer input."
	(interactive)
	(counsel-ag initial-input (car Info-default-directory-list)
							" -z" "Search emacs/elisp info"))


;; Version of ivy-completing-read that does not sort
;;
;;;###autoload
(defun idev:ivy-completing-read (&rest args)
  (let ((ivy-sort-functions-alist '((t . nil))))
    (apply 'ivy-completing-read args)))


;;;###autoload
(defun idev:change-generic ()
	"Change the generic the Project is in"
	(interactive)
	(let* ((gen (idev:ivy-completing-read "Gen:" (process-lines "inc-generics") nil t)))
		(process-file-shell-command (concat "echo " gen " >" (getenv "BASE") "/.gen"))))



;;;###autoload
(defun idev:submit (mrinfo g reso)
  "Submit an Mr"
  (interactive (list
                (idev:ivy-completing-read "Mr:" (idev:list-mrs "assigned" "dan") nil t)
                (read-string "Generic:" (getenv "sabGEN"))
                (elispm:simplified-read-mb-lines-def "" (idev:string-from-file "/home/dan/var/def-mr-resolution"))))
  (let* ((rfile (make-temp-file "sablime"))
         (parts (split-string mrinfo " "))
         (mr (nth 0 parts)))
    (write-region reso nil rfile)
    (set-file-modes rfile #o644)
    (shell-command (concat  (getenv "sabLCB") "/submit prompt=n "
                            (format "mr=%s g=%s rfile=%s" mr g rfile)))
    (delete-file rfile)))






;;;###autoload
(defun idev:mr-command (mrinfo command)
	"Print MR Report"
	(interactive
	 (let ((dev (idev:ivy-completing-read "Dev:" (process-lines "developers") nil nil "dan")))
		 (list
			(idev:ivy-completing-read "Mr:" (idev:list-all-mrs dev) nil t)
			(idev:ivy-completing-read "Command:" (process-lines "mr-commands") nil nil "rep"))))
	(let* ((parts (split-string mrinfo " "))
				 (mr (nth 0 parts))
				 (mr-buffer-name (format "%s.txt" mr)))
		(shell-command (format "%s %s" command mr) mr-buffer-name "*mr*")
		(pop-to-buffer mr-buffer-name)
		(text-mode)))

;;;###autoload
(defun idev:reformat-c-buffer()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))

;;;###autoload
(defun idev:indent-c-buffer()
  (interactive)
  (setq sh-indent-command (concat
                           "indent -st "
                           buffer-file-name)))

;;;###autoload
(defun idev:indent-json-buffer()
  (interactive)
  (setq sh-indent-command (concat
                           "jq . "
                           buffer-file-name)))


;;;###autoload
(defun idev:make-name-from-desc (desc)
	(downcase (replace-regexp-in-string "[[:space:]]+" "-" desc)))

;;;###autoload
(defun idev:remove-newline (s)
	(if (string-match "[\t\n\r]+\\'" s)
			(replace-match "" t t s) s))


;;
;;;###autoload
(defun idev:string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;;###autoload
(defun idev:sh-oneliner (command)
	(let ((output (shell-command-to-string command)))
		(if (string-match "[\t\n\r]+\\'" output)
				(replace-match "" t t output))))
;;;###autoload
(defun idev:switch-project (gen basename base)
  (let ((node (format "/inc/%s/%s" gen basename)))
    (setenv "OFF" node)
    (setenv "sabGEN" gen)
    (setenv "GTAGSDBPATH" (concat node "/xref/gtags"))
    (setenv "GTAGSROOT" node)
    (if (string-match "_patch" node)
        (let* ((parts (split-string node "_patch"))
               (off (nth 0 parts)))
	  (setenv "PATCHOFF" node)
          (setenv "OFF" off)
          (setenv "GTAGSDBPATH" (concat off "/xref/gtags"))
          (setenv "GTAGSROOT" off)
          (setenv "GTAGSLIBPATH" (format "%s/xref/gtags" node))))
    (setenv "BASE" base)
    (setenv "BUILDNODE" base)
    (setenv "CDPATH" (format ":.:%s" base))
    (process-file-shell-command (concat "echo " node " >~/.INC-node"))
    (process-file-shell-command (concat "echo " base " >~/.dotfiles/basenode"))
    (setenv "VPATH" (idev:sh-oneliner "vpath"))
    (find-file base)))



;;;###autoload
(defun idev:latest-node (gen)
	(let ((node (shell-command-to-string (format "latest-node %s" gen))))
		(if (string-match "[\t\n\r]+\\'" node)
				(replace-match "" t t node))))

;;;###autoload
(defun idev:project-generic (proj)
	(let ((genfile (format "~/inc/%s/.gen" proj)))
		(if (file-exists-p genfile)
				(let ((s (idev:string-from-file genfile)))
					(if (string-match  "[\t\n\r]+\\'" s)
							(replace-match "" t t s))) "inc35.1")))

;;;###autoload
(defun idev:select-project()
	"Select node to work in."
	(interactive)
	(let* ((proj (ivy-read "Porject:" (process-lines "inc-projects")
												 :caller 'idev:select-project))
				 (gen (idev:project-generic proj))
				 (node (idev:latest-node gen))
				 (parts (split-string node "/"))
				 (basename (nth 3 parts)))
		(idev:switch-project gen basename (expand-file-name (format "~/inc/%s" proj)))))

(defun idev:remove-o (file-list)
	(cl-remove-if-not (lambda (s) (not (string-match "\.o$\\|\.ms$\\|\.orig$\\|^save." s))) file-list))


(defun idev:dired-sdiff ()
	"Run sdiff command on file and display output in shell buffer."
	(interactive)
	(shell-command (format "sdiff %s" (file-name-nondirectory (dired-get-file-for-visit)))))

(defun idev:dired-edget ()
	"Run edget for the current dired file and display output in shell buffer."
	(interactive)
	(shell-command (format "eg %s" (file-name-nondirectory (dired-get-file-for-visit)))))

(defun idev:dired-edput ()
	"Run edput for the current dired file and display output in shell buffer."
	(interactive)
	(shell-command (format "ep %s" (file-name-nondirectory (dired-get-file-for-visit)))))

(defun idev:dired-sget ()
	"Select a file to sget"
	(interactive)
	(ivy-read "sget ile:" (process-lines "vfiles")
						:action (lambda (x)
											(shell-command (format "getfile %s" x)))
						:caller 'sablime-sget-project-files))


(defun idev:make-project ()
	"Create a new Project"
	(interactive)
	(let* ((gen (idev:ivy-completing-read "Gen:" (process-lines "inc-generics") nil t))
				 (desc (read-string "Description:"))
				 (node (idev:remove-newline (shell-command-to-string (format "latest-node %s" gen))))
				 (parts (split-string node "/"))
				 (basename (nth 3 parts))
				 (base (expand-file-name (format "~/inc/%s" (idev:make-name-from-desc desc)))))
		(process-file-shell-command (format "my-setnode %s" base))
		(process-file-shell-command (concat "echo " gen " >" base "/.gen"))
		(idev:switch-project gen basename base)))



(provide 'idev)
;;; idev.el ends here
