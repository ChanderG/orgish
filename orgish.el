(defun orgish/save-buffer (buffer file)
  (with-current-buffer (org-html-export-as-html)
    ; insert our source org stuff in
    (insert "<!-- org\n")
    (insert-buffer-substring buffer)
    (insert "\n")
    (insert "org -->\n")
    (write-region nil nil file))
  t
  )

(defun orgish/open (filename)
  (interactive)
  (if (file-exists-p filename)
    ()
    ; doesn't exist - create a new buffer
    (progn
      (find-file-literally (concat filename "-orgish"))
      (org-mode)
      (setq org-export-show-temporary-export-buffer nil)
      (add-hook 'write-contents-functions (apply-partially 'orgish/save-buffer (buffer-name) filename))
      )))
