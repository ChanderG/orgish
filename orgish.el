(defun orgish/save-buffer (buffer file)
  (with-current-buffer (org-html-export-as-html)
    ; insert our source org stuff in
    (insert "<!-- org\n")
    (insert-buffer-substring buffer)
    (insert "\n")
    (insert "org -->\n")
    (write-region nil nil file))
  ; notify to emacs that the file has no more changes
  ; this is needed to ensure that this buffer doesn't come up in save-some-buffers group
  (with-current-buffer buffer
    (set-buffer-modified-p nil))
  t
  )

(defun orgish/open (filename)
  (interactive)
  (if (file-exists-p filename)
    ()
    ; doesn't exist - create a new buffer
    (progn
      (find-file-literally (concat "*" filename "-orgish" "*"))
      (org-mode)
      (setq org-export-show-temporary-export-buffer nil)
      ; setup the filename of the buffer to point to the source html file
      ; can be nil, but you can accidently close the buffer without saving
      (setq buffer-file-name filename)
      (add-hook 'write-contents-functions (apply-partially 'orgish/save-buffer (buffer-name) filename))
      )))
