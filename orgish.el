;; -*- lexical-binding:t -*-

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

(defun orgish/extract-source (file buffer)
  "Extract out the source org from the html file."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (search-forward "<!-- org\n")
    (let* ((beg (point))
           (end (progn
                  (search-forward "org -->\n")
                  (forward-line -1)
                  (point)))
           (sbuf (current-buffer)))
        (with-current-buffer buffer
          (insert-buffer-substring sbuf beg end)
          )))
  t)

(defun orgish/open (filename)
  (interactive)
  ; open this file - which shouldn't exist in the first place
  ; so a new fresh buffer should be created
  (find-file-literally (concat "*" filename "-orgish" "*"))
  ; check if we are opening an existing orgish file
  (if (file-exists-p filename)
    (if (not (ignore-errors (orgish/extract-source filename (current-buffer))))
        ; some errors in parsing
        (error "Source file does not seem to be orgish. Refusing to continue!")))
  ; do the normal processing here
  (org-mode)
  (setq org-export-show-temporary-export-buffer nil)
  ; unset the file connection - since this is not a normal buffer
  (setq buffer-file-name nil)
  ; autosave on focus out
  (add-hook 'focus-out-hook 'save-buffer 0 t)
  (add-hook 'write-contents-functions (apply-partially 'orgish/save-buffer (buffer-name) filename)))
