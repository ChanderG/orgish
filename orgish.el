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
  "Extract out the source org from the html file into the buffer."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (search-forward "<!-- org\n")
    (let* ((beg (point))
           (end (progn
                  (search-forward "org -->\n")
                  (forward-line -1)
                  (forward-char -1)
                  (point)))
           (sbuf (current-buffer)))
        (with-current-buffer buffer
          (insert-buffer-substring sbuf beg end)
          )))
  t)

(defun orgish/open (filename)
  "Single entry point to open/create a new orgish file."
  (interactive "FEnter file name: ")
  (let* ((bufname (concat "*" (file-name-nondirectory filename) "-orgish" "*"))
        (buf-already-exists (get-buffer bufname)))
    (switch-to-buffer bufname)
    (if (not buf-already-exists)
        (orgish/setup filename))))

(defun orgish/setup (filename)
  "Setup the current buffer as orgish with filename as the backing file.

   Should never be called directly. Should only be called by orgish/open."
  ; check if we are opening an existing orgish file
  (if (file-exists-p filename)
    (if (not (ignore-errors (orgish/extract-source filename (current-buffer))))
        ; some errors in parsing
        (progn
          (kill-buffer)
          (throw 'invalid-orgish-file "Source file does not seem to be orgish. Refusing to continue!"))
      ))
  ; do the normal processing here
  (org-mode)
  (setq org-export-show-temporary-export-buffer nil)
  ; unset the file connection - since this is not a normal buffer
  (setq buffer-file-name nil)
  ; autosave on focus out
  (add-hook 'focus-out-hook 'save-buffer 0 t)
  (add-hook 'write-contents-functions (apply-partially 'orgish/save-buffer (buffer-name) filename)))

(defun orgish/check ()
  "Check if the current file is orgish and upgrade the view to orgish."
  (if (equal (buffer-substring-no-properties 1 9) "<!-- org")
      (let ((srcfile (buffer-file-name)))
        (kill-buffer)
        (orgish/open srcfile))
  ))

(add-hook 'html-mode-hook 'orgish/check)
