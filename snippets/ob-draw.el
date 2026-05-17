;;; lisp/ob-draw.el -*- lexical-binding: t; -*-

(defvar fanshi/org-babel-draw-models
  '((z . "z_image_1.0_f16.ckpt") ;;  "Tongyi-MAI/Z-Image"
    (ernie . "ernie_image_f16.ckpt")
    (qwen-edit . "Qwen/Qwen-Image-Edit-2511")
    (ltx . "Lightricks/LTX-2.3"))
  "List of image generation models for draw blocks.
The :model header arg can be a number (index into this list) or a string (model name)."
  )

(defvar fanshi/drawthings-cli-args-alist
  '((:model . 'special)
    (:width) (:height) ;; NOTE: width & height need to be multiple of 64
    (:strength) (:seed) (:steps) (:cfg) (:img . "image")
    (:neg . (lambda (neg)
              (format " --%s %s"
                      (if (and (stringp neg) (file-readable-p neg))
                          "negative-prompt-file"
                        "negative-prompt")
                      (shell-quote-argument neg)))))
  ""
  )

(defvar org-babel-default-header-args:draw
  '((:model . 0)
    (:turbo . t)
    (:results . "file")
    (:file-ext . "png"))
  "Default header args for draw blocks.")

(defun org-babel-execute:draw (body params)
  "Execute a draw prompt block.
BODY is the prompt text. PARAMS is an alist of header arguments."
  (let* ((params (org-babel-merge-params org-babel-default-header-args:draw params))
         (model-raw (alist-get :model params))
         (model (cond ((numberp model-raw) (cdr (nth model-raw fanshi/org-babel-draw-models)))
                      ((and (symbolp model-raw) (alist-get model-raw fanshi/org-babel-draw-models)))
                      ((and (stringp model-raw) (alist-get (intern model-raw) fanshi/org-babel-draw-models)))
                      (t model-raw)))
         (model (if (assq :turbo params)
                    (pcase model-raw
                      (z  "z_image_turbo_1.0_f16.ckpt")
                      (ernie "ernie_image_turbo_f16.ckpt")
                      (_ model))
                  model))
         (file-ext (alist-get :file-ext params))
         (out-file (or (alist-get :file params) (concat (make-temp-name "draw-") "." file-ext)))
         (prompt (replace-regexp-in-string "\n" " " body))
         ;; Build optional CLI arguments
         (cli-args (format "--model %s --offline --disable-preview --prompt \"%s\" --output %s"
                           (shell-quote-argument model)
                           (shell-quote-argument prompt)
                           (shell-quote-argument out-file))))
    (dolist (arg fanshi/drawthings-cli-args-alist)
      (when-let* ((header-arg (car arg))
                  (cli-arg (or (cdr arg) (string-remove-prefix ":" (symbol-name header-arg))))
                  (value (alist-get header-arg params)))
        (setq cli-args
              (concat cli-args
                      (cl-typecase cli-arg
                        (string (format (concat " --" cli-arg " %s") value))
                        (interpreted-function (cli-arg value))
                        (symbol "")
                        ;; (symbol (if (not (eq (symbol-name cli-arg) "special")) (error "fanshi/drawthings-cli-args-alist: only symbol special is supported for cli-arg") t))
                        ;; (t (error "fanshi/drawthings-cli-args-alist: unsupported type of cli-arg(%s): %s" header-arg cli-arg))
                        (t ""))))))


    ;; (let* ((cmd (format "draw-things-cli generate %s" cli-args))
    ;;        (log-buf (get-buffer-create (concat "*" out-file "-output-buffer*"))))
    ;;   (if (call-process-shell-command cmd nil log-buf)
    ;;       (concat "[[file:" out-file "]]\n")
    ;;     (with-current-buffer log-buf (buffer-string))))
    (fanshi/omlx-list-loaded-model 'unload-all)
    (call-process-shell-command (format "draw-things-cli generate %s" cli-args) nil 0)
    out-file))

(provide 'ob-draw)
