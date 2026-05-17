;;; lisp/ob-draw.el -*- lexical-binding: t; -*-

(defvar fanshi/org-babel-draw-models
  '((z . "z_image_1.0_f16.ckpt")
    (ernie . "ernie_image_f16.ckpt")
    (flux . "flux_2_dev_f16.ckpt")
    (qwen-edit . "Qwen/Qwen-Image-Edit-2511")
    (ltx . "Lightricks/LTX-2.3"))
  "List of image generation models for draw blocks.
The :model header arg can be a number (index into this list) or a string (model name)."
  )

(defvar fanshi/mflux-models
  '((z . "z-image")
    (flux2 . "flux2")
    (flux2-9b . "flux2")
    (fibo . "fibo")
    (qwen . "qwen"))
  "List of mflux model presets.
Each entry maps a symbol to the mflux CLI subcommand suffix.")

(defvar fanshi/mlx-video-models
  '((ltx . "ltx_2")
    (wan . "wan_2"))
  "List of mlx-video model families.
Each entry maps a symbol to the mlx-video module name.")

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

(defvar fanshi/mflux-cli-args-alist
  '((:width) (:height)
    (:seed) (:steps) (:cfg . "guidance") (:img . "image")
    (:neg . "negative-prompt") (:q . "quantize"))
  "CLI argument mapping for mflux backend.")

(defvar fanshi/mlx-video-cli-args-alist
  '((:width) (:height) (:seed) (:steps)
    (:cfg . "cfg-scale") (:pipeline) (:model-repo)
    (:num-frames) (:fps)
    (:neg . "negative-prompt"))
  "CLI argument mapping for mlx-video backend.")

(defvar org-babel-default-header-args:draw
  '((:backend . "draw-things")
    (:model . "z")
    (:turbo . t)
    (:dry-run . nil))
  "Default header args for draw blocks.")

(defun fanshi/draw-build-cli-args (cli-args-alist params)
  "Build CLI argument string from CLI-ARGS-ALIST and PARAMS."
  (let ((args ""))
    (dolist (arg cli-args-alist)
      (when-let* ((header-arg (car arg))
                  (cli-arg (or (cdr arg) (string-remove-prefix ":" (symbol-name header-arg))))
                  (value (alist-get header-arg params)))
        (setq args
              (concat args
                      (cl-typecase cli-arg
                        (string (format (concat " --" cli-arg " %s") value))
                        (interpreted-function (cli-arg value))
                        (symbol "")
                        (t ""))))))
    args))

(defun fanshi/draw-backend-draw-things-build-command (out-file prompt params)
  "Build the CLI command string for draw-things-cli backend generation.
OUT-FILE is the output file path.  PROMPT is the prompt text.  PARAMS
is an alist of header arguments.

Returns the full command string."
  (let* ((model-raw (alist-get :model params))
         (model (cond
                 ((and (symbolp model-raw) (alist-get model-raw fanshi/org-babel-draw-models)))
                 ((and (stringp model-raw) (alist-get (intern model-raw) fanshi/org-babel-draw-models)))
                 (t model-raw)))
         (model (if (alist-get :turbo params)
                    (pcase model-raw
                      ((or 'z "z") "z_image_turbo_1.0_f16.ckpt")
                      ((or 'ernie "ernie") "ernie_image_turbo_f16.ckpt")
                      (_ model))
                  model))
         (cli-args (format "--model %s --offline --disable-preview --prompt \"%s\" --output %s"
                           (shell-quote-argument model)
                           (shell-quote-argument prompt)
                           (shell-quote-argument out-file))))
    (format "draw-things-cli generate %s %s" cli-args (fanshi/draw-build-cli-args fanshi/drawthings-cli-args-alist params))))

(defun fanshi/draw-backend-mflux-build-command (out-file prompt params)
  "Build the CLI command string for mflux backend generation.
OUT-FILE is the output file path.  PROMPT is the prompt text.  PARAMS
is an alist of header arguments.

Returns the full command string."
  (let* ((model-raw (alist-get :model params))
         (model-suffix (cond ((symbolp model-raw) (alist-get model-raw fanshi/mflux-models))
                             ((stringp model-raw) (alist-get (intern model-raw) fanshi/mflux-models))
                             (t (error "mflux: unsupported :model value %s" model-raw))))
         (model-suffix (concat model-suffix
                               (when (alist-get :turbo params)
                                 (pcase model-raw
                                   ((or 'z "z") "-turbo")
                                   (_ nil)))))

         (cli-args (format "--prompt \"%s\" --output %s"
                           (shell-quote-argument prompt)
                           (shell-quote-argument out-file))))
    (format "mflux-generate-%s %s %s" model-suffix cli-args (fanshi/draw-build-cli-args fanshi/mflux-cli-args-alist params))))

(defun fanshi/draw-backend-mlx-video-build-command (out-file prompt params)
  "Build the CLI command string for mlx-video backend generation.
OUT-FILE is the output file path.  PROMPT is the prompt text.  PARAMS
is an alist of header arguments.

Returns the full command string."
  (let* ((model-raw (alist-get :model params))
         (module (cond ((symbolp model-raw) (alist-get model-raw fanshi/mlx-video-models))
                       ((stringp model-raw) (alist-get (intern model-raw) fanshi/mlx-video-models))
                       (t (error "mlx-video: unsupported :model value %s" model-raw))))
         (output-flag (if (string-prefix-p "wan" module) "--output-path" "--output"))
         (cli-args (format "--prompt \"%s\" %s %s"
                           (shell-quote-argument prompt)
                           output-flag
                           (shell-quote-argument out-file))))
    (format "python -m mlx_video.%s.generate %s %s" module cli-args (fanshi/draw-build-cli-args fanshi/mlx-video-cli-args-alist params))))

(defun org-babel-execute:draw (body params)
  "Execute a draw prompt block.
BODY is the prompt text. PARAMS is an alist of header arguments."
  (let* ((backend (alist-get :backend params))
         (result-params (alist-get :result-params params))
         (file-ext (or (alist-get :file-ext params)
                       (pcase backend
                         ("mlx-video" "mp4")
                         (_ "png"))))
         (out-file (or (alist-get :file params) (concat (make-temp-name "draw-") "." file-ext)))
         (cmd (pcase backend
                ("mflux" (fanshi/draw-backend-mflux-build-command out-file body params))
                ("mlx-video" (fanshi/draw-backend-mlx-video-build-command out-file body params))
                (_ (fanshi/draw-backend-draw-things-build-command out-file body params)))))
    (if (alist-get :dry-run params)
        (if (and  (member "value" result-params)
                  (or
                   (member "scalar" result-params)
                   (member "verbatim" result-params)))
            cmd
          (user-error "Draw aborted. for :dry-run, please set the :results to value + scalar/verbatim."))
      (if (member "file" result-params)
          (progn
            (fanshi/omlx-list-loaded-model 'unload-all)
            (call-process-shell-command cmd nil 0)
            out-file)
        (user-error "Draw aborted. Please set the :results to file")))))

(provide 'ob-draw)
