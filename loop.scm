(softmodule
  public: ((map-loop-string map-loop)
           (fold-loop-string fold-loop)
           (loop-string-function loop-function))

  private: ((loop-input-size
             (lambda (x)
               (cond ((ast-arg? x) (ast-arg-size x))
                     ((ast-list? x) (ast-list-size x))
                     ((ast-constant? x) (ast-constant-size x))
                     ((ast-call? x) (loop-input-size (car (ast-call-args x))))
                     ((ast-shared? x) (ast-shared-size x))
                     ((ast-variable? x) (ast-variable-size x))
                     (else (not-glist-error x loop-input-size)))))

            (loop-input-name
             (lambda (x)
               (cond ((ast-list? x) (ast-list-name x))
                     ((ast-arg? x) (ast-arg-name x))
                     ((ast-constant? x) (ast-constant-name x))
                     ((ast-shared? x) (ast-shared-name x))
                     ((ast-call/map? x) (map-operation-name (ast-call-operation x)))
                     ((ast-variable? x) (ast-variable-name x))
                     ((ast-call/user? x) (user-function-name (ast-call-operation x)))
                     (else (not-glist-error x loop-input-name)))))

            (loop-inputs
             (lambda (call)
               (map (lambda (x)
                      (ast-literal (conc (loop-input-name x) "_idx") (ast-type x)))
                    (ast-call-args call))))

            (element-type
             (lambda (type)
               (let ((c (lambda (x) (delete-substring "constant " x)))
                     (g (lambda (x) (delete-substring "global " x)))
                     (p (lambda (x) (delete-substring "*" x))))
                 ((o c g p) type))))

            (loop-input-declare
             (lambda (call expand)
               (substrings->string (map (lambda (x)
                                          (let* ((type (ast-type x))
                                                 (name (loop-input-name x))
                                                 (fmt (if (and (ast-arg? x) (scope-arg? x))
                                                          "~a ~a_idx=~a;"
                                                          "~a ~a_idx=~a[idx];")))
                                            (format fmt (element-type type) name (expand x))))
                                        (ast-call-args call)))))

            (map-loop
             (lambda (call expand)
               (let* ((op (ast-call-operation call))
                      (type (map-operation-type op))
                      (func (map-operation-procedure op))
                      (rtn (map-operation-name op))
                      (size (loop-input-size call))
                      (declare (loop-input-declare call expand))
                      (args (loop-inputs call))
                      (call-string (expand (invoker->ast-call func args)))
                      (size-check (format "if(~a>0)" size))
                      (for-loop (format "for(int idx=0;idx<~a;idx++)" size))
                      (statement (format "{~a~a[idx]=~a;}" declare rtn call-string)))
                 (if (number? size)
                     (if (> size 0) (conc for-loop statement) "")
                     (conc size-check "{" for-loop statement "}" )))))

            (fold-loop
             (lambda (call expand)
               (if (fold-operation-left? (ast-call-operation call))
                   (fold-left-loop call expand)
                   (fold-right-loop call expand))))

            (fold-right-loop
             (lambda (call expand)
               (fold-loop-internal call expand "for(int idx=~a;idx>=0;idx--)")))

            (fold-left-loop
             (lambda (call expand)
               (fold-loop-internal call expand "for(int idx=0;idx<~a;idx++)")))

            (fold-loop-internal
             (lambda (call expand cond)
               (let* ((op (ast-call-operation call))
                      (func (fold-operation-procedure op))
                      (start (expand (fold-operation-start op)))
                      (type (fold-operation-type op))
                      (rtn (fold-operation-name op))
                      (size (loop-input-size call))
                      (declare (loop-input-declare call expand))
                      (inputs (loop-inputs call))
                      (scope-inputs (filter scope-arg? inputs))
                      (normal-inputs (remove scope-arg? inputs))
                      (args (flatlist normal-inputs (ast-literal rtn type) scope-inputs))
                      (call-string (expand (invoker->ast-call func args)))
                      (declare-rtn (format "~a ~a=~a;" type rtn start))
                      (size-check (format "if(~a>0)" size))
                      (for-loop (format cond size))
                      (statement (format "{~a~a=~a;}" declare rtn call-string)))
                 (if (number? size)
                     (if (> size 0)
                         (conc declare-rtn for-loop statement)
                         declare-rtn)
                     (conc declare-rtn size-check "{" for-loop statement "}")))))

            (loop-function
             (lambda (call)
               (let ((op (ast-call-operation call)))
                 (cond ((map-operation? op) map-loop)
                       ((fold-operation? op) fold-loop))))))
  )
