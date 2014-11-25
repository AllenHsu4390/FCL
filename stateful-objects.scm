(define user-functions '())

(define global-constants '())

(define record-types '())

(define buffers '())

(define (add-constant c)
  (set! global-constants (append global-constants (list c))))

(define (add-user-function f)
  (set! user-functions (append user-functions (list f))))

(define (add-record-type r)
  (set! record-types (append record-types (list r))))

(define (add-buffer buffer)
  (set! buffers (append buffers (list buffer))))

(define (remove-constant c)
  (remove! (lambda (x)
             (string=? (ast-constant-name x)
                       (ast-constant-name c)))
           global-constants))

(define (remove-user-function f)
  (remove! (lambda (x)
             (string=? (user-function-name x)
                       (user-function-name f)))
           user-functions))

(define (remove-record-type r)
  (remove! (lambda (x)
             (string=? (grecord-type-alias x)
                       (grecord-type-alias r)))
           record-types))

(define (remove-buffer b)
  (remove! (lambda (x)
             (string=? (ast-shared-name b)
                       (ast-shared-name x)))
           buffers))

(define (reset-stateful-objects)
  (set! global-constants '())
  (set! user-functions '())
  (set! record-types '())
  (set! buffers '()))

(softmodule

  public: ((order-counter (new-counter 0))
           (assign-variable-name next-variable)
           (assign-glist-name next-glist)
           (assign-scope-arg next-scope-arg)
           (assign-shared-name next-shared)
           (assign-functor-name next-functor)
           (assign-struct-id struct-id-counter)
           (assign-glambda-name next-glambda))

  private: ((glist-counter (new-counter 0))
            (functor-counter (new-counter 0))
            (glambda-counter (new-counter 0))
            (shared-counter (new-counter 0))
            (gvariable-counter (new-counter 0))
            (scope-counter (new-counter 0))
            (struct-id-counter (new-counter 0))

            (next-shared
             (lambda ()
               (conc "shared" (shared-counter))))

            (next-scope-arg
             (lambda ()
               (conc "arg_s" (scope-counter))))

            (next-variable
             (lambda ()
               (conc "var" (gvariable-counter))))

            (next-glist
             (lambda ()
               (conc "list" (glist-counter))))

            (next-glambda
             (lambda ()
               (conc "lambda" (glambda-counter))))

            (next-functor
             (lambda ()
               (conc "high_func" (functor-counter)))))
)
