(define-record-type <gprocedure>
  (gprocedure args body order)
  gprocedure?
  (order gprocedure-order)
  (args gprocedure-args set-gprocedure-args!)
  (body gprocedure-body set-gprocedure-body!))

(define (gprocedure-inputs p)
  ((o reverse cdr reverse gprocedure-args) p))

(define (gprocedure-output p)
  (last (gprocedure-args p)))

(define (gprocedure-types p)
  (map ast-arg-type (gprocedure-args p)))

(define (gprocedure-output-type p)
  ((o car reverse gprocedure-types) p))

(define (gprocedure-input-types p)
  ((o reverse cdr reverse gprocedure-types) p))

(define (scope-args p)
  (let* ((inputs (gprocedure-inputs p))
         (body (gprocedure-body p))
         (body-args (extract-ast-type body ast-arg?)))
    (delete-duplicates (set-difference body-args inputs))))

(define (scope-gprocedure p scope)
  (let* ((inputs (gprocedure-inputs p)))
    (map (lambda (a) (set-ast-arg-name! a (assign-scope-arg))) scope)
    (set-gprocedure-args! p (flatlist inputs scope (gprocedure-output p)))
    p))

(define (gprocedure-instance name proc call-args)
  (let ((proc-args (gprocedure-args proc))
        (list-args (filter list-arg? call-args)))
    (if (not (null? list-args))
        (let ((indices (find-indices list-args call-args))
              (inst-name (assign-glambda-name)))
          (for-each (lambda (arg i)
                      (set-ast-arg-size! (list-ref proc-args i) (glist-size arg gprocedure-instance)))
                    list-args
                    indices)
          (cons inst-name
                (gprocedure proc-args
                            (rewrite-tree (gprocedure-body proc))
                            (gprocedure-order proc))))
        (cons name proc))))
