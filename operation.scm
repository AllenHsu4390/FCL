(define-record-type <binary>
  (binary symbol)
  binary?
  (symbol binary-symbol))

(define-record-type <tertiary>
  (tertiary symbol1 symbol2)
  tertiary?
  (symbol1 tertiary-symbol1)
  (symbol2 tertiary-symbol2))

(define-record-type <dereference>
  (dereference)
  dereference?)

(define-record-type <function>
  (function cname)
  function?
  (cname function-cname))

(define-record-type <user-function>
  (user-function cname gprocedure kernel-code local-code)
  user-function?
  (cname user-function-name)
  (gprocedure user-function-procedure)
  (kernel-code user-function-kernel)
  (local-code user-function-local))

(define-record-type <struct-access>
  (struct-access property record-type)
  struct-access?
  (record-type struct-access-record-type)
  (property struct-access-property))

(define-record-type <map-operation>
  (map-operation functor-name procedure type order)
  map-operation?
  (functor-name map-operation-name)
  (order map-operation-order)
  (procedure map-operation-procedure)
  (type map-operation-type))

(define-record-type <fold-operation>
  (fold-operation functor-name procedure start type left? order)
  fold-operation?
  (left? fold-operation-left?)
  (functor-name fold-operation-name)
  (order fold-operation-order)
  (procedure fold-operation-procedure)
  (start fold-operation-start)
  (type fold-operation-type))

;;;-----Operation Strings-------------------------------------------------------
(define (binary-string call expand)
  (let* ((args (ast-call-args call))
         (operands (map expand args))
         (operator (binary-symbol (ast-call-operation call))))
    (conc "(" (substrings->string (splice-element operator operands)) ")")))

(define (tertiary-string call expand)
  (let* ((args (ast-call-args call))
         (arg1 (expand (car args)))
         (arg2 (expand (cadr args)))
         (arg3 (expand (caddr args)))
         (operation (ast-call-operation call))
         (operator1 (tertiary-symbol1 operation))
         (operator2 (tertiary-symbol2 operation)))
    (conc "(" arg1 operator1 arg2 operator2 arg3 ")")))

(define (dereference-string call expand)
  (let* ((op (ast-call-operation call))
         (args (ast-call-args call))
         (lst (car args))
         (index (cadr args)))
    (conc (case-cond lst
            (ast-arg? (ast-arg-name lst))
            (ast-list? (ast-list-name lst))
            (ast-constant? (ast-constant-name lst))
            (ast-shared? (ast-shared-name lst))
            (ast-call/user? (expand lst))
            (ast-call/map? (map-operation-name (ast-call-operation lst))))
          "[" (expand index) "]")))

(define (function-string call expand)
  (let* ((expand-args (lambda (args) (map expand args)))
         (cname (function-cname (ast-call-operation call)))
         (params ((o list->csv expand-args ast-call-args) call)))
    (conc cname "(" params ")")))

(define (user-function-string call expand)
  (let* ((op (ast-call-operation call))
         (proc (user-function-procedure op))
         (expand-args (lambda (args)
                        (map (lambda (a t)
                               (cond ((void-type? t)
                                      (if (void-type? (ast-type a))
                                          (expand a)
                                          (conc "&" (expand a))))
                                     ((void-type? (ast-type a))
                                      (format "(*(~a*)~a)" t (expand a)))
                                     (else (expand a))))
                             args
                             (if (list-type? (ast-call-type call))
                                 (gprocedure-types proc)
                                 (gprocedure-input-types proc)))))
         (cname (user-function-name op))
         (params ((o list->csv expand-args ast-call-args) call)))
    (conc cname "(" params ")")))

(define (struct-access-string call expand)
  (let* ((op (ast-call-operation call))
         (prop (struct-access-property op))
         (obj (car (ast-call-args call)))
         (cast (format "(~a*)" (grecord-type-alias (struct-access-record-type op))))
         (pointer-access (conc "(" cast (expand obj) ")->" prop))
         (normal-access (conc (expand obj) "." prop)))
    (if (void-type? (ast-type obj))
        pointer-access
        normal-access)))

(define (map-string call expand)
  (map-operation-name (ast-call-operation call)))

(define (fold-string call expand)
  (fold-operation-name (ast-call-operation call)))

;;;-----Functions---------------------------------------------------------------
(softmodule

  public: ((operation-procedure op-procedure)
           (operation-type op-type)
           (operation-name op-name)
           (operation-string-function op-string-function)

           (struct-access-property-type
            (lambda (op)
              (let* ((rec-type (struct-access-record-type op))
                     (prop (struct-access-property op))
                     (attr-types (grecord-type-attribute-types rec-type))
                     (attr (grecord-type-attributes rec-type))
                     (index (list-index (lambda (x) (string=? prop x)) attr)))
                (list-ref attr-types index)))))

  private: ((op-name-function
             (lambda (op)
               (case-cond op
                 (user-function? user-function-name)
                 (map-operation? map-operation-name)
                 (fold-operation? fold-operation-name)
                 (struct-access? struct-access-property)
                 (function? function-cname)
                 (else (lambda (x) '())))))

            (op-type-function
             (lambda (op)
               (case-cond op
                 (user-function? (o gprocedure-output-type user-function-procedure))
                 (map-operation? map-operation-type)
                 (fold-operation? fold-operation-type)
                 (struct-access? (o grecord-type-alias struct-access-record-type))
                 (else (lambda (x) '())))))

            (access-property-type
             (lambda (call)
               (let ((prop (struct-access-property call))
                     (rec (struct-access-record-type call)))
                 (list-ref (grecord-type-attribute-types rec)
                           (index-of prop (grecord-type-attributes rec))))))

            (op-type
             (lambda (op)
               ((op-type-function op) op)))

            (op-name
             (lambda (op)
               ((op-name-function op) op)))

            (op-procedure
             (lambda (op)
               (let ((find-proc (case-cond op
                                  (map-operation? map-operation-procedure)
                                  (fold-operation? fold-operation-procedure))))
                 (find-proc op))))

            (op-string-function
             (lambda (op)
               (case-cond op
                 (binary? binary-string)
                 (tertiary? tertiary-string)
                 (dereference? dereference-string)
                 (function? function-string)
                 (user-function? user-function-string)
                 (struct-access? struct-access-string)
                 (fold-operation? fold-string)
                 (map-operation? map-string)))))
  )
