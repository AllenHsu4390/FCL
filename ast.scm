;;;; Abstract Syntax Tree Functions

;;;-----Records-----------------------------------------------------------------
(define-record-type <ast-literal>
  (ast-literal value type)
  ast-literal?
  (type ast-literal-type)
  (value ast-literal-value))

(define-record-type <ast-arg>
  (create-ast-arg name number type size)
  ast-arg?
  (name ast-arg-name set-ast-arg-name!)
  (size ast-arg-size set-ast-arg-size!)
  (number ast-arg-number set-ast-arg-number!)
  (type ast-arg-type set-ast-arg-type!))

(define-record-type <ast-variable>
  (create-ast-variable type name arg order size)
  ast-variable?
  (size ast-variable-size)
  (type ast-variable-type)
  (order ast-variable-order)
  (name ast-variable-name)
  (arg ast-variable-arg))

(define-record-type <ast-call>
  (ast-call operation type args)
  ast-call?
  (type ast-call-type)
  (args ast-call-args set-ast-call-args!)
  (operation ast-call-operation))

(define-record-type <ast-list>
  (ast-list name type size args order)
  ast-list?
  (order ast-list-order)
  (name ast-list-name)
  (type ast-list-type)
  (size ast-list-size)
  (args ast-list-args set-ast-list-args!))

(define-record-type <ast-constant>
  (ast-constant name type size args order)
  ast-constant?
  (order ast-constant-order)
  (name ast-constant-name)
  (args ast-constant-args set-ast-constant-args!)
  (type ast-constant-type)
  (size ast-constant-size))

(define-record-type <ast-shared>
  (ast-shared name buffer singular?)
  ast-shared?
  (singular? ast-shared-singular?)
  (name ast-shared-name)
  (buffer ast-shared-buffer set-ast-shared-buffer!))

(define-record-type <ast-record>
  (ast-record name args grecord-type order)
  ast-record?
  (order ast-record-order)
  (name ast-record-name)
  (args ast-record-args)
  (grecord-type ast-record-grecord-type))

;;;-----Constructors------------------------------------------------------------
(define (ast-arg number type #!optional size name)
  (create-ast-arg (or name (conc "arg" number)) number type (or size 0)))

(define (ast-variable type name arg order #!optional size)
  (create-ast-variable type name arg order (or size 0)))

;;;-----Functions---------------------------------------------------------------
(define (list-arg? a)
  (or (ast-list? a)
      (ast-constant? a)
      (and (ast-shared? a) (not (ast-shared-singular? a)))
      (ast-call/map? a)
      (list-type? (or (ast-type a) ""))))

(define (global-arg? a)
  (and (ast-shared? a) (not (ast-shared-singular? a))))

(define (ast-call/user? c)
  (and (ast-call? c) (user-function? (ast-call-operation c))))

(define (ast-call/map? c)
  (and (ast-call? c) (map-operation? (ast-call-operation c))))

(define (ast-call/fold? c)
  (and (ast-call? c) (fold-operation? (ast-call-operation c))))

(define (ast-call/dereference? c)
  (and (ast-call? c) (dereference? (ast-call-operation c))))

(define (ast-call/struct-access? c)
  (and (ast-call? c) (struct-access? (ast-call-operation c))))

(define ast-shared-type (o cl-buffer-type ast-shared-buffer))

(define ast-shared-size (o cl-buffer-size ast-shared-buffer))

(define (ast-call-input-type call)
  (map ast-arg-type ((o reverse cdr reverse ast-call-args) call)))

(define (ast-call-output-type call)
  ((o ast-arg-type last ast-call-args) call))

(define (ast-string-function tree)
  (case-cond tree
    (ast-arg? ast-arg-string)
    (ast-literal? ast-literal-string)
    (ast-call? ast-call-string)
    (ast-variable? ast-variable-string)
    (ast-list? ast-list-string)
    (ast-constant? ast-constant-string)
    (ast-shared? ast-shared-string)
    (ast-record? ast-record-string)
    (else (lambda (t e) t))))

(define (ast-order tree)
  ((case-cond tree
     (ast-constant? ast-constant-order)
     (ast-list? ast-list-order)
     (ast-record? ast-record-order)
     (ast-variable? ast-variable-order)
     (grecord-type? grecord-type-order)
     (ast-call?
      (case-cond (ast-call-operation tree)
        (map-operation? (o map-operation-order ast-call-operation))
        (fold-operation? (o fold-operation-order ast-call-operation)))))
   tree))

(define (ast-rest-function tree)
  (let* ((fold-start (o fold-operation-start ast-call-operation))
         (fold-args (lambda (c) (flatlist (fold-start c) (ast-call-args c)))))
    (case-cond tree
      (ast-call/fold? fold-args)
      (ast-call? ast-call-args)
      (ast-list? ast-list-args)
      (ast-constant? ast-constant-args)
      (ast-record? ast-record-args)
      (ast-variable? (o list ast-variable-arg))
      (else #f))))

(define (ast-rest tree)
  (let ((rst (ast-rest-function tree)))
    (if rst (rst tree) '())))

(define (ast-type-function tree)
  (case-cond tree
    (ast-call? ast-call-type)
    (ast-arg? ast-arg-type)
    (ast-list? ast-list-type)
    (ast-constant? ast-constant-type)
    (ast-variable? ast-variable-type)
    (ast-literal? ast-literal-type)
    (ast-shared? ast-shared-type)
    (ast-record? (o grecord-type-alias ast-record-grecord-type))
    (else (lambda (x) #f))))

(define (ast-name-function tree)
  (case-cond tree
    (ast-call? (o operation-name ast-call-operation))
    (ast-arg? ast-arg-name)
    (ast-list? ast-list-name)
    (ast-constant? ast-constant-name)
    (ast-shared? ast-shared-name)
    (ast-record? ast-record-name)
    (ast-variable? ast-variable-name)
    (ast-literal? ast-literal-value)
    (else (lambda (x) #f))))

(define (ast-name tree)
  ((ast-name-function tree) tree))

(define (ast-type tree)
  ((ast-type-function tree) tree))

(define (ast-expand tree)
  ((ast-string-function tree) tree ast-expand))

(define (ast-expand/local tree)
  ((ast-string-function tree) tree ast-expand/local))

;;;-----String Forms------------------------------------------------------------
(define (ast-shared-string shared expand)
  (if (ast-shared-singular? shared)
      (conc (ast-shared-name shared) "[0]")
      (conc (ast-shared-name shared))))

(define (local-arg-string arg)
  (ast-arg-name arg))

(define (kernel-arg-string arg)
  (conc (ast-arg-name arg) "[i]"))

(define (ast-arg-string argument expand)
  (if (eq? expand ast-expand/local)
      (local-arg-string argument)
      (kernel-arg-string argument)))

(define (ast-literal-string literal expand)
  (ast-literal-value literal))

(define (ast-variable-string variable expand)
  (ast-variable-name variable))

(define (ast-list-string glst expand)
  (ast-list-name glst))

(define (ast-record-string record expand)
  (ast-record-name record))

(define (ast-constant-string gconst expand)
  (ast-constant-name gconst))

(define (ast-call-string call expand)
  (let ((op (ast-call-operation call)))
    ((operation-string-function op) call expand)))
