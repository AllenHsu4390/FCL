(define-syntax gbind-operator/binary
  (syntax-rules ()
    ((_ gname cname)
     (define gname (call-invoker/binary cname)))))

(define-syntax gbind-operator/tertiary
  (syntax-rules ()
    ((_ gname cname1 cname2)
     (define gname (call-invoker/tertiary cname1 cname2)))))

(define-syntax gbind
  (syntax-rules ()
    ((_ gname cname)
     (define gname (call-invoker cname)))
    ((_ gname cname type)
     (define gname (call-invoker cname type)))))

(define-syntax gdefine-record-type-accessor
  (syntax-rules ()
    ((_ (prop accessor) grec)
     (define accessor (record-accessor-invoker (make-c-safe (string-quote prop)) grec)))))

(define (gdefine-record-type-get-type-internal t)
  t)

(define (gdefine-record-type-get-prop-internal t p)
  p)

(define-syntax gdefine-record-type-get-type
  (syntax-rules ()
    ((_ (type prop))
     (gdefine-record-type-get-type-internal (string-quote type)))))

(define-syntax gdefine-record-type-get-prop
  (syntax-rules ()
    ((_ (type prop))
     (gdefine-record-type-get-prop-internal (string-quote type)
                                            (make-c-safe (string-quote prop))))))

(define-syntax glambda-make-arg
  (syntax-rules ()
    ((_ (space type arg) count)
     (define arg (ast-arg count (cond ((string=? "gbuffer" (string-quote space))
                                       (conc "global " (string-quote type) "*"))
                                      ((string=? "glist" (string-quote space))
                                       (conc (string-quote type) "*"))
                                      ((string=? "gdata" (string-quote space))
                                       (string-quote type))))))
    ((_ (type arg) count)
     (define arg (ast-arg count (make-c-safe (string-quote type)))))))

(define-syntax glambda-param-arg
  (syntax-rules ()
    ((_ (space type arg))
     arg)
    ((_ (type arg))
     arg)))

(define-syntax glambda-param-type
  (syntax-rules ()
    ((_ (space type arg))
     type)
    ((_ (type arg))
     type)))

;;;-----Exported Macros---------------------------------------------------------
(define-syntax glet
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     (let ((x (declare-variable v))
           ...)
       e1
       e2
       ...))))

(define-syntax glet*
  (syntax-rules ()
    ((_ ((x v) ...) e1 e2 ...)
     (let* ((x (declare-variable v))
            ...)
       e1
       e2
       ...))))

(define-syntax gdefine
  (syntax-rules (->)
    ((_ (name -> o-type) body)
     (define name (glambda (o-type) body)))
    ((_ (name input ... -> o-type) body)
     (define name (glambda (input ... -> o-type) body)))
    ((_ name x)
     (define name x))))

(define-syntax gdefine*
  (syntax-rules (->)
    ((_ (name -> o-type) body)
     (begin
       (define name (glambda (o-type) body))
       (fcl-include name)))
    ((_ (name input ... -> o-type) body)
     (begin
       (gdefine (name input ... -> o-type) body)
       (fcl-include name)))
    ((_ name x)
     (begin
       (gdefine name x)
       (if (gprocedure? x)
           (fcl-include name))))))

(define-syntax glambda
  (syntax-rules (->)
    ((_ (o-type) body)
     (call-invoker/user
      (gprocedure (list (ast-arg 0 (make-c-safe (string-quote o-type))))
                  body
                  (order-counter))))
    ((_ (input ... -> o-type) body)
     (let ((arg-counter (new-counter 0)))
       (glambda-make-arg input (arg-counter))
       ...
       (call-invoker/user
        (gprocedure (list (glambda-param-arg input)
                          ...
                          (ast-arg (arg-counter) (make-c-safe (string-quote o-type))))
                    body
                    (order-counter)))))))

(define-syntax gdefine-record-type
  (syntax-rules ()
    ((_ type-name (constructor type-prop ...) pred prop-accessor ...)
     (begin
       (define auto-genrec-type
         (grecord-type (make-c-safe (string-quote type-name))
                       (flatten (list (gdefine-record-type-get-type type-prop) ...))
                       (flatten (list (gdefine-record-type-get-prop type-prop) ...))
                       (order-counter)))
       (define pred (record-pred-invoker auto-genrec-type))
       (define constructor (record-constructor-invoker auto-genrec-type))
       (gdefine-record-type-accessor prop-accessor auto-genrec-type)
       ...))))

(define-syntax glist
  (syntax-rules ()
    ((_ type args ...)
     (glist-internal (make-c-safe (string-quote type)) (list args ...)))))

(define-syntax gcond
  (syntax-rules (else)
    ((_ (else e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...) c1 ...)
     (gif e1
          (begin e2 ...)
          (gcond c1 ...)))))

(define-syntax gcase
  (syntax-rules (else)
    ((_ key ((c ...) t) ... (else e))
     (letrec ((switch (lambda (k lst)
                        (cond ((null? (cdr lst))
                               (g= k (car lst)))
                              ((null? (cdr (cdr lst)))
                               (gor (g= k (car lst)) (g= k (cadr lst))))
                              (else
                               (gor (g= k (car lst)) (switch k (cdr lst))))))))
       (gcond ((switch key (list c ...)) t) ... (else e))))))

(define-syntax gfold
  (syntax-rules ()
    ((_ proc start lst ...)
     (gfold-internal proc start (list start lst ...) #t))))

(define-syntax gfold-right
  (syntax-rules ()
    ((_ proc start lst ...)
     (gfold-internal proc start (list start lst ...) #f))))


(define-syntax gmap
  (syntax-rules  (->)
    ((_ proc lst ...)
     (gmap-internal proc (list lst ...)))))

(define-syntax gdata
  (syntax-rules ()
    ((_ type value readonly?)
     (gdata-construct (make-c-safe (string-quote type)) value #t))
    ((_ type value)
     (gdata-construct (make-c-safe (string-quote type)) value #f))))

(define-syntax gbuffer
  (syntax-rules ()
    ((_ type args ...)
     (list->gbuffer type (list args ...)))))

(define-syntax fcl-include
  (syntax-rules ()
    ((_ invoker ...)
     (begin (set! invoker (invoker-with-kernel invoker))
            ...))))

(define-syntax fcl-set!
  (syntax-rules (pmap)
    ((_ buffer (pmap invoker args ...))
     (pmap!-internal invoker (list args ...) buffer))
    ((_ buffer value)
     (fcl-set-internal! buffer value))))

(define-syntax set-gdata!
  (syntax-rules ()
    ((_ data value)
     (set-gbuffer-internal! data value))))

(define-syntax pmap
  (syntax-rules ()
    ((_ invoker args ...)
     (pmap-internal invoker (list args ...)))))

(define-syntax list->gbuffer
  (syntax-rules ()
    ((_ type lst)
     (list->gbuffer-internal (make-c-safe (string-quote type)) lst))
    ((_ type lst readonly?)
     (list->gbuffer-internal (make-c-safe (string-quote type)) lst readonly?))))

(define-syntax list->glist
  (syntax-rules ()
    ((_ type lst)
     (glist-internal (make-c-safe (string-quote type)) lst))))

(define-syntax define-fcl-exp
  (syntax-rules ()
    ((_ (name params ...) body ...)
     (define name (lambda (params ...)
                    (fcl-run (lambda (params ...) body ...)
                             params ...))))))

(define-syntax fcl-check
  (syntax-rules ()
    ((_ gname)
     (fcl-check-internal (string-quote gname) gname))))
