;;;; File: glanguage.scm
;;;; Description: language functionality available to the user
;;;; Author: Allen Hsu

(module fcl

  (;; gpu space
   g=
   gand
   gor
   g>
   g<
   gnot
   gand-bit
   gor-bit
   gnot-bit
   gshiftr-bit
   gshiftl-bit
   g+
   g-
   g*
   g/
   gmodulo
   gneg
   gsqrt
   gcos
   gsin
   gtan
   gceil
   gfloor
   gpow
   gmax
   gmin
   gif
   gcond
   gcase
   gmap
   gfold
   gfold-right
   glist
   glet
   glet*
   glambda
   gdefine
   gdefine*
   gdefine-record-type
   gtrue
   gfalse
   gtype
   gbase-id
   make-int
   make-float
   gref
   glength

   ;; cpu space
   list->glist
   list->gbuffer
   vector->gbuffer
   gbuffer->list
   gbuffer->vector
   gdata
   gbuffer
   fcl-begin
   fcl-end
   fcl-check
   fcl-set!
   define-fcl-exp
   pmap
   pfilter
   premove

   ;; testing
   write-to-file
   clock-time
   clock-difference

   ;; deprecation
   release-all-gbuffers
   release-gbuffer
   fcl-include
   fcl-reset
   glist-ref
   gbuffer-ref
   glist-size

   ;; macro exposed functions
   fcl-check-internal
   fcl-set-internal!
   fcl-compile
   fcl-run
   glist-internal
   gmap-internal
   gfold-internal
   gdefine-record-type-get-type-internal
   gdefine-record-type-get-prop-internal
   set-gbuffer-internal!
   gdata-construct
   list->gbuffer-internal
   pmap!-internal
   pmap-internal
   order-counter
   declare-variable
   gprocedure
   ast-arg
   string-quote
   gdefine-record-type-get-type
   gdefine-record-type-get-prop
   gdefine-record-type-accessor
   glambda-make-arg
   glambda-param-arg
   glambda-param-type
   call-invoker/user
   new-counter
   make-c-safe
   invoker-with-kernel
   grecord-type
   record-constructor-invoker
   record-accessor-invoker
   record-pred-invoker
   )

  (import scheme chicken)

  (include "utility")
  (include "cwrite")
  (include "ast")
  (include "operation")
  (include "gprocedure")
  (include "grecord-type")
  (include "extraction")
  (include "call-invoke")
  (include "error")
  (include "kernel")
  (include "cl-interface")
  (include "stateful-objects")
  (include "glanguage")

  (define (glist-gentype->type args)
    (gentype->type (map gtype args)))

  (define (gentype->type types)
    (let ((priority (prioritize-types types)))
      (or priority (car types))))

  (define (dynamic->type args)
    (let* ((types (map ast-type args)))
      (prioritize-types types)))

  (define (prioritize-types types)
    (let* ((clean-types (map (lambda (t)
                               (delete-substrings (list "global " "constant " "*") t))
                             (remove not types)))
           (has-type (lambda (t) (member t clean-types))))
      (cond ((has-type "double") "double")
            ((has-type "float") "float")
            ((has-type "long") "long")
            ((has-type "int") "int")
            ((has-type "short") "short")
            ((has-type "ulong") "ulong")
            ((has-type "uint") "uint")
            ((has-type "ushort") "ushort")
            ((has-type "uchar") "uchar")
            ((has-type "char") "char")
            (else #f))))

  (define (make-c-safe name)
    (letrec ((r (lambda (lst accum)
                  (if (null? lst)
                      accum
                      (r (cdr lst)
                         (delete-substring (car lst) accum))))))
      (r (list "!" "@" "#" "$" "%" "^" "" "-" "+" "=" "?" "<" ">" "/" "~")
         name)))

  (define (gcompile name p)
    (let* ((in (gprocedure-inputs p))
           (out (gprocedure-output p))
           (body (gprocedure-body p))
           (local-declarations (extract-local-declarations body ast-expand))
           (instruction (ast-expand body))
           (shared (extract-shared-instances body))
           (top-level (append (extract-record-type-definitions body)
                              (extract-constant-declarations body)
                              (extract-user-function-definitions name body)))
           (kernel-program (kernel-source-code in
                                               out
                                               instruction
                                               top-level
                                               shared
                                               local-declarations)))
      (cprogram-string kernel-program)))

  (define (fcl-check-internal name inv)
    (let* ((call (invoker->ast-call inv (list 0)))
           (op (ast-call-operation call))
           (gproc (user-function-procedure op))
           (in (gprocedure-inputs gproc))
           (out (gprocedure-output gproc))
           (body (gprocedure-body gproc)))
      (check-kernel-errors name in out body)))

  (define (fcl-compile invoker)
    (let ((call (invoker->ast-call invoker (list 0))))
      (gcompile/pretty
       (user-function-name (ast-call-operation call))
       (user-function-procedure (ast-call-operation call)))))

  (define (gcompile/pretty name p)
    (let* ((in (gprocedure-inputs p))
           (out (gprocedure-output p))
           (body (gprocedure-body p))
           (instruction (ast-expand body))
           (shared (extract-shared-instances body))
           (local-declarations (extract-local-declarations body ast-expand))
           (top-level (append (extract-record-type-definitions body)
                              (extract-constant-declarations body)
                              (extract-user-function-definitions name body)))
           (kernel-program (kernel-source-code in
                                               out
                                               instruction
                                               top-level
                                               shared
                                               local-declarations)))
      (cprogram-string/pretty kernel-program)))

  (define (glist-type lst)
    (case-cond lst
      (ast-arg? (delete-substring "*" (ast-arg-type lst)))
      (ast-list? (ast-list-type lst))
      (ast-shared? (ast-shared-type lst))
      (ast-constant? (delete-substring "*" (ast-constant-type lst)))
      (ast-call/map? (map-operation-type (ast-call-operation lst)))
      (else #f)))

  (define (gtype lst)
    (delete-substring "*" (or (ast-type lst) "")))

  (define (cl-buffer-pmap program proc inputs shared)
    (let* ((output (cl-buffer/write (gprocedure-output-type proc)
                                    (cl-buffer-size (car inputs)))))
      (cl-run! program proc (append inputs shared) output)
      (ast-shared (assign-shared-name)
                  (if (null? (cdr output))
                      (car output)
                      output)
                  #f)))

  (define (pmap-internal inv lst)
    (let* ((invoker (invoker-with-kernel inv))
           (call (invoker->ast-call invoker (list 0)))
           (op (ast-call-operation call))
           (kernel (user-function-kernel op))
           (prog (cl-kernel-program kernel))
           (proc (user-function-procedure op))
           (shared (map normalize-shared (cl-kernel-shared kernel)))
           (inputs (map normalize-shared lst)))
      (cl-buffer-pmap prog proc inputs shared)))

  (define (pmap!-internal inv lst buffer)
    (let* ((invoker (invoker-with-kernel inv))
           (call (invoker->ast-call invoker (list 0)))
           (op (ast-call-operation call))
           (kernel (user-function-kernel op))
           (prog (cl-kernel-program kernel))
           (proc (user-function-procedure op))
           (shared (map normalize-shared (cl-kernel-shared kernel)))
           (inputs (map normalize-shared lst)))
      (cl-run! prog proc (append inputs shared) (normalize-shared buffer))))

  (define (gfold-internal proc start args left?)
    (let* ((call (invoker->ast-call proc args))
           (op (ast-call-operation call))
           (type (cond ((or (user-function? op)
                            (struct-access? op)
                            (function? op))
                        (ast-call-type call))
                       (else (glist-gentype->type args)))))
      (function-call/fold type proc (cdr args) start left?)))

  (define (gmap-internal proc args)
    (let* ((call (invoker->ast-call proc args))
           (op (ast-call-operation call))
           (type (cond ((or (user-function? op)
                            (struct-access? op)
                            (function? op))
                        (ast-call-type call))
                       (else (glist-gentype->type args)))))
      (function-call/map type proc args)))

  (define (normalize-shared s)
    (if (ast-shared? s)
        (ast-shared-buffer s)
        s))

  (define (pfilter invoker lst)
    (let ((pfilter-rtn '()))
      (for-each (lambda (value x)
                  (if (= value 1)
                      (set! pfilter-rtn (append pfilter-rtn (list x)))))
                (gbuffer->list (pmap invoker lst))
                (gbuffer->list lst))
      (list->gbuffer-internal (cl-buffer-type (ast-shared-buffer lst)) pfilter-rtn)))

  (define (premove invoker lst)
    (let ((premove-rtn '()))
      (for-each (lambda (value x)
                  (if (= value 0)
                      (set! premove-rtn (append premove-rtn (list x)))))
                (gbuffer->list (pmap invoker lst))
                (gbuffer->list lst))
      (list->gbuffer-internal (cl-buffer-type (ast-shared-buffer lst)) premove-rtn)))

  (define (glist-size lst #!optional caller)
    (let ((r (lambda (x) (glist-size x caller))))
      (case-cond lst
        (ast-arg? (ast-arg-size lst))
        (ast-list? (ast-list-size lst))
        (ast-shared? (ast-shared-size lst))
        (ast-constant? (ast-constant-size lst))
        (ast-call/map? ((o r car ast-call-args) lst))
        (ast-variable? (ast-variable-size lst))
        (ast-call/user? ((o r car ast-call-args) lst))
        (ast-literal? 0)
        (else (not-glist-error lst glist-size caller)))))

  (define (gbuffer-release! shared)
    (let ((buffer (ast-shared-buffer shared))
          (release (o cl-release-mem-object cl-buffer-mem)))
      (if (cl-buffer-mem buffer)
          (if (list? buffer)
              (map release buffer)
              (release buffer)))
      (set-cl-buffer-mem! buffer #f)))

  (define (release-all-gbuffers)
    (map gbuffer-release! buffers))

  (define (glist-internal/original type lst)
    (let* ((name (assign-glist-name))
           (pred (lambda (x) (or (ast-arg? x) (ast-call? x) (ast-variable? x))))
           (make (if (null? (filter pred lst)) ast-constant ast-list)))
      (make name type (length lst) lst (order-counter))))

  (define (glist-internal type lst)
    (ast-list (assign-glist-name) type (length lst) lst (order-counter)))

  (define (vector->gbuffer vec #!optional readonly?)
    (let* ((mk (if readonly? vector->cl-buffer vector->cl-buffer/mutable))
           (buffer (mk vec))
           (name (assign-shared-name))
           (rtn (ast-shared name buffer #f)))
      (add-buffer rtn)
      rtn))

  (define (gbuffer->vector shared)
    (let ((buffer (ast-shared-buffer shared)))
      (if (list? buffer)
          (map cl-buffer-vector buffer)
          (cl-buffer-vector buffer))))

  (define (gbuffer->list shared)
    (let* ((buffer (ast-shared-buffer shared))
           (vec (gbuffer->vector shared))
           (to-list (lambda (t v) ((type-vector->list t) v)))
           (types (if (list? buffer)
                      (map cl-buffer-type buffer)
                      (cl-buffer-type buffer))))
      (if (list? vec)
          (map to-list (map cl-buffer-type buffer) vec)
          (to-list types vec))))

  (define (fcl-set-internal! b value)
    (if (ast-shared-singular? b)
        (set-gbuffer-value! b 0 value)
        (set-gbuffer-internal! b value)))

  (define (set-gbuffer-value! s i value)
    (let* ((buffer (ast-shared-buffer s))
           (type (cl-buffer-type buffer)))
      ((type-vector-set! type) (cl-buffer-vector buffer) i value)
      (cl-enqueue-buffer/write (cl-buffer-mem buffer)
                               (cl-buffer-bytes buffer)
                               (cl-buffer-host buffer))))
  (define (release-kernel f)
    (let ((release! (lambda (k)
                      (if k (cl-release-kernel (cl-kernel-program k))))))
      ((o release! user-function-kernel) f)))

  (define (release-all-gbuffers)
    (map gbuffer-release! buffers)
    (set! buffers '()))

  (define (release-all-gprocedures)
    (map release-user-function user-functions)
    (set! user-functions '()))

  (define (release-user-function f)
    (release-kernel f)
    (remove-user-function f))

  (define (fcl-exclude inv)
    (let ((f (ast-call-operation (invoker->ast-call inv (list 0)))))
      (release-kernel f)
      (remove-user-function f)))

  (define (release-gbuffer buffer)
    (gbuffer-release! buffer)
    (remove-buffer buffer))

  (define (fcl-release x)
    (if (ast-shared? x)
        (release-gbuffer x)
        (fcl-exclude x)))

  (define (fcl-run proc . args)
    (let ((starting-buffers buffers)
          (starting-functions user-functions)
          (starting-types record-types)
          (starting-constants global-constants))
      (define rtn (apply proc args))
      (define new-buffers (set-difference buffers starting-buffers))
      (define new-functions (set-difference user-functions starting-functions))
      (define new-types (set-difference record-types starting-types))
      (define new-constants (set-difference global-constants starting-constants))
      (for-each release-gbuffer new-buffers)
      (for-each remove-buffer new-buffers)
      (for-each remove-user-function new-functions)
      (for-each remove-constant new-constants)
      rtn))

  (define release-gbuffer gbuffer-release!)
  (define fcl-begin cl-begin)
  (define fcl-end cl-end)
  (define fcl-reset reset-stateful-objects)
  (define gtrue "true")
  (define gfalse "false")

  ;; test
  (define glength (lambda (gseq) (glist-size gseq)))
  (define gref call-invoker/dereference)
  ;;

  ;; deprecation
  (define glist-ref call-invoker/dereference)
  (define gbuffer-ref call-invoker/dereference)
  ;;

  (gbind _gcos "cos")
  (gbind _gsin "sin")
  (gbind _gtan "tan")
  (gbind _gceil "ceil")
  (gbind _gfloor "floor")
  (gbind _gsqrt "sqrt")
  (gbind _gpow "pow")
  (gbind _gmax "fmax")
  (gbind _gmin "fmin")

  (gbind make-int "(int)" "int")
  (gbind make-float "(float)" "float")
  (gbind make-double "(double)" "double")

  (gbind gneg "-")
  (gbind gindirection "*")
  (gbind gnot "!")

  (gbind-operator/tertiary gif "?" ":")
  (gbind-operator/binary gset! "=")
  (gbind-operator/binary gand "&&")
  (gbind-operator/binary gor "||")
  (gbind-operator/binary g= "==")
  (gbind-operator/binary g> ">")
  (gbind-operator/binary g< "<")
  (gbind-operator/binary g+ "+")
  (gbind-operator/binary g- "-")
  (gbind-operator/binary g* "*")
  (gbind-operator/binary g/ "/")
  (gbind-operator/binary gmodulo "%")
  (gbind-operator/binary gand-bit "&")
  (gbind-operator/binary gor-bit "|")
  (gbind-operator/binary gnot-bit "^")
  (gbind-operator/binary gshiftr-bit ">>")
  (gbind-operator/binary gshiftl-bit "<<")

  (gdefine (gtan (float x) -> float)
    (_gtan x))

  (gdefine (gsin (float x) -> float)
    (_gsin x))

  (gdefine (gcos (float x) -> float)
    (_gcos x))

  (gdefine (gmax (float x) (float y) -> float)
    (_gmax x y))

  (gdefine (gmin (float x) (float y) -> float)
    (_gmin x y))

  (gdefine (gpow (float x) (float y) -> float)
    (_gpow x y))

  (gdefine (gceil (float x) -> float)
    (_gceil x))

  (gdefine (gfloor (float x) -> float)
    (_gfloor x))

  (gdefine (gsqrt (float x) -> float)
    (_gsqrt x))

  (gdefine-record-type <gbase>
    (gbase (int id))
    gbase?
    (id gbase-id))

  )
