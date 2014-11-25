(define-record-type <cprogram>
  (cprogram header type name parameters body)
  cprogram?
  (header cprogram-header)
  (type cprogram-type)
  (name cprogram-name)
  (parameters cprogram-parameters)
  (body cprogram-body))

(define-syntax make-cprogram
  (syntax-rules (header: type: name: parameters: body:)
    ((_ header: header type: type name: name parameters: parameters body: body)
     (cprogram header type name parameters body))
    ((_ type: type name: name parameters: parameters body: body)
     (cprogram #f type name parameters body))))

(softmodule

  public: ((cprogram-string
            (lambda (p)
              (conc (if (cprogram-header p)
                        (substrings->string (cprogram-header p))
                        "")
                    (cprogram-type p)
                    #\space
                    (cprogram-name p)
                    (cwrite-parameters (cprogram-parameters p))
                    (cwrite-statements (cprogram-body p)))))

           (cprogram-string/pretty
            (lambda (p)
              (conc (if (cprogram-header p)
                        (list->lines (cprogram-header p))
                        "")
                    #\newline
                    #\newline
                    (cprogram-type p)
                    #\space
                    (cprogram-name p)
                    (cwrite-parameters (cprogram-parameters p))
                    (cwrite-statements/pretty (cprogram-body p)))))

           (gprocedure->cprogram-string
            (lambda (name procedure)
              (cprogram-string (gprocedure->cprogram name procedure))))


           (gprocedure->cprogram-string/pretty
            (lambda (name procedure)
              (cprogram-string/pretty (gprocedure->cprogram name procedure))))

           (gprocedure->cprogram
            (lambda (name procedure)
              (proc->cprogram name procedure param-fmt)))

           (constant-struct-init-string
            (lambda (record expand)
              (let* ((grecord (ast-record-grecord-type record))
                     (alias (grecord-type-alias grecord))
                     (attribute-types (grecord-type-attribute-types grecord))
                     (args (ast-record-args record))
                     (instance-name (ast-record-name record)))
                (format "constant ~a ~a={~a}"
                        alias
                        instance-name
                        (list->csv
                         (map (lambda (arg)
                                (if (or (ast-record? arg) (ast-list? arg))
                                    (format "{~a}" (list->csv (map expand (ast-rest arg))))
                                    (expand arg)))
                              args))))))

           (struct-init-string
            (lambda (record expand)
              (let* ((grecord (ast-record-grecord-type record))
                     (alias (grecord-type-alias grecord))
                     (attribute-types (grecord-type-attribute-types grecord))
                     (attributes (grecord-type-attributes grecord))
                     (args (ast-record-args record))
                     (instance-name (ast-record-name record))
                     (prop-fmt (conc instance-name ".~a=~a;")))
                (conc (format "~a ~a;" alias instance-name)
                      (conc instance-name ".id=" (grecord-type-order grecord) ";")
                      (substrings->string
                       (map (lambda (attr value type) (format prop-fmt attr value))
                            attributes
                            (map expand args)
                            attribute-types))))))

           (struct-define
            (lambda (record-type)
              (let* ((attr-declare (lambda (type name) (format "~a ~a;" type name)))
                     (grecord record-type)
                     (type-alias (grecord-type-alias grecord))
                     (attribute-types (grecord-type-attribute-types grecord))
                     (attributes (grecord-type-attributes grecord))
                     (declared-attributes
                      (map attr-declare attribute-types attributes)))
                (format "typedef struct {~a~a} ~a;"
                        (if (string=? "gbase" type-alias)
                            ""
                            "int id;")
                        (substrings->string declared-attributes)
                        type-alias))))

           (property-type
            (lambda (rec arg)
              (list-ref (grecord-type-attribute-types (ast-record-grecord-type rec))
                        (index-of arg (ast-record-args rec)))))

           (property-name
            (lambda (rec arg)
              (list-ref (grecord-type-attributes (ast-record-grecord-type rec))
                        (index-of arg (ast-record-args rec)))))

           (multidimension-internal
            (lambda (rec x)
              (if (ast-record? x)
                  (format ".~a=(~a){~a}"
                          (property-name rec x)
                          (property-type rec x)
                          (list->csv (map (lambda (a)
                                            (multidimension-internal x a))
                                          (ast-record-args x))))
                  (format ".~a=(~a)~a"
                          (property-name rec x)
                          (property-type rec x)
                          (ast-expand x)))))

           (multidimension
            (lambda (x)
              (if (ast-record? x)
                  (format "{~a}" (list->csv (map (lambda (a)
                                                   (multidimension-internal x a))
                                                 (ast-record-args x))))
                  (ast-expand x))))

            (constant-declaration
            (lambda (c)
              (let* ((size (ast-constant-size c))
                     (name (ast-constant-name c))
                     (type (ast-constant-type c))
                     (args (map multidimension (ast-constant-args c)))
                     (refined-args (list->csv args)))
                (format "~a ~a[~a]={~a};"
                        type
                        name
                        size
                        refined-args))))

           (loop-declaration-string
            (lambda (tree expand)
              (let* ((args (ast-call-args tree)))
                ((loop-string-function tree) tree expand))))

           (list-type?
            (lambda (x)
              (string-contains x "*")))

           (void-type?
            (lambda (x)
              (if x
                  (string=? "record" (base-type x))
                  x)))

           (scope-arg?
            (lambda (a)
              (string-contains (ast-name a) "_s")))

           (base-type
            (lambda (x)
              (delete-substring "*" x)))

           (pointer-type
            (lambda (x)
              (conc (base-type x) "*")))

           (global-type
            (lambda (x)
              (conc "global " (pointer-type x))))

           (dynamic-space-type
            (lambda (t)
              (if (global-arg? t)
                  (global-type (ast-type t))
                  (ast-type t))))

           (standard-types (list "double"
                                 "float"
                                 "long"
                                 "int"
                                 "char"
                                 "uchar"
                                 "short"
                                 "ulong"
                                 "uint"
                                 "ushort"
                                 "bool"))

           (standard-type?
            (lambda (x)
              (member (base-type x) standard-types)))

           (variable-declare-string
            (lambda (var expand)
              (format "~a ~a=~a"
                      (ast-variable-type var)
                      (ast-variable-name var)
                      (expand (ast-variable-arg var)))))

           (list-declare-string
            (lambda (glst expand)
              (let* ((type (ast-list-type glst))
                     (name (ast-list-name glst))
                     (size (ast-list-size glst))
                     (args (list->csv (map expand (ast-list-args glst)))))
                (conc (if (null? (ast-list-args glst))
                          (format "~a ~a[~a];" type name size)
                          (format "~a ~a[~a]={~a};" type name size args)))))))

  private: ((proc->cprogram
             (lambda (name procedure param-func)
               (let* ((inputs (gprocedure-inputs procedure))
                      (output (gprocedure-output procedure))
                      (types (gprocedure-input-types procedure))
                      (tree (gprocedure-body procedure))
                      (expand ast-expand/local)
                      (instruction (expand tree))
                      (parameters (flatlist (map param-func inputs types)
                                            (if (list-arg? output)
                                                (param-func output (gprocedure-output-type procedure))
                                                '())))
                      (return (return-fmt instruction))
                      (declarations (extract-local-declarations tree expand)))
                 (make-cprogram type: (gprocedure-output-type procedure)
                                name: name
                                parameters: parameters
                                body: (flatlist declarations return)))))

            (loop-discrete-size
             (lambda (args)
               (let ((sizes (filter (lambda (n) (and (number? n) (> n 0))) (map glist-size args))))
                 (if (null? sizes)
                     0
                     (car sizes)))))

            (ast-arg->param-string
             (lambda (arg)
               (param-fmt arg (ast-arg-type arg))))

            (param-fmt
             (lambda (arg type)
               (let ((declare (lambda (type name) (conc type " " name)))
                     (base-name (ast-arg-name arg)))
                     (declare type base-name))))

            (return-fmt
             (lambda (x)
               (conc "return " x)))

            (cwrite-statements
             (lambda (lst)
               (format "{~a} "
                       (substrings->string
                        (map (lambda (x) (format "~a;" x)) lst)))))

            (cwrite-parameters
             (lambda (lst)
               (format "(~a)" (list->csv lst))))

            (cwrite-statements/pretty
             (lambda (lst)
               (format "{~%~a}~%"
                       (substrings->string
                        (map (lambda (x) (format "  ~a;~%" x)) lst))))))

  )
