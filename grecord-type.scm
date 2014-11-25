(define-record-type <grecord-type>
  (grecord-type alias types attributes order)
  grecord-type?
  (order grecord-type-order)
  (alias grecord-type-alias)
  (types grecord-type-attribute-types)
  (attributes grecord-type-attributes))

(define (record-aliases)
  (map grecord-type-alias record-types))

(define (alias-record-dictionary)
  (map cons (record-aliases) record-types))

(define (record-alias->base-types type)
  (grecord-type-attribute-types (cdr (assoc type (alias-record-dictionary)))))

(define (record-arg->base-args output)
  (let ((record-types (record-alias->base-types (ast-arg-type output))))
    (map ast-arg
         (iota (length record-types) (ast-arg-number output))
         record-types)))
