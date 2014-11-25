(include "loop")

(define (extract-local-declarations tree expand)
  (let* ((functors (extract-functors tree))
         (glsts (extract-lists tree))
         (records (record-instances tree))
         (variables (extract-variables tree)))
    (map (lambda (x) (local-declaration-string x expand))
         (flatlist
          (sort (append records glsts functors variables)
                sort-by-order)))))

(define (extract-user-function-definitions name tree)
  (let* ((cname user-function-name)
         (not-kernel? (lambda (x) (not (string=? name (cname x)))))
         (duplicate? (lambda (a b) (string=? (cname a) (cname b))))
         (unique (delete-duplicates user-functions duplicate?)))
    (map user-function-local (filter not-kernel? unique))))

(define (extract-record-type-definitions tree)
  (let ((unique (delete-duplicates record-types)))
    (map struct-define (sort unique sort-by-order))))

(define (extract-constant-declarations tree)
  (let* ((cname ast-constant-name)
         (duplicate? (lambda (a b) (string=? (cname a) (cname b)))))
    (map constant-declaration (delete-duplicates global-constants duplicate?))))

(define (extract-shared-instances tree)
  (delete-duplicates (extract-ast-type tree ast-shared?)))

(define (extract-ast-type tree is-type?)
  (letrec ((continue (lambda (t) (map extract (ast-rest t))))
           (found (lambda (t) (list t (continue t))))
           (extract (lambda (t)
                      (if (is-type? t)
                          (found t)
                          (continue t)))))
    (flatten (extract tree))))

(define (local-declaration-string x expand)
  ((case-cond x
     (ast-call? loop-declaration-string)
     (ast-record? struct-init-string)
     (ast-variable? variable-declare-string)
     (ast-list? list-declare-string)) x expand))

(define (sort-by-order a b)
  (< (ast-order a) (ast-order b)))

(define (record-instances tree)
  (delete-duplicates (extract-ast-type tree ast-record?)
                     ast-record-compare))

(define (extract-variables tree)
  (delete-duplicates (extract-ast-type tree ast-variable?)
                     ast-variable-compare))

(define (extract-functors tree)
  (let* ((calls (extract-ast-type tree ast-call?))
         (functors (filter functor? calls)))
    (delete-duplicates functors ast-functor-compare)))

(define (functor? tree)
  (or (ast-call/map? tree) (ast-call/fold? tree)))

(define (extract-lists tree)
  (let* ((map? (o map-operation? ast-call-operation))
         (maps (filter map? (extract-functors tree)))
         (map-lists (map map->ast-list maps))
         (glists (extract-ast-type tree ast-list?)))
    (delete-duplicates glists ast-list-compare)))

(define (ast-functor-compare a b)
  (let ((a-op (ast-call-operation a))
        (b-op (ast-call-operation b))
        (name (lambda (x)
                (case-cond x
                  (fold-operation? fold-operation-name)
                  (map-operation? map-operation-name)))))
    (string=? ((name a-op) a-op) ((name b-op) b-op))))

(define (ast-list-compare a b)
  (string=? (ast-list-name a) (ast-list-name b)))

(define (ast-variable-compare a b)
  (and (string=? (ast-variable-name a) (ast-variable-name b))
       (eq? (ast-variable-arg a) (ast-variable-arg b))))

(define (ast-record-compare a b)
  (string=? (ast-record-name a) (ast-record-name b)))

(define (map->ast-list tree)
  (let* ((op (ast-call-operation tree))
         (name (map-operation-name op))
         (order (map-operation-order op))
         (type (glist-type tree))
         (size (glist-size tree)))
    (ast-list name type size '() (- order 0.5))))
