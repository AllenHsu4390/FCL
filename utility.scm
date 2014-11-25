;;;; File: utility.scm
;;;; Description: utility functions
;;;; Author: Allen Hsu

(use data-structures srfi-1 srfi-13 extras)

(define (set-difference s1 s2)
  (cond ((null? s1) '())
        ((not (memq (car s1) s2))
         (cons (car s1) (set-difference (cdr s1) s2)))
        (else (set-difference (cdr s1) s2))))

(define (index-of x lst #!optional pred?)
  (let ((check (if pred? pred? eq?)))
    (list-index (lambda (e) (check x e)) lst)))

(define (indices-of x lst #!optional pred?)
  (letrec ((check (if pred? pred? eq?))
           (r (lambda (i accum)
                (if (= i (length lst))
                    accum
                    (r (+ 1 i) (list accum (if (check (list-ref lst i) x)
                                               i
                                               '())))))))
    (flatten (r 0 '()))))

(define (find-indices find lst #!optional pred?)
  (letrec ((check (if pred? pred? eq?))
           (r
            (lambda (l accum)
              (if (null? l)
                  accum
                  (r (cdr l)
                     (append accum (indices-of (car l) lst check)))))))
    (delete-duplicates (r find '()))))

(define-syntax string-quote
  (syntax-rules ()
    ((_ x)
     (symbol->string (quote x)))))

(define-syntax case-cond
  (syntax-rules (else)
    ((_ e (c r) ... (else d))
     (cond ((c e) r) ... (else d)))
    ((_ e (c r) ...)
     (cond ((c e) r) ...))))

(define (new-counter start)
  (lambda ()
    (let ((rtn start))
      (set! start (+ start 1))
      rtn)))

(define-syntax softmodule-publicize
  (syntax-rules ()
    ((_ lst p ...)
     (begin
       (define pcounter (new-counter 0))
       (define p (list-ref lst (pcounter)))
       ...))))

(define-syntax softmodule
  (syntax-rules ()
    ((_ public: ((pname1 proc1) ...)
        private: ((pname2 proc2) ...))
     (softmodule-publicize (letrec ((pname2 proc2)
                                    ...
                                    (pname1 proc1)
                                    ...)
                             (list proc1 ...))
                           pname1
                           ...))))

(define-syntax append-any
  ;; append an arbitary number of list, atoms
  (syntax-rules ()
    ((append-any first rest ...)
     (append (if (list? first) first (list first))
             (if (list? rest) rest (list rest))
             ...))))

(define (flatlist . args)
  ;; create a flattened list from an aribitary number of arguments
  (flatten args))

(define (substrings->string lst)
  ;; generates a string with a list of substrings
  (letrec ((r (lambda (lst accum)
                (if (null? lst)
                    accum
                    (r (cdr lst) (conc accum (car lst)))))))
    (r lst "")))

(define (replace-substring before after str)
  ;; replace substring with second substring
  (let ((idx (string-contains str before)))
    (if idx
        (string-replace str after idx (+ idx (string-length before)))
        str)))

(define (replace-all-substring before after str)
  ;; replace all substring with second substring
  (define (r b a s)
    (if (string= s (replace-substring b a s))
        s
        (replace-all-substring b a (replace-substring b a s))))
  (r before after str))

(define (delete-substring sub str)
  ;; deletes all instances substring from string
  (replace-all-substring sub "" str))

(define (delete-substrings lst str)
  ;; deletes all instances of lst from string
  (fold (lambda (curr accum)
          (delete-substring curr accum))
        str
        lst))

(define (splice-element e lst)
  ;; add e in between every element in list
  (letrec ((r (lambda (e lst accum)
                (if (null? (cdr lst))
                    (append-any accum lst)
                    (r e (cdr lst) (append-any accum (car lst) e))))))
    (r e lst '())))

(define (list->csv lst)
  ;; converts a list to a csv string
  (if (null? lst)
      ""
      (substrings->string (splice-element "," lst))))

(define (list->lines lst)
  ;; converts a list to a string with interplaced newlines
  (if (null? lst)
      ""
      (substrings->string (splice-element #\newline lst))))

(define (write-to-file string file)
  ;; write string to file
  (let ((port (open-output-file file)))
    (display string port)
    (close-output-port port))
  string)
