(softmodule
  public: ((not-glist-error
            (lambda (x location #!optional caller)
              (print (format "At -> ~a" location))
              (print (format "From -> ~a" caller))
              (print (format "Error: ~a is not a glist" (ast-expand/local x)))
              (print (format "Syntax tree: ~a" x))
              (if (ast-call? x)
                  (print (format "Operation type: ~a" (ast-call-operation x))))
              (quit)))

           (check-kernel-errors
            (lambda (name in out body)
              (let* ((in-types (map ast-arg-type in))
                     (out-type (ast-arg-type out))
                     (param-types (flatlist in-types out-type))
                     (return-type (ast-type body))
                     (checks (list (check-return name return-type out-type)))
                     (errors (remove not checks)))
                (if (not (null? errors))
                    (for-each print errors))))))

  private: ((check-return
             (lambda (name return-type output-type)
               (if (and return-type (not (string=? output-type return-type)))
                   (format (conc "Warning: Return type of ~a does not match "
                                 "declaration~%~aDeclared: ~a ~%Returned: ~a")
                           name
                           return-type
                           output-type)
                   #f)))))
