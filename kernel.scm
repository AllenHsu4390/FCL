;;;; File: kernel.scm
;;;; Description: functions relating to creating a OpenCL kernel
;;;; Author: Allen Hsu

(define (kernel-has-double? inputs output)
  ;; checks if any of the inputs or outputs are of type double
  ;; doubles require extra extension declaring in OpenCL
  (let ((o-type (ast-arg-type output))
        (i-types (map ast-arg-type inputs)))
    (or (string= "double" o-type) (member "double" i-types))))

(define (base-type-access-format arg base-type)
  (format "~a[i]=rtn.~a" (ast-arg-name arg) base-type))

(define (kernel-statements inputs output instruction declarations shared)
  ;; create c like statements for the OpenCL kernel using the user ast expanded
  ;; glanguage s expressions
  (let* ((record (assoc (ast-arg-type output) (alias-record-dictionary)))
         (output-statement (if record
                               (map base-type-access-format
                                    (record-arg->base-args output)
                                    (grecord-type-attributes (cdr record)))
                               (format "~a[i]=rtn" (ast-arg-name output)))))
    (flatlist
     "int i = get_global_id(0)"
     declarations
     (format "~a rtn=~a;" (ast-arg-type output) instruction)
     output-statement)))

(define (global-format arg)
  (format "global ~a* ~a" (ast-arg-type arg) (ast-arg-name arg)))

(define (kernel-parameters inputs output shared)
  ;; returns string specifying the parameters of the kernel
  (flatlist
   (map global-format inputs)
   (map (lambda (s) (format "global ~a* ~a" (ast-shared-type s) (ast-shared-name s)))
        shared)
   (if (assoc (ast-arg-type output) (alias-record-dictionary))
       (map global-format (record-arg->base-args output))
       (global-format output))))

(define enable-double "#pragma OPENCL EXTENSION cl_khr_fp64 : enable
")

(define (kernel-source-code inputs
                            output
                            instruction
                            top-level-declarations
                            shared
                            local-declarations)
  (let* ((header (flatlist "typedef void* record;"
                           top-level-declarations)))
    (make-cprogram header: header
                   type: "kernel void"
                   name: "func_kernel"
                   parameters: (kernel-parameters inputs output shared)
                   body: (kernel-statements inputs
                                            output
                                            instruction
                                            local-declarations
                                            shared))))
