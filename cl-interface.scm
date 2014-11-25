(use srfi-4)
(import foreign)

#>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <time.h>
<#

;; temporary
(define-foreign-type clock_t size_t)

(define-foreign-type cl_event (c-pointer (struct "_cl_event")))
(define-foreign-type cl_context (c-pointer (struct "_cl_context")))
(define-foreign-type cl_command_queue (c-pointer (struct "_cl_command_queue")))
(define-foreign-type cl_platform_id (c-pointer (struct "_cl_platform_id")))
(define-foreign-type cl_device_id (c-pointer (struct "_cl_device_id")))
(define-foreign-type cl_kernel (c-pointer (struct "_cl_kernel")))
(define-foreign-type cl_program (c-pointer (struct "_cl_program")))
(define-foreign-type cl_mem (c-pointer (struct "_cl_mem")))

(define-external context cl_context)
(define-external queue cl_command_queue)
(define-external deviceID cl_device_id)
(define-external platformID cl_platform_id)

(define-record-type <cl-kernel>
  (cl-kernel program shared)
  cl-kernel?
  (program cl-kernel-program)
  (shared cl-kernel-shared))

(define clock-time
  (foreign-safe-lambda* clock_t ()
    "C_return(clock());"))

(define clock-difference
  (foreign-safe-lambda* double ((clock_t t1) (clock_t t2))
    "C_return(((double)t2 - (double)t1) * 1.0e-6);"))

(define (invoker-with-kernel inv)
  (let* ((call (invoker->ast-call inv (list 0)))
         (op (ast-call-operation call)))
    (if (user-function-kernel op)
        inv
        (let* ((cname (user-function-name op))
               (proc (user-function-procedure op))
               (shared (extract-shared-instances (gprocedure-body proc)))
               (program (cl-kernel-build (gcompile cname proc))))
          (call-invoker/user proc cname (cl-kernel program shared))))))

(define cl-begin
  (foreign-lambda* int ()
    "cl_uint entries = 1;"
    "cl_int error = 0;"
    "error = clGetPlatformIDs(entries, &platformID, NULL);"
    "if (error != 0) { printf(\"platform id %d\\n\", error); exit(1); }"
    "clGetDeviceIDs(platformID, CL_DEVICE_TYPE_DEFAULT, entries, &deviceID, NULL);"
    "if (error != 0) { printf(\"device id %d\\n\", error); exit(1); }"
    "context = clCreateContext(0, 1, &deviceID, 0, 0, &error);"
    "if (error != 0) { printf(\"create context %d\\n\", error); exit(1); }"
    "queue = clCreateCommandQueue(context, deviceID, 0, &error);"
    "if (error != 0) { printf(\"command queue %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-release-context
  (foreign-lambda* void ()
    "clReleaseContext(context);"))

(define cl-release-kernel
  (foreign-safe-lambda* void ((cl_kernel k))
    "clReleaseKernel(k);"))

(define (cl-end)
  (release-all-gbuffers)
  (cl-release-context)
  (reset-stateful-objects))

(define cl-kernel-build
  (foreign-lambda* cl_kernel (((const c-string) src))
    "int error = 0;"
    "cl_program program = clCreateProgramWithSource(context, 1, &src, 0, &error);"
    "if (error != 0) { printf(\"program make %d\\n\", error); exit(1); }"
    "error = clBuildProgram(program, 0, 0, 0, 0, 0);"
    "if (error != 0) { printf(\"program build %d\\n\", error); exit(1); }"
    "cl_kernel k = clCreateKernel(program, \"func_kernel\", &error);"
    "if (error != 0) { printf(\"kernel make %d\\n\", error); exit(1); }"
    "clReleaseProgram(program);"
    "C_return(k);"))


(define cl-create-buffer/read
  (foreign-lambda* cl_mem ((size_t bytes) (c-pointer host_ptr))
    "int error = 0;"
    "cl_mem rtn = clCreateBuffer(context, CL_MEM_READ_ONLY, bytes, NULL, &error);"
    "if (error != 0) { printf(\"buffer read %d\\n\", error); exit(1); }"
    "C_return(rtn);"))

(define cl-create-buffer/write
  (foreign-lambda* cl_mem ((size_t bytes) (c-pointer host_ptr))
    "int error = 0;"
    "cl_mem rtn = clCreateBuffer(context, CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR, bytes, host_ptr, &error);"
    "if (error != 0) { printf(\"buffer write %d\\n\", error); exit(1); }"
    "C_return(rtn);"))

(define cl-create-buffer
  (foreign-lambda* cl_mem ((size_t bytes) (c-pointer host_ptr))
    "int error = 0;"
    "cl_mem rtn = clCreateBuffer(context, CL_MEM_USE_HOST_PTR, bytes, host_ptr, &error);"
    "if (error != 0) { printf(\"buffer write %d\\n\", error); exit(1); }"
    "C_return(rtn);"))

(define cl-set-kernel-arg
  (foreign-safe-lambda* int ((cl_kernel kernel) (int arg_num) (cl_mem arg))
    "int error = clSetKernelArg(kernel, arg_num, sizeof(cl_mem), &arg);"
    "if (error != 0) { printf(\"set kernel arg %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-set-kernel-arg/count
  (foreign-safe-lambda* int ((cl_kernel kernel) (int arg_num) (unsigned-int arg))
    "int error = clSetKernelArg(kernel, arg_num, sizeof(unsigned int), &arg);"
    "if (error != 0) { printf(\"set kernel arg %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-enqueue-nd-range-kernel
  (foreign-safe-lambda* int ((cl_kernel kernel) (size_t gblsize))
    "size_t local = 64;"
    "int error = clEnqueueNDRangeKernel(queue, kernel, 1, NULL, &gblsize, NULL, 0, NULL, NULL);"
    "if (error != 0) { printf(\"enqueue nd range %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-enqueue-buffer/write
  (foreign-safe-lambda* int ((cl_mem arg) (size_t bytes) (c-pointer host_ptr))
    "int error = clEnqueueWriteBuffer(queue, arg, CL_TRUE, 0, bytes, host_ptr, 0, NULL, NULL);"
    "if (error != 0) { printf(\"enqueue write %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-enqueue-buffer/read
  (foreign-safe-lambda* int ((cl_mem arg) (size_t bytes) (c-pointer ptr))
    "int error=clEnqueueReadBuffer(queue, arg, CL_TRUE, 0, bytes, ptr, 0, NULL, NULL);"
    "if (error != 0) { printf(\"enqueue read %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-finish
  (foreign-safe-lambda* int ()
    "int error = clFinish(queue);"
    "if (error != 0) { printf(\"finish %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define cl-release-mem-object
  (foreign-safe-lambda* int ((cl_mem object))
    "int error = clReleaseMemObject(object);"
    "if (error != 0) { printf(\"release mem %d\\n\", error); exit(1); }"
    "C_return(error);"))

(define s32vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((s32vector v))
    "C_return((cl_int*)v);"))

(define u32vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((u32vector v))
    "C_return((cl_uint*)v);"))

(define s16vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((s16vector v))
    "C_return((cl_short*)v);"))

(define u16vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((u16vector v))
    "C_return((cl_ushort*)v);"))

(define s8vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((s8vector v))
    "C_return((cl_char*)v);"))

(define u8vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((u8vector v))
    "C_return((cl_uchar*)v);"))

;; (define s64vector->cl-pointer
;;   (foreign-safe-lambda* c-pointer ((s64vector v))
;;     "C_return((cl_long*)v);"))

;; (define u64vector->cl-pointer
;;   (foreign-safe-lambda* c-pointer ((u64vector v))
;;     "C_return((cl_ulong*)v);"))

(define f32vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((f32vector v))
    "C_return((cl_float*)v);"))

(define f64vector->cl-pointer
  (foreign-safe-lambda* c-pointer ((f64vector v))
    "C_return((cl_double*)v);"))

;; (define cl_bool-size
;;   (foreign-safe-lambda* size_t ()
;;     "C_return(sizeof(cl_bool));"))

(define cl_int-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_int));"))

(define cl_uint-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_uint));"))

;; (define cl_long-size
;;   (foreign-safe-lambda* size_t ()
;;     "C_return(sizeof(long));"))

;; (define cl_ulong-size
;;   (foreign-safe-lambda* size_t ()
;;     "C_return(sizeof(ulong));"))

(define cl_char-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_char));"))

(define cl_uchar-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_uchar));"))

(define cl_short-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_short));"))

(define cl_ushort-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_ushort));"))

(define cl_float-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_float));"))

(define cl_double-size
  (foreign-safe-lambda* size_t ()
    "C_return(sizeof(cl_double));"))

(define type-byte-dictionary
  (list (cons "int" (cl_int-size))
        (cons "uint" (cl_uint-size))
        (cons "bool" (cl_ushort-size))
        ;; (cons "long" (cl_long-size))
        ;; (cons "ulong" (cl_ulong-size))
        (cons "char" (cl_char-size))
        (cons "uchar" (cl_uchar-size))
        (cons "short" (cl_short-size))
        (cons "ushort" (cl_ushort-size))
        (cons "float" (cl_float-size))
        (cons "double" (cl_double-size))))

(define type-vector-dictionary
  (list (cons "int" list->s32vector)
        (cons "uint" list->u32vector)
        (cons "bool" list->u8vector) ;; has no type.  Find out solution!
        ;; (cons "long" list->s64vector) ;; does not exist
        ;; (cons "ulong" list->u64vector) ;; does not exist
        (cons "char" (o list->s8vector (lambda (x) (map char->integer x))))
        (cons "uchar" list->u8vector)
        (cons "short" list->s16vector)
        (cons "ushort" list->u16vector)
        (cons "float" list->f32vector)
        (cons "double" list->f64vector)))

(define type-list-dictionary
  (list (cons "int" s32vector->list)
        (cons "uint" u32vector->list)
        (cons "bool" u8vector->list) ;; has no type.  Find out solution!
        ;; (cons "long" s64vector->list) ;; does not exist
        ;; (cons "ulong" u64vector->list) ;; does not exist
        (cons "char" (o (lambda (x) (map integer->char x)) s8vector->list))
        (cons "uchar" u8vector->list)
        (cons "short" s16vector->list)
        (cons "ushort" u16vector->list)
        (cons "float" f32vector->list)
        (cons "double" f64vector->list)))

(define type-pointer-dictionary
  (list (cons "int" s32vector->cl-pointer)
        (cons "uint" u32vector->cl-pointer)
        (cons "bool" u8vector->cl-pointer) ;; does not exist
        ;; (cons "long" s64vector->cl-pointer) not allowed
        ;; (cons "ulong" u64vector->cl-pointer) not allowed
        (cons "char" s8vector->cl-pointer)
        (cons "uchar" u8vector->cl-pointer)
        (cons "short" s16vector->cl-pointer)
        (cons "ushort" u16vector->cl-pointer)
        (cons "float" f32vector->cl-pointer)
        (cons "double" f64vector->cl-pointer)))

(define type-length-dictionary
  (list (cons "int" s32vector-length)
        (cons "uint" u32vector-length)
        (cons "bool" u8vector-length) ;; does not exist
        ;; (cons "long" s64vector-length) not allowed
        ;; (cons "ulong" u64vector-length) not allowed
        (cons "char" s8vector-length)
        (cons "uchar" u8vector-length)
        (cons "short" s16vector-length)
        (cons "ushort" u16vector-length)
        (cons "float" f32vector-length)
        (cons "double" f64vector-length)))

(define vector-make-dictionary
  (list (cons "int" make-s32vector)
        (cons "uint" make-u32vector)
        (cons "bool" make-u8vector) ;; does not exist
        ;; (cons "long" s64vector-length) not allowed
        ;; (cons "ulong" u64vector-length) not allowed
        (cons "char" make-s8vector)
        (cons "uchar" make-u8vector)
        (cons "short" make-s16vector)
        (cons "ushort" make-u16vector)
        (cons "float" make-f32vector)
        (cons "double" make-f64vector)))

(define vector-set-dictionary
  (list (cons "int" s32vector-set!)
        (cons "uint" u32vector-set!)
        (cons "bool" u8vector-set!) ;; does not exist
        ;; (cons "long" s64vector-length) not allowed
        ;; (cons "ulong" u64vector-length) not allowed
        (cons "char" s8vector-set!)
        (cons "uchar" u8vector-set!)
        (cons "short" s16vector-set!)
        (cons "ushort" u16vector-set!)
        (cons "float" f32vector-set!)
        (cons "double" f64vector-set!)))

(define (byte-size type)
  (cdr (assoc type type-byte-dictionary)))

(define (list->type-vector type)
  (cdr (assoc type type-vector-dictionary)))

(define (type-vector->list type)
  (cdr (assoc type type-list-dictionary)))

(define (vector->pointer type)
  (cdr (assoc type type-pointer-dictionary)))

(define (type-vector-length type)
  (cdr (assoc type type-length-dictionary)))

(define (type-vector-make type)
  (cdr (assoc type vector-make-dictionary)))

(define (type-vector-set! type)
  (cdr (assoc type vector-set-dictionary)))

(define (vector-bytes vec)
  (case-cond vec
    (s32vector? (cl_int-size))
    (u32vector? (cl_uint-size))
    (s8vector? (cl_char-size))
    (u8vector? (cl_uchar-size))
    (s16vector? (cl_short-size))
    (u16vector? (cl_ushort-size))
    (f32vector? (cl_float-size))
    (f64vector? (cl_double-size))))

(define (vector-type vec)
  (case-cond vec
    (s32vector? "int")
    (u32vector? "uint")
    (s8vector? "char")
    (u8vector? "uchar")
    (s16vector? "short")
    (u16vector? "ushort")
    (f32vector? "float")
    (f64vector? "double")))

(define-record-type cl-buffer
  (cl-buffer type mem size bytes vector)
  cl-buffer?
  (size cl-buffer-size)
  (bytes cl-buffer-bytes)
  (type cl-buffer-type)
  (vector cl-buffer-vector)
  (mem cl-buffer-mem set-cl-buffer-mem!))

(define (gdata-construct type value #!optional readonly?)
  (let* ((vec ((list->type-vector type) (list value)))
         (mk (if readonly? vector->cl-buffer vector->cl-buffer/mutable))
         (buffer (mk vec))
         (name (assign-shared-name))
         (rtn (ast-shared name buffer #t)))
    (add-buffer rtn)
    rtn))

(define (set-gbuffer-internal! s buffer)
  (let ((b (normalize-shared buffer)))
    (set-ast-shared-buffer! s b)))

(define (list->gbuffer-internal type lst #!optional readonly?)
  (vector->gbuffer ((list->type-vector type) lst) readonly?))

(define (vector->cl-buffer vec)
  (let* ((type (vector-type vec))
         (n ((type-vector-length type) vec))
         (bytes (* n (vector-bytes vec)))
         (host ((vector->pointer type) vec))
         (mem (cl-create-buffer/read bytes host)))
    (cl-enqueue-buffer/write mem bytes host)
    (cl-buffer type mem n bytes vec)))

(define (vector->cl-buffer/mutable vec)
  (let* ((type (vector-type vec))
         (n ((type-vector-length type) vec))
         (bytes (* n (vector-bytes vec)))
         (host ((vector->pointer type) vec))
         (mem (cl-create-buffer bytes host)))
    (cl-enqueue-buffer/write mem bytes host)
    (cl-buffer type mem n bytes vec)))

(define (cl-buffer/write type size)
  (let ((mk (lambda (t s)
              (let* ((vec ((type-vector-make t) s 0 #t #t))
                     (bytes (* s (vector-bytes vec)))
                     (host ((vector->pointer t) vec))
                     (mem (cl-create-buffer bytes ((vector->pointer t) vec))))
                (cl-buffer t mem s bytes vec)))))
    (if (assoc type (alias-record-dictionary))
        (map (lambda (x) (mk x size)) (record-alias->base-types type))
        (list (mk type size)))))

(define (cl-buffer-host buffer)
  ((vector->pointer (cl-buffer-type buffer)) (cl-buffer-vector buffer)))

(define (cl-run! kernel gproc input-buffers out)
  (let* ((output-buffers (if (list? out) out (list out)))
         (i (new-counter 0))
         (n (cl-buffer-size (car input-buffers)))
         (output-bytes (map cl-buffer-bytes output-buffers))
         (local-size 64)
         (global-size (ceiling (* (/ n local-size) local-size)))
         (host-outputs (map cl-buffer-host output-buffers))
         (device-read-args (map cl-buffer-mem input-buffers))
         (device-write-args (map cl-buffer-mem output-buffers))
         (device-args (flatlist device-read-args device-write-args))
         (enqueue-read-buffer (lambda (d b h) (cl-enqueue-buffer/read d b h)))
         (set-kernel-args (lambda (d) (cl-set-kernel-arg kernel (i) d))))
    (define start (clock-time))
    (map set-kernel-args device-args)
    (cl-enqueue-nd-range-kernel kernel global-size)
    (map enqueue-read-buffer device-write-args output-bytes host-outputs)
    (cl-finish)))
