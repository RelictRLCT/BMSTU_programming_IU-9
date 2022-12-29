(define-syntax my-let
  (syntax-rules ()
    ((my-let ((per expr) ...) exprs ...)
     ((lambda (per ...)
        (begin exprs ...))
      expr ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((per expr)) exprs ...)
     (my-let ((per expr)) exprs ...))
    ((my-let* ((per expr) (per2 expr2) ...) exprs ...)
     (my-let ((per expr))
             (my-let* ((per2 expr2) ...) exprs ...)))))
