(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)
          (call/cc
           (lambda (+fk)
             (set! amb-fail
                   (lambda ()
                     (set! amb-fail +prev-amb-fail)
                     (+fk 'fail)))
             (+sk alt)))
          ...
          (+prev-amb-fail)))))))

(define-syntax my-sum
  (syntax-rules ()
    ((my-sum x ...)
     (+ (* 2 x)
        ...))))

(define-syntax add-em-up
  (syntax-rules ()
    ((add-em-up x ...)
     (+ (* 2 x) ...))))

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((?variable ?expression)
           ...)
       ?body
       ...)
     ((lambda (?variable ...)
        ?body
        ...)
      ?expression
            ...))))
