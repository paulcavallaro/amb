;; Placeholder definition for amb-fail
(define amb-fail '*)

;; Initialize the amb-fail
(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            (error "amb tree exhausted")))))

(initialize-amb-fail)

;; The basic amb operator
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

;; Tutorial of define-syntax
(define-syntax double-sum
  (syntax-rules ()
    ((my-sum x ...)
     (+ (* 2 x) ...))))

;; More advanced usage of syntax-rules
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((?variable ?expression) ...)
             ?body ...)
     ((lambda (?variable ...)
        ?body ...)
      ?expression
      ...))))

;; Direct style
(define myfun
  (lambda (x)
    (* (+ x 1) 5)))

;; Continuation Passing Style (CPS)
(define (*& x y k)
  (k (* x y)))

;; CPS
(define (+& x y k)
  (k (+ x y)))

;; CPS
(define myfun-cps
  (lambda (x k)
    (+& x 1 (lambda (x-plus-1)
              (*& x-plus-1 5 k)))))

;; Generalize our *& and +& operators
(define (cps-prim f)
  (lambda args
    (let ((r (reverse args)))
      ((car r) (apply f (reverse (cdr r)))))))

(define *& (cps-prim *))
(define +& (cps-prim +))

