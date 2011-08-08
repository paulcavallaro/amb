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
(define (myfun x)
  (* (+ x 1) 5))

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

;; call/cc example
(define (do-nothing)
  (call/cc (lambda (k) (k))))

;; call/cc example
(define (return-cc)
  (call/cc (lambda (k) k)))

;; call/cc example
(define (my-call x)
  (+ 2 (call/cc
        (lambda (k)
          (+ 3 (k x))))))

;; call/cc example
(define (call-again x y)
  (call/cc
   (lambda (top-k)
     (top-k x y))))

;; call/cc in let
(define (deja-vu)
  (let ((x (call/cc (lambda (k) k))))
    (x (lambda (ignore) "vu"))))

;; So what happens?
;; 
;; (1) x is set to the current continuation at that moment. That is if
;; you were to call x (the curent continuation) you'd actually be
;; rebinding x to whatever you passed it.
;;
;; (2) We rebind x in exactly this way by rebinding it to the
;; anonymous function which ignores its only argument and returns "vu"
;;
;; (3) Now x, rebound to that anonymous function of ours, gets called
;; with that anonymous function, but x ignores it and just returns "vu"
;;
;; Neat huh?

;; One more lesson to keep in mind is you can always have side effects
;; in your let bindings
(define (side-effect-let)
  (let ((x ((lambda (y)
              (display "side effect in binding")
              (newline) y)
            +)))
    (x 2 3)))

;; Now onto the big boy, the yin yang puzzle of call/cc!

;; Side effect display @ 
(define (yin-display cc)
  (display #\@)
  cc)

;; Side effect display *
(define (yang-display cc)
  (display #\*)
  cc)

;; yin yang call/cc annotated
(define (yin-yang-annotated) 
  (let ((yin
         (yin-display
          (call/cc (lambda (k) k))))
        (yang
         (yang-display
          (call/cc (lambda (k) k)))))
    (yin yang)))

;; Let's take a look at yin-yang-annotated:
;; 
;; yin first gets bound to the somewhat confusing term:
;;

(yin-display
 (call/cc (lambda (k) k)))

;; So yin is the application of yin-display to the captured current
;; continuation, k_0. yin-display is applied for the side effect of
;; printing the character "@", and then returns the same continuation
;; k_0.
;;
;; The current continuation k_0 is actually in the middle of the let
;; binding defining yin. We can think about calling k_0 with arg as
;; replacing that entire "(call/cc (lambda (k) k))" with arg.
;;
;; Next yang is defined exactly as yin, except using yang-display to
;; print the character "*".
;;
;; Now we get to the let body. yin, which is bound to k_0, is called
;; with yang. We can use the mental trick of replacing the call/cc
;; form with yang.
;;
;; This current stack, stack_1, is also in the middle of a let
;; binding, but defining yang. We again can think of calling k_1 with arg the
;; same way as above, as replacing that "call/cc" bit with arg.
;;
;; So now to go through the first level:
;;
;; (1) yin gets bound to k_0 which has the side effect of displaying
;; the "@" character.
;;
;; (2) yang gets bound to k_1 which has the side effect of displaying
;; the "*" character.
;;
;; (3) yin is called with yang. Here is where things start to get
;; messy =p. yin is bound to k_0, so we can use the mental trick of
;; replacing the "call/cc" bit in the yin binding with yang, aka
;; k_1. So now yin is bound to 
;;

;; Now we can rewrite it anonymizing both yin and yang-display to make
;; it slightly harder to understand, and hence make ourselves feel
;; cooler for understanding it =p

;; yin yang call/cc original
(define (yin-yang) 
  (let ((yin
         ((lambda (cc) (display #\@) cc)
          (call/cc (lambda (k) k))))
        (yang
         ((lambda (cc) (display #\*) cc)
          (call/cc (lambda (k) k)))))
    (yin yang)))



(((lambda (cc) (display #\@) cc)
  (call/cc (lambda (k) k)))
 ((lambda (cc) (display #\*) cc)
  (call/cc (lambda (k) k))))
