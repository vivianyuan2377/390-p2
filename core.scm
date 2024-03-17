; core.scm -- implements evaluation as well as special forms.
;
; Uses the error procedure defined in driver.scm and arity-error
; defined in primitives.scm. May also use other procedures defined in
; primitives.scm.
;
; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

(load "env.scm")
(load "error.scm")
(load "primitives.scm")

; Evaluates the given expression in the given environment.
(define (scheme-eval datum env)
  (cond ((or (number? datum)     ; self-evaluating objects
             (boolean? datum)
             (char? datum)
             (string? datum)
             (procedure? datum)
         )
         datum
        )
        ((and (pair? datum) (list? datum))  ; combinations
         ; The result of evaluating the first element must be a host
         ; procedure, representing either a client procedure or
         ; special form. Use procedure? to check this. Send the 'call
         ; message to the host procedure, along with the necessary
         ; arguments, to invoke it.
          (let ((proc (scheme-eval (car datum) env)))
            (cond
              ((procedure? proc)
                (apply proc (cons 'call (cons env (cdr datum))))
              )
              (else
                (error "not a procedure")
              )
            )
          )
        )
        ; Add a case for symbols (identifiers) here.
        ((symbol? datum)
          (if (env 'contains datum)
            (env 'get datum)
            (error "unknown identifier undefined")
          )
        )
        (else (error "cannot evaluate" datum))
  )
)


; Implements the begin form, which consists of a sequence of
; expressions.
(define (scheme-begin env . args)
  ; replace with your solution
  (if (>= (length args) 1)
    (scheme-begin-impl env args '())
    (error "Begin with 0 args") 
  )
)

(define (scheme-begin-impl env args res)
  (if (null? args)
    res
    (scheme-begin-impl env (cdr args) (scheme-eval (car args) env))
  )
)


; Implements a conditional, with a test, a then expression, and an
; optional else expression.
(define (scheme-if env . args)
  (cond 
    ((not (or (= (length args) 2) (= (length args) 3)))
      (error "Wrong number of args for if-statement")
    )
    ((scheme-eval (car args) env)
      (scheme-eval (cadr args) env)
    )
    ((not (null? (cddr args)))
      (scheme-eval (caddr args) env)
    )
  )
)


; Implements the quote form.
(define (scheme-quote env . args)
  (car args)
)


; Returns an object representing a user-defined lambda procedure with
; the given name, formal parameters, list of body expressions, and
; definition environment.
;
; The returned object should accept a message and any number of
; arguments.
;
; For the 'call message, it should:
; 1) Check whether the given number of arguments matches the expected
;    number (plus the initial environment argument). Use arity-error
;    to signal an error.
; 2) Evaluate the arguments in the given (dynamic) environment.
; 3) Extend the definition environment with a new frame.
; 4) Bind the formal parameters to the argument values in the new
;    frame.
; 5) Evaluate the body expressions in the new environment.
;
; For the 'to-string message, it should produce a string with the
; following format:
;   [lambda procedure <name>]
; where <name> is the name passed in to primitive-procedure.
(define (lambda-procedure name formals body parent-env)
  ; replace with your solution
  (lambda (message . args)
    (case message
      ((to-string)
        (string-append "[lambda procedure " (symbol->string name) "]")
      )
      ((call)
        (cond
          ((= (- (length args) 1) (length formals))
            (let* (
                (dynamic-env (car args))
                (evaluated-args (eval-args-with-env (cdr args) dynamic-env))
                (extended-env (frame parent-env))
              )
              (bind-formals formals evaluated-args extended-env)
              (apply scheme-begin (cons extended-env body))
            )
          )
          (else
            (arity-error name (length formals) (- (length args) 1))
          )
        )
      )
      (else
        (error "Unsupported primitive procedure message"))
    )
  )
)


(define (bind-formals formals evaluated-args extended-env)
  (cond
    ((not (null? formals))
      (extended-env 'insert (car formals) (car evaluated-args))
      (bind-formals (cdr formals) (cdr evaluated-args) extended-env)
    )
  )
)


; Implements the lambda form. Returns the newly created procedure.
;
; You are only required to support a fixed (non-variadic) number of
; arguments. You may choose to support other forms or signal an error.
;
; Use lambda-procedure to create the actual representation of the
; procedure.
(define (scheme-lambda env . args)
  ; replace with your solution
  (check-formals (car args))
  ;(check-body (cadr args)) TODO need to implement
  (lambda-procedure 'name (car args) (cdr args) env)
)

(define (has-duplicates? lst)
  (if (null? lst)
    #f
    (begin
      (if (lst-has-target (cdr lst) (car lst))
        #t
        (has-duplicates? (cdr lst)))
    )
  )
)

(define (lst-has-target lst target)
  (cond
    ((null? lst)
      #f)
    ((equal? (car lst) target)
      #t)
    (else 
      (lst-has-target (cdr lst) target))
  )
)

(define (check-formals formals)
  (if (or (not (or (symbol? formals) 
               (and (pair? formals) (every identifier? formals)) 
               (and (pair? formals) (not (every identifier? (cdr (reverse formals)))))
          ))
          (has-duplicates? formals)
      )
    (error "Invalid formals")
  )
)

(define (identifier? x)
  (and (symbol? x) (not (eq? x '())))
)

(define (every pred lst)
  (cond
    ((null? lst) #t)
    ((not (pred (car lst))) #f)
    (else (every pred (cdr lst)))
  )
)


; Implements the define form. Returns the symbol being defined.
;
; You must support both variable and procedure definitions.
; For procedure definitions, you are only required to support a fixed
; (non-variadic) number of arguments. You may choose to support other
; forms or signal an error.
;
; For procedure definitions, use lambda-procedure to create the actual
; representation of the procedure.
(define (scheme-define env . args)
  (cond
    ((not (= (length args) 2))
      (error "multiple arguments in define" args)
    )
    ((identifier? (car args))
      ; first form
      (env 'insert (car args) (scheme-eval (cadr args) env))
      (car args)
    )
    (else
      ; second form
      (if (or (not (list? (car args))) (not (identifier? (caar args))))
        (error "Define has no variable"))
      (check-formals (cdar args))
      ; (check-body (cadr args)) - implement!!
      (env 'insert (caar args) (scheme-lambda env (cdar args) (cadr args)))
      (caar args)
    )
  )
)


; Implement the mu form here.


; Returns an object respresenting the given library implementation for
; a special form.
;
; The returned object should accept a message and any number of
; arguments.
;
; For the 'call message, it should:
; 1) Invoke the library implementation on the arguments.
;
; For the 'to-string message, it should produce a string with the
; following format:
;   [syntax <name>]
; where <name> is the name passed in to primitive-procedure.
(define (special-form name native-impl)
  ; replace with your solution
  (lambda (message . args)
    (case message
      ((to-string)
        (string-append "[syntax " (symbol->string name) "]"))
      ((call)
        (apply native-impl args))
      (else
        (error "Unsupported special form message"))
    )
  )
)


; Adds special forms to the given environment and returns the
; environment.
(define (add-special-forms env)
  (env 'insert 'begin (special-form 'begin scheme-begin))
  (env 'insert 'if (special-form 'if scheme-if))
  (env 'insert 'quote (special-form 'quote scheme-quote))
  (env 'insert 'lambda (special-form 'lambda scheme-lambda))
  (env 'insert 'define (special-form 'define scheme-define))
  ; Insert the mu form here.
  env
)
