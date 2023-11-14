(import scheme
        (chicken format)
        (chicken string)
        (chicken syntax)
        srfi-1
        srfi-13)

(define (self-evaluating? x) 
  (or (boolean? x)
      (fixnum? x)
      (char? x)
      (string? x)))

(define (variable? x)
  (symbol? x))

(define (pair-with-symbol? x sym)
  (if (pair? x)
      (and (symbol? (car x)) (eq? (car x) (string->symbol sym)))
      (#f)))

(define (quoted? x) (pair-with-symbol? x "quote"))

(define (compound-procedure? x) (pair-with-symbol? x "procedure"))

(define (procedure-parameters p) (cadr p))

(define (text-of-quotation exp) (cadr exp))

(define (define? x) (pair-with-symbol? x "define"))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (assignment? x) (pair-with-symbol? x "set!"))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (lambda? x) (pair-with-symbol? x "lambda"))

(define (if? x) (pair-with-symbol? x "if"))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? x) (pair-with-symbol? x "begin"))

(define (cond? x) (pair-with-symbol? x "cond"))

(define (let? x) (pair-with-symbol? x "let"))

(define (and? x) (pair-with-symbol? x "and"))

(define (or? x) (pair-with-symbol? x "or"))

(define (application? x) (and (pair? x) (not (null? x))))

(define (operator x) (car x))

(define (operands x) (cdr x))

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exp env ind) 
  (if (null? exp)
      '()
      (cons (eval-with-trace (first-operand exp) env (+ 2 ind))
            (list-of-values (rest-operands exp) env ind))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (primitive-procedure? proc) 
  (if (procedure? proc)
      (let ((proc-info (->string (procedure-information proc))))
       (or 
         (fixnum? (string-contains proc-info "scheme#"))
         (fixnum? (string-contains proc-info "C_"))))
      (#f)))

(define (first-frame env) (car env))

(define (enclosing-environment env) (cdr env))

(define the-empty-environment '())

(define (frame-variables frame) (car frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (apply-primitive-procedure proc args)
        (apply proc args))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-without-trace (first-exp exps) env))
        (else (eval-without-trace (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (eval-assignment exp env ind)
  (set-variable-value! (assignment-variable exp)
                       (eval-without-trace (assignment-value exp) env ind)
                       env)
  'ok)

(define (eval-definition exp env ind)
  (define-variable! (definition-variable exp)
                    (eval-without-trace (definition-value exp) env ind)
                    env)
  'ok)

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (current-environment)
        (let ((env-procs (map (lambda (x) (car x)) (##sys#current-environment))))
             (list (cons (map (lambda (x) (quasiquote (unquote x))) env-procs)
                         (map (lambda (x) (eval x)) env-procs)))))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

 ;; let expression 
 (define (let-vars exp) (map car (cadr exp))) 
 (define (let-inits exp) (map cadr (cadr exp))) 
 (define (let-body exp) (cddr exp)) 
  
 (define (let->combination exp) 
   (list (make-lambda (let-vars exp) (let-body exp)) 
         (let-inits exp)))

(define (eval-without-trace exp env ind)
  (eval-with-trace-handler exp env ind))

(define-syntax display_input_as_entered
  (syntax-rules ()
    ((_ exp) 'exp)))

(define (eval-with-trace exp env ind)
  (let ((result (eval-with-trace-handler exp env ind)))
       (print (format "~A" (make-string ind #\ )) exp " ==> " result)
       result))

(define (eval-with-trace-handler exp env ind) 
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((define? exp) (eval-definition exp env ind))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-with-trace (cond->if exp) env))
        ((let? exp) (eval-without-trace (let->combination exp) env ind))
        ;((let? exp) (eval-let (clauses exp) env))
        ;((and? exp) (eval-and (clauses exp) env))
        ;((or? exp) (eval-or (clauses exp) env))
        ((application? exp) (apply-with-trace (eval-with-trace (operator exp) env ind )
                                              (list-of-values (operands exp) env ind)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (apply-with-trace procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                          (procedure-parameters procedure)
                          arguments
                          (procedure-environment procedure))))
        (else  (error "Unknown procedure type -- APPLY" procedure))))
