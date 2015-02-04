(load "pmatch.scm")

(define empty-env
	(lambda () '(empty environment)))
	
(define extend-env
	(lambda(x a env)
		(lambda(y) 
			(if (eqv? x y) a (env y)))))
	
(define apply-env
	(lambda (env y) 
		(env y)))
	
(define apply-closure
	(lambda (rator rand)
		(rator rand)))

;**********************************CALL BY VALUE*******************************
(define val-of-cbv
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
	  [`,x (guard (symbol? x)) (unbox (apply-env env x))]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
	  [`(set! ,x ,e) (set-box! (apply-env env x) (val-of-cbv e env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))
									  
(define closure-cbv
	(lambda (x body env)
		(lambda (arg)
			(val-of-cbv body (extend-env x arg env)))))
									  
;*******************************CALL BY REFERENCE******************************
(define val-of-cbr
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
	  [`,x (guard (symbol? x)) (unbox (apply-env env x))]
	  [`(,rator ,x)(guard (symbol? x)) ((val-of-cbr rator env)
										(apply-env env x))]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
	  [`(set! ,x ,e) (set-box! (apply-env env x) (val-of-cbr e env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(lambda (,x) ,body) (closure-cbr x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

(define set-box!
	(lambda (box val)
		(set-cdr! box val)))
		
(define unbox
	(lambda (box)
		(cdr box)))
		
(define box
	(lambda (y)
		(cons 'box y))) 
									  
(define closure-cbr
	(lambda (x body env)
		(lambda (arg)
			(val-of-cbr body (extend-env x arg env)))))
	
;*********************************CALL BY NAME********************************  
(define val-of-cbname
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
	  [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
	  [`(+ ,n1 ,n2) (+ (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (closure-cbname x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda () (val-of-cbname rand env))))])))
									  
(define closure-cbname
	(lambda (x body env)
		(lambda (arg)
			(val-of-cbname body (extend-env x arg env)))))
									  
;*********************************CALL BY NEED***************************** 
(define val-of-cbneed
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
	  [`,x (guard (symbol? x)) (unbox-need (apply-env env x))]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (closure-cbneed x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda () (val-of-cbneed rand env))))])))
									  
(define closure-cbneed
	(lambda (x body env)
		(lambda (arg)
			(val-of-cbneed body (extend-env x arg env)))))
			
(define unbox-need
	(lambda (box)
		(let ([val ((unbox box))])
			(begin
				(set-box! box (lambda () val))
				val))))
				
;*******************************CALL BY DENOTATION***********************************
(define val-of-cbden
  (lambda (exp env)
    (pmatch exp
      [`,b (guard (boolean? b)) b]
      [`,n (guard (number? n)) n]
	  [`,x (guard (symbol? x)) ((unbox (apply-env env x)))]
      [`(zero? ,n) (zero? (val-of-cbden n env))]
      [`(sub1 ,n) (sub1 (val-of-cbden n env))]
      [`(* ,n1 ,n2) (* (val-of-cbden n1 env) (val-of-cbden n2 env))]
	  [`(+ ,n1 ,n2) (+ (val-of-cbden n1 env) (val-of-cbden n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbden test env)
                                    (val-of-cbden conseq env)
                                    (val-of-cbden alt env))]
      [`(random ,n) (random (val-of-cbden n env))]
      [`(lambda (,x) ,body) (closure-cbden x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbden rator env)
                                      (val-of-cbden rand env))])))
									  
(define closure-cbden
	(lambda (x body env)
		(lambda (arg)
			(val-of-cbden body (extend-env x arg env)))))
