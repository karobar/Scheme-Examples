(load "pmatch.scm")

;1.Complete the definition of list-ref with a naturally-recursive implementation 
;of nth-cdr, so that the following work correctly.
(define list-ref
    (lambda (ls n)
      (letrec
        ([nth-cdr
           (lambda (n) 
             (cond
               [(zero? n) ls]
               [else(cdr(nth-cdr (sub1 n)))]))])
        (car (nth-cdr n)))))
		
;2. Define and test a procedure lambda->lumbda that takes a lambda-calculus
;expression and returns the expression unchanged with the exception that each 
;occurrence of lambda has been replaced with the word lumbda.
(define lambda->lumbda
	(lambda (exp)
		(pmatch exp
			[`,x (guard (symbol? x)) x]
			[`(lambda (,x) ,body) (list 'lumbda (list x)(lambda->lumbda body)) ]
			[`(,rator ,rand)      (list (lambda->lumbda rator)
								        (lambda->lumbda rand))])))
	
;3. Define and test a procedure vars that takes a lambda-calculus expression and
;returns a list containing all variables that occur in the expression. The order 
;of the variables does not matter.
(define vars
	(lambda (exp)
		(pmatch exp
			[`,x (guard(symbol? x)) (list x)]
			[`(lambda (,x) ,body)   (vars body)]
			[`(,rator ,rand)        (append (vars rator)(vars rand))])))

;4. Define and test a procedure union that takes two lists with no duplicates, 
;and returns a list containing the union of the two input lists. The order of the 
;elements in your answer does not matter.						
(define union
	(lambda (ls1 ls2)
		(pmatch ls2
			[`() ls1]
			[`(,a . ,b) (if (memv a ls1) 
							(union ls1 b)
							(cons a (union ls1 b)))])))
	
;5. Define and test a modification of vars called unique-vars that behaves like 
;vars but does not return duplicates. 
(define unique-vars
	(lambda (exp)
		(pmatch exp
			[`,x (guard(symbol? x)) (list x)]
			[`(lambda (,x) ,body)   (unique-vars body)]
			[`(,rator ,rand)        (union (unique-vars rator)(unique-vars rand))])))
	
;6. Define and test a procedure extend that takes two arguments, say x and pred. 
;The second argument pred is a predicate. (Recall what predicates are and how to 
;use them from the previous assignment.) What extend returns should be another 
;predicate. The returned predicate should be satisfied exactly by those things 
;that are eqv? to x or satisfy pred.
(define extend 
	(lambda (exp pred)
		(pmatch exp
			[`,x (lambda (val) (or (eqv? x val)(pred val)))])))
	
;7. Define and test a procedure free? that takes a symbol and a 
;lambda-calculus expression and returns #t if that variable occurs free in 
;that expression, and #f otherwise. The solution developed in class used 
;an accumulator, your solution should not.
(define free?
	(lambda (symbol exp)
		(pmatch exp
			[`,x (guard(symbol? x)) (eqv? x symbol)]
			[`(lambda (,x) ,body)   (and (not(eqv? x symbol))(free? symbol body))]
			[`(,rator ,rand)        (or (free? symbol rator)(free? symbol rand))])))

;8. Define and test a procedure bound? that takes a symbol and a 
;lambda-calculus expression and returns #t if that variable occurs bound in 
;the expression, and #f otherwise. Do not use helper procedures except for 
;free? as defined above. The solution developed in class used an 
;accumulator, your solution should not.
(define bound?
	(lambda (symbol exp)
		(pmatch exp
			[`,x (guard(symbol? x)) #f]
			[`(lambda (,x) ,body)   (or (and (eqv? x symbol)(free? symbol body))
										(bound? symbol body))]
			[`(,rator ,rand)        (or (bound? symbol rator)(bound? symbol rand))])))

;9. Define and test a procedure free that takes a lambda-calculus expression and 
;returns a list of all the variables that occur free in that expression. Order 
;doesn't matter, but the list must not contain duplicate variables. You should 
;not use free? or bound? as helpers.
(define remove-all
	(lambda (x ls)
		(cond
			[(null? ls) ls]
			[(eqv? x (car ls))   (remove-all x (cdr ls))]
			[else (cons (car ls) (remove-all x (cdr ls)))])))

(define free
	(lambda (exp)
		(pmatch exp
			[`,x (guard(symbol? x)) (list x)]
			[`(lambda (,x) ,body)   (remove-all x (free body))]
			[`(,rator ,rand)        (union (free rator)(free rand))])))

;10. Define and test a procedure bound that takes a lambda-calculus expression 
;and returns a list of all the variables that occur bound in the input 
;expression. Order doesn't matter, but the list must not contain duplicate 
;variables.
(define deep-member
	(lambda (x ls)
		(cond 
			[(null? ls) #f]
			[(pair? (car ls)) (or (deep-member x (car ls)) (deep-member x (cdr ls)))]
			[(eqv? x (car ls)) #t]
			[else (deep-member x (cdr ls))])))

(define bound
	(lambda (exp)
		(pmatch exp
			[`,x (guard(symbol? x)) '()]
			[`(lambda (,x) ,body)   (if (deep-member x body) 
										(union (list x) (bound body)))]
			[`(,rator ,rand)        (union (bound rator)(bound rand))])))

;11. Define and test a procedure walk-symbol that takes a symbol x and an 
;association list s. Your procedure should search through s for the value 
;associated with x. If the associated value is a symbol, it too must be walked in 
;s. If x has no association, then walk-symbol should return x.			
(define walk-symbol
	(lambda (symbol s)
		(pmatch s
			[`() symbol]
			[`(,a . ,b) (walk-symbol-rec symbol (cons a b)(cons a b))])))
					
(define walk-symbol-rec
	(lambda (symbol s entire-s)
		(cond 
			[(null? s) symbol]
			[(eqv? (caar s) symbol) (walk-symbol-rec (cdar s) entire-s entire-s)]
			[else (walk-symbol-rec symbol (cdr s) entire-s)])))

;12. Define and test a procedure lex that takes a lambda-calculus expression and an 
;accumulator (which starts as the empty list), and returns the same expression 
;with 1) all bound variable references replaced by lists of two elements whose 
;car is the symbol var and whose cadr is the lexical address of the referenced 
;variable, 2) free variables similarly wrapped within a list whose car is the 
;symbol free-var and whose cadr is the free variable, and 3) the (now 
;superfluous) formal parameters of the lambda expressions are dropped.
(define get-position
	(lambda (x ls)
		(cond
			[(null? ls) #f]
			[(eqv? x (car ls)) 0]
			[else (add1 (get-position x (cdr ls)))])))

(define lex
	(lambda (exp acc)
		(pmatch exp
			[`,x (guard(symbol? x)) (if (memv x acc) (list 'var (get-position x acc))(list 'free-var x))]
			[`(lambda (,x) ,body)   (list 'lambda (lex body (cons x acc)))]
			[`(,rator ,rand)        (list (lex rator acc)(lex rand acc))])))
	
