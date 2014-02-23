#lang racket

(define (transform-if-to-cond code)
  ; locates every occurrence of an if expression in code and 
  ; transforms it into an equivalent cond

  ;  (if c p q)  => (cond (c p) (else q ))
  
  (define (trans c p q)
      (list 'cond (list c p) (list 'else q) ) )
  
  (if (and (list? code) (not (null? code)) (eq? 'if (first code)))

      ; recursively transform the parts of the if before transforming the if
      (apply trans (map transform-if-to-cond (rest code)))
      
      (if (list? code) 
            ; recursively transform expressions
            (map transform-if-to-cond code) 
            ; leaf expression, leave alone
            code)
      ))



(define (transform-let-to-apply code)
  ; locates every occurrence of an let expression in code and 
  ; transforms it into an equivalent lambda applied to the defining expresions.

  ; (let ( (v1 e1) (v2 e2) ... (vn en) ) b1 b2 ... bk )
  ; =>
  ; ((lambda (v1 v2 ... vn) b1 b2 ... bk) e1 e2 ... en)
  (define (trans ve b)
      (foldr cons (map car (map rest ve)) (list (list 'lambda (map first ve) b))))
  
  (if (and (list? code) (not (null? code)) (eq? 'let (first code)))

      ; recursively transform the parts of the let before transforming the let
      (apply trans (map transform-let-to-apply (rest code)))
      
      (if (list? code) 
            ; recursively transform expressions
            (map transform-let-to-apply code) 
            ; leaf expression, leave alone
            code)
      )
  )
