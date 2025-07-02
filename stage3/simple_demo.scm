;; Simple standalone demo of core cognitive functionality
;; Copyright (C) 2024 Copilot AI

;; Load ascension library
;; 2 level car/cdr
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

; 3 level car/cdr
(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

; 4 level car/cdr
(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

; reverse a list
(define reverse
	(lambda (l)
		(define (reving l result)
			(cond
				((null? l) result)
				((list? list) (reving (cdr l) (cons (car l) result)))
				(#t (cons l result))))
		(reving l nil)))

; Map
(define map
	(lambda (f l)
		(if (null? l)
			nil
			(cons (f (car l)) (map f (cdr l))))))

; Filter
(define filter
	(lambda (p l)
		(if (null? l)
			nil
			(if (p (car l))
				(cons (car l) (filter p (cdr l)))
				(filter p (cdr l))))))

; Folds
(define fold-right
	(lambda (f a l)
		(if (null? l)
			a
			(f (car l) (fold-right f a (cdr l))))))
(define fold-left
	(lambda (f a xs)
		(if (null? xs)
			a
			(fold-left f (f a (car xs)) (cdr xs)))))

; char functions
(define numeric-char? (lambda (ch) (if (and (char? ch) (<= 48 ch 57)) #t nil)))
(define digit->number (lambda (d) (if (and (char? d) (<= 48 d 57)) (- d 48) nil)))

; length functions
(define length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l))))))
(define string-length (lambda (s) (length (string->list s))))

; More generic comparision
(define eq?
	(lambda (a b)
		(cond
			((string? a) (if (string? b) (string=? a b) nil))
			((char? a) (if (char? b) (= a b) nil))
			(#t (= a b)))))

; Additional utility functions
(define member
	(lambda (item lst)
		(cond
			((null? lst) #f)
			((equal? item (car lst)) #t)
			(else (member item (cdr lst))))))

(define assoc
	(lambda (key alist)
		(cond
			((null? alist) #f)
			((equal? key (car (car alist))) (car alist))
			(else (assoc key (cdr alist))))))

(define equal?
	(lambda (a b)
		(cond
			((and (null? a) (null? b)) #t)
			((or (null? a) (null? b)) #f)
			((and (pair? a) (pair? b))
			 (and (equal? (car a) (car b))
				  (equal? (cdr a) (cdr b))))
			(else (eq? a b)))))

;; Core cognitive functions

;; Variable representation - symbols starting with ?
(define variable? 
  (lambda (x) 
    (and (symbol? x) 
         (let ((str (symbol->string x)))
           (and (> (string-length str) 0)
                (char=? (string-ref str 0) #\?))))))

;; Simple unification
(define unify
  (lambda (term1 term2 subst)
    (cond
      ((eq? subst 'fail) 'fail)
      ((eq? term1 term2) subst)
      ((variable? term1) (unify-variable term1 term2 subst))
      ((variable? term2) (unify-variable term2 term1 subst))
      ((and (pair? term1) (pair? term2))
       (unify (cdr term1) (cdr term2)
              (unify (car term1) (car term2) subst)))
      (else 'fail))))

;; Unify a variable with a term
(define unify-variable
  (lambda (var term subst)
    (let ((binding (assoc var subst)))
      (cond
        (binding (unify (cdr binding) term subst))
        (else (cons (cons var term) subst))))))

;; Apply substitution to a term
(define substitute
  (lambda (term subst)
    (cond
      ((variable? term)
       (let ((binding (assoc term subst)))
         (if binding
             (substitute (cdr binding) subst)
             term)))
      ((pair? term)
       (cons (substitute (car term) subst)
             (substitute (cdr term) subst)))
      (else term))))

;; Pattern matching with backtracking
(define match-pattern
  (lambda (pattern data)
    (unify pattern data nil)))

;; Basic AtomSpace
(define make-truth-value
  (lambda (strength confidence)
    (list 'truth-value strength confidence)))

(define tv-strength cadr)
(define tv-confidence caddr)

(define true-tv (make-truth-value 1.0 1.0))
(define false-tv (make-truth-value 0.0 1.0))

(define make-atom
  (lambda (type name . args)
    (let ((outgoing (if (null? args) nil (car args)))
          (tv (if (and (not (null? args)) (not (null? (cdr args))))
                  (cadr args)
                  (make-truth-value 0.5 0.0))))
      (list 'atom type name outgoing tv))))

(define atom-type cadr)
(define atom-name caddr)
(define atom-outgoing cadddr)

(define make-atomspace
  (lambda ()
    (list 'atomspace (list) 0))) ; atoms, next-id

(define atomspace-atoms cadr)
(define atomspace-next-id caddr)

(define *atomspace* (make-atomspace))

(define next-atom-id
  (lambda (atomspace)
    (let ((id (atomspace-next-id atomspace)))
      (set-car! (cddr atomspace) (+ id 1))
      id)))

(define add-atom
  (lambda (atomspace atom)
    (let ((id (next-atom-id atomspace)))
      (let ((atom-with-id (append atom (list id))))
        (set-car! (cdr atomspace) 
                  (cons atom-with-id (atomspace-atoms atomspace)))
        atom-with-id))))

(define concept
  (lambda (name . tv)
    (let ((truth-val (if (null? tv) (make-truth-value 0.5 0.0) (car tv))))
      (add-atom *atomspace* 
                (make-atom 'ConceptNode name nil truth-val)))))

(define inheritance
  (lambda (child parent . tv)
    (let ((truth-val (if (null? tv) (make-truth-value 0.5 0.0) (car tv))))
      (add-atom *atomspace*
                (make-atom 'InheritanceLink nil 
                          (list child parent) truth-val)))))

(define atomspace-size
  (lambda (atomspace)
    (length (atomspace-atoms atomspace))))

;; Demonstrations
(display "Stage0 Cognitive Architecture Demo")
(newline)
(display "====================================")
(newline)
(newline)

;; Test pattern matching
(display "1. Pattern Matching & Unification:")
(newline)
(let ((result (unify '(?x loves ?y) '(john loves mary) nil)))
  (display "   Unifying (?x loves ?y) with (john loves mary)")
  (newline)
  (display "   Result: ")
  (display result)
  (newline))

(display "   Is ?x a variable? ")
(display (variable? '?x))
(newline)

(display "   Is foo a variable? ")
(display (variable? 'foo))
(newline)
(newline)

;; Test AtomSpace
(display "2. AtomSpace Operations:")
(newline)
(let ((cat (concept "cat" true-tv))
      (animal (concept "animal" true-tv)))
  (display "   Created concepts: cat and animal")
  (newline)
  
  (let ((inh (inheritance cat animal true-tv)))
    (display "   Created inheritance: cat is-a animal")
    (newline)
    
    (display "   AtomSpace size: ")
    (display (atomspace-size *atomspace*))
    (newline)))

(newline)

;; Test substitution
(display "3. Substitution:")
(newline)
(let ((subst '((?x . john) (?y . mary))))
  (display "   Substituting ?x with john in (?x loves ?y)")
  (newline)
  (display "   Result: ")
  (display (substitute '(?x loves ?y) subst))
  (newline))

(newline)
(display "Demo completed successfully!")
(newline)