;; Simple demonstration of cognitive architecture functionality
;; Load basic library
(load "stage3/ascension.scm")

;; Demonstrate pattern matching and unification
(define test-unify
  (lambda ()
    (display "Testing unification...")
    (newline)
    (let ((result (unify '(?x loves ?y) '(john loves mary) nil)))
      (display "Unifying (?x loves ?y) with (john loves mary): ")
      (display result)
      (newline))))

;; Load cognitive patterns
(load "stage3/cognitive_patterns.scm")

;; Demonstrate pattern matching
(define demo-pattern-matching
  (lambda ()
    (display "=== Pattern Matching Demo ===")
    (newline)
    (test-unify)
    
    ;; Test variable detection
    (display "Is ?x a variable? ")
    (display (variable? '?x))
    (newline)
    
    ;; Test modus ponens
    (display "Modus ponens: (implies (rains) (wet)) + (rains) = ")
    (display (modus-ponens '(implies (rains) (wet)) '(rains)))
    (newline)))

;; Load AtomSpace
(load "stage3/atomspace_bindings.scm")

;; Demonstrate AtomSpace operations
(define demo-atomspace
  (lambda ()
    (display "=== AtomSpace Demo ===")
    (newline)
    
    ;; Create concepts
    (let ((cat (concept "cat"))
          (animal (concept "animal")))
      (display "Created concepts: cat and animal")
      (newline)
      
      ;; Create inheritance
      (let ((inh (inheritance cat animal true-tv)))
        (display "Created inheritance: cat is-a animal")
        (newline)
        
        ;; Check atomspace size
        (display "AtomSpace size: ")
        (display (atomspace-size *atomspace*))
        (newline))))

;; Load Plan9 namespaces
(load "stage3/plan9_namespace.scm")

;; Demonstrate namespace operations
(define demo-namespaces
  (lambda ()
    (display "=== Namespace Demo ===")
    (newline)
    
    ;; Bind a service
    (ns-bind "/cognitive/test" "test-service")
    (display "Bound service to /cognitive/test")
    (newline)
    
    ;; Resolve the binding
    (display "Resolved /cognitive/test: ")
    (display (ns-resolve "/cognitive/test"))
    (newline)
    
    ;; Test path operations
    (display "Split path /a/b/c: ")
    (display (split-path "/a/b/c"))
    (newline)))

;; Run all demonstrations
(display "Stage0 Cognitive Architecture Demonstration")
(newline)
(newline)

(demo-pattern-matching)
(newline)

(demo-atomspace)
(newline)

(demo-namespaces)
(newline)

(display "All demonstrations completed successfully!")
(newline)