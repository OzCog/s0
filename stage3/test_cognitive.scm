;; Test suite for cognitive patterns and AtomSpace functionality
;; Copyright (C) 2024 Copilot AI

;; Load the cognitive architecture
(load "stage3/ascension.scm")
(load "stage3/cognitive_patterns.scm")
(load "stage3/plan9_namespace.scm")
(load "stage3/atomspace_bindings.scm")

;; Test framework
(define test-passed 0)
(define test-failed 0)

(define assert
  (lambda (condition message)
    (if condition
        (begin
          (set! test-passed (+ test-passed 1))
          (display "PASS: ")
          (display message)
          (newline))
        (begin
          (set! test-failed (+ test-failed 1))
          (display "FAIL: ")
          (display message)
          (newline)))))

(define test-summary
  (lambda ()
    (display "Test Results: ")
    (display test-passed)
    (display " passed, ")
    (display test-failed)
    (display " failed")
    (newline)))

;; Test unification
(define test-unification
  (lambda ()
    (display "Testing unification...")
    (newline)
    
    ;; Test simple unification
    (let ((result (unify '?x 'john nil)))
      (assert (and (not (eq? result 'fail))
                   (equal? (assoc '?x result) '(?x . john)))
              "Simple variable unification"))
    
    ;; Test complex unification
    (let ((result (unify '(?x loves ?y) '(john loves mary) nil)))
      (assert (and (not (eq? result 'fail))
                   (equal? (assoc '?x result) '(?x . john))
                   (equal? (assoc '?y result) '(?y . mary)))
              "Complex pattern unification"))
    
    ;; Test unification failure
    (let ((result (unify '(a b) '(c d) nil)))
      (assert (eq? result 'fail)
              "Unification failure detection"))))

;; Test AtomSpace operations
(define test-atomspace
  (lambda ()
    (display "Testing AtomSpace operations...")
    (newline)
    
    ;; Test concept creation
    (let ((cat (concept "cat")))
      (assert (atom? cat) "Concept creation"))
    
    ;; Test inheritance link
    (let ((cat (concept "cat"))
          (animal (concept "animal")))
      (let ((inh (inheritance cat animal)))
        (assert (and (atom? inh)
                     (eq? (atom-type inh) inheritance-link))
                "Inheritance link creation")))
    
    ;; Test atomspace size
    (let ((initial-size (atomspace-size *atomspace*)))
      (concept "test-concept")
      (let ((new-size (atomspace-size *atomspace*)))
        (assert (> new-size initial-size)
                "AtomSpace size increase")))
    
    ;; Test atom lookup
    (let ((test-atom (concept "lookup-test")))
      (let ((found (find-atom *atomspace* concept-node "lookup-test")))
        (assert (not (null? found))
                "Atom lookup by type and name")))))

;; Test namespace operations
(define test-namespaces
  (lambda ()
    (display "Testing namespace operations...")
    (newline)
    
    ;; Test binding
    (ns-bind "/test/service" "test-service")
    (assert (equal? (ns-resolve "/test/service") "test-service")
            "Namespace binding and resolution")
    
    ;; Test mounting
    (ns-mount "/source" "/target" 'read-write)
    (assert (not (null? mount-table))
            "Namespace mounting")
    
    ;; Test path splitting
    (let ((components (split-path "/a/b/c")))
      (assert (equal? components '("a" "b" "c"))
              "Path splitting"))
    
    ;; Test path joining
    (let ((path (join-path '("a" "b" "c"))))
      (assert (equal? path "a/b/c")
              "Path joining"))))

;; Test inference
(define test-inference
  (lambda ()
    (display "Testing inference...")
    (newline)
    
    ;; Test forward chaining
    (let ((facts '((bird tweety) (bird polly)))
          (rules '(((bird ?x) (flies ?x)))))
      (let ((result (forward-chain facts rules)))
        (assert (member '(flies tweety) result)
                "Forward chaining inference")))
    
    ;; Test modus ponens
    (let ((premise1 '(implies (rains) (wet ground)))
          (premise2 '(rains)))
      (let ((result (modus-ponens premise1 premise2)))
        (assert (equal? result '(wet ground))
                "Modus ponens inference"))))

;; Test frame operations
(define test-frames
  (lambda ()
    (display "Testing frame operations...")
    (newline)
    
    ;; Test frame creation
    (let ((frame (make-frame "person" '((name . "john") (age . 30)))))
      (assert (equal? (frame-name frame) "person")
              "Frame creation"))
    
    ;; Test slot access
    (let ((frame (make-frame "person" '((name . "john") (age . 30)))))
      (assert (equal? (get-slot frame 'name) "john")
              "Frame slot access"))
    
    ;; Test slot modification
    (let ((frame (make-frame "person" '((name . "john") (age . 30)))))
      (let ((new-frame (set-slot frame 'age 31)))
        (assert (equal? (get-slot new-frame 'age) 31)
                "Frame slot modification")))))

;; Test truth values
(define test-truth-values
  (lambda ()
    (display "Testing truth values...")
    (newline)
    
    ;; Test truth value creation
    (let ((tv (make-truth-value 0.8 0.9)))
      (assert (and (= (tv-strength tv) 0.8)
                   (= (tv-confidence tv) 0.9))
              "Truth value creation"))
    
    ;; Test truth value merging
    (let ((tv1 (make-truth-value 0.8 0.5))
          (tv2 (make-truth-value 0.6 0.7)))
      (let ((merged (tv-merge tv1 tv2)))
        (assert (> (tv-confidence merged) 0.5)
                "Truth value merging")))))

;; Test goal-directed reasoning
(define test-planning
  (lambda ()
    (display "Testing planning...")
    (newline)
    
    ;; Test operator creation
    (let ((op (make-operator "move" '(at-robot ?loc1) '(at-robot ?loc2))))
      (assert (equal? (operator-name op) "move")
              "Operator creation"))
    
    ;; Test operator applicability
    (let ((op (make-operator "move" '(at-robot home) '(at-robot work)))
          (state '(at-robot home)))
      (assert (applicable? op state)
              "Operator applicability check"))))

;; Run all tests
(define run-all-tests
  (lambda ()
    (display "Starting cognitive architecture tests...")
    (newline)
    (newline)
    
    (test-unification)
    (newline)
    
    (test-atomspace)
    (newline)
    
    (test-namespaces)
    (newline)
    
    (test-inference)
    (newline)
    
    (test-frames)
    (newline)
    
    (test-truth-values)
    (newline)
    
    (test-planning)
    (newline)
    
    (test-summary)
    (newline)))

;; Run the tests
(run-all-tests)