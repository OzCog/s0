# Integration Pathways for ECAN/PLN/MOSES

This document outlines integration pathways for connecting the stage0 cognitive architecture components with OpenCog's ECAN (Economic Attention Networks), PLN (Probabilistic Logic Networks), and MOSES (Meta-Optimizing Semantic Evolutionary Search) systems.

## Overview

The stage0 project provides a minimal bootstrap environment that can be extended to support advanced cognitive architectures. The cognitive patterns library, Plan9-style namespaces, and AtomSpace bindings provide the foundation for integrating with OpenCog systems.

## Architecture Integration Points

### 1. AtomSpace Bridge

The `atomspace_bindings.scm` provides a basic AtomSpace implementation that can serve as a bridge to OpenCog's AtomSpace:

```scheme
;; Example: Creating basic knowledge representation
(define cat-concept (concept "cat" true-tv))
(define animal-concept (concept "animal" true-tv))
(define cat-is-animal (inheritance cat-concept animal-concept true-tv))
```

**Integration Path:**
- Use the stage0 AtomSpace as a lightweight front-end
- Implement serialization/deserialization to OpenCog's native format
- Create proxy objects that delegate operations to full OpenCog AtomSpace

### 2. ECAN Integration

The Economic Attention Networks system can be integrated through the attention allocation mechanisms:

```scheme
;; Attention value management
(define high-importance (make-attention-value 100 50 10))
(set-attention important-atom high-importance)

;; Attentional focus
(define focused-atoms (attentional-focus *atomspace* 50))
```

**Integration Pathway:**
1. **Attention Spreading**: Implement attention spread algorithms using the cognitive patterns library
2. **Economic Dynamics**: Use the namespace system to model economic relationships between atoms
3. **Rent Collection**: Implement rent-based attention decay using truth value operations

**Implementation Steps:**
```scheme
;; ECAN attention spreading
(define spread-attention
  (lambda (atomspace source-atom spread-factor)
    (let ((incoming (get-incoming atomspace source-atom)))
      (map (lambda (atom)
             (let ((current-av (get-attention atom)))
               (set-attention atom 
                 (increase-sti current-av 
                   (* spread-factor (av-sti current-av))))))
           incoming))))
```

### 3. PLN Integration

Probabilistic Logic Networks can be implemented using the cognitive patterns and unification system:

```scheme
;; PLN rule example using cognitive patterns
(define modus-ponens-rule
  (lambda (implication antecedent)
    (let ((pattern '(implication ?a ?b))
          (subst (match-pattern pattern implication)))
      (if (and (not (eq? subst 'fail))
               (equal? antecedent (substitute '?a subst)))
          (substitute '?b subst)
          nil))))
```

**Integration Pathway:**
1. **Rule Engine**: Extend the forward-chaining inference with PLN-specific rules
2. **Truth Value Revision**: Implement PLN's truth value formulas using the truth value operations
3. **Uncertainty Handling**: Use the truth value confidence for uncertainty propagation

**Key PLN Rules Implementation:**
```scheme
;; Deduction rule: (A->B) ∧ (B->C) ⊢ (A->C)
(define deduction-rule
  (lambda (impl1 impl2)
    (let ((tv1 (atom-tv impl1))
          (tv2 (atom-tv impl2)))
      (make-truth-value
        (* (tv-strength tv1) (tv-strength tv2))
        (* (tv-confidence tv1) (tv-confidence tv2))))))

;; Inversion rule: (A->B) ⊢ (B->A)
(define inversion-rule
  (lambda (impl)
    (let ((tv (atom-tv impl))
          (outgoing (atom-outgoing impl)))
      (make-atom implication-link nil
        (reverse outgoing)
        (make-truth-value
          (/ (tv-strength tv) (+ (tv-strength tv) 1))
          (tv-confidence tv))))))
```

### 4. MOSES Integration

Meta-Optimizing Semantic Evolutionary Search can be integrated through the planning and goal-directed reasoning components:

```scheme
;; MOSES program representation
(define make-program
  (lambda (genome fitness complexity)
    (list 'program genome fitness complexity)))

;; Population management
(define evolve-population
  (lambda (population fitness-fn)
    (let ((scored (map (lambda (prog)
                         (cons prog (fitness-fn prog)))
                       population)))
      (selection scored))))
```

**Integration Pathway:**
1. **Program Representation**: Use S-expressions as program genomes
2. **Fitness Evaluation**: Integrate with AtomSpace for semantic fitness evaluation
3. **Evolutionary Operators**: Implement crossover and mutation using pattern matching

**MOSES Components:**
```scheme
;; Fitness evaluation using AtomSpace
(define evaluate-program
  (lambda (program atomspace)
    (let ((result (execute-program program atomspace)))
      (+ (semantic-fitness result atomspace)
         (- (program-complexity program))))))

;; Crossover operation
(define crossover
  (lambda (parent1 parent2)
    (let ((point1 (random-crossover-point parent1))
          (point2 (random-crossover-point parent2)))
      (list (splice-programs parent1 parent2 point1 point2)
            (splice-programs parent2 parent1 point2 point1)))))
```

## Namespace Integration for Modularity

The Plan9-style namespace system provides a clean way to modularize cognitive components:

```scheme
;; Mount cognitive components
(ns-mount "/sys/ecan" "/cognitive/attention" '(read write))
(ns-mount "/sys/pln" "/cognitive/reasoning" '(read write))
(ns-mount "/sys/moses" "/cognitive/learning" '(read write))

;; Access through namespace
(define attention-service (ns-resolve "/sys/ecan/spread"))
(define reasoning-service (ns-resolve "/sys/pln/infer"))
(define learning-service (ns-resolve "/sys/moses/evolve"))
```

## Communication Protocols

### 1. Message Passing Interface

```scheme
;; Message structure for inter-component communication
(define make-message
  (lambda (from to type payload)
    (list 'message from to type payload (current-time))))

;; ECAN -> PLN: Attention updates
(define send-attention-update
  (lambda (atoms attention-values)
    (send-message "ecan" "pln" 'attention-update
                  (list 'atoms atoms 'values attention-values))))

;; PLN -> MOSES: Learning targets
(define send-learning-target
  (lambda (rules fitness-threshold)
    (send-message "pln" "moses" 'learning-target
                  (list 'rules rules 'threshold fitness-threshold))))
```

### 2. Event System

```scheme
;; Event-driven architecture for cognitive components
(define event-bus (list))

(define subscribe
  (lambda (component event-type handler)
    (set! event-bus 
          (cons (list component event-type handler) event-bus))))

(define publish-event
  (lambda (event-type data)
    (map (lambda (subscription)
           (when (eq? (cadr subscription) event-type)
             ((caddr subscription) data)))
         event-bus)))
```

## Performance Considerations

### 1. Memory Management

```scheme
;; Implement garbage collection for atoms
(define gc-atomspace
  (lambda (atomspace)
    (let ((reachable (mark-reachable-atoms atomspace))
          (all-atoms (atomspace-atoms atomspace)))
      (set-car! (cdr atomspace)
                (filter (lambda (atom) (member atom reachable))
                        all-atoms)))))
```

### 2. Distributed Processing

```scheme
;; Distribute computation across processes
(define distribute-inference
  (lambda (rules facts worker-count)
    (let ((partitions (partition facts worker-count)))
      (map (lambda (partition)
             (spawn-worker (lambda () (forward-chain partition rules))))
           partitions))))
```

## Build Integration

To integrate with the stage0 build system, add to the makefile:

```makefile
# Cognitive architecture targets
cognitive: cognitive_patterns atomspace_bindings plan9_namespace

cognitive_patterns: stage3/cognitive_patterns.scm stage3/ascension.scm
	cat stage3/ascension.scm stage3/cognitive_patterns.scm > cognitive_temp
	./bin/vm --rom roms/lisp --tape_01 cognitive_temp --memory 4M
	rm cognitive_temp

atomspace_bindings: stage3/atomspace_bindings.scm cognitive_patterns
	cat stage3/ascension.scm stage3/cognitive_patterns.scm stage3/atomspace_bindings.scm > atomspace_temp
	./bin/vm --rom roms/lisp --tape_01 atomspace_temp --memory 4M
	rm atomspace_temp
```

## Testing Framework

```scheme
;; Test utilities for cognitive components
(define run-tests
  (lambda (test-suite)
    (map (lambda (test)
           (let ((result ((cdr test))))
             (display (car test))
             (display ": ")
             (display (if result "PASS" "FAIL"))
             (newline)))
         test-suite)))

;; Example tests
(define cognitive-tests
  (list
    (cons "unification test"
          (lambda () 
            (not (eq? (unify '(?x loves mary) '(john loves ?y) nil) 'fail))))
    (cons "atomspace test"
          (lambda ()
            (let ((concept1 (concept "test")))
              (> (atomspace-size *atomspace*) 0))))))
```

## Future Extensions

1. **Streaming Integration**: Connect to external data sources through namespace mounts
2. **Parallel Processing**: Extend the VM to support multiple threads of execution
3. **Network Protocols**: Implement distributed AtomSpace across multiple machines
4. **Learning Integration**: Connect to external ML libraries through FFI

This integration pathway provides a solid foundation for building cognitive architectures on the minimal stage0 platform while maintaining compatibility with OpenCog's advanced reasoning systems.