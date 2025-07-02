;; Test script for Ghost in the Guile Shell cognitive features
;; This demonstrates the hypergraph, agent, and attention systems

;; Load the cognitive grammar library 
;; (Note: In the current lisp, we'll inline some basic tests)

;; Test 1: Create hypergraph nodes and edges
(define test-node1 (list 'node "concept" "cat" nil (list 'tensor-shape '(3 4) "animal" 2)))
(define test-node2 (list 'node "concept" "dog" nil (list 'tensor-shape '(3 4) "animal" 2)))
(define test-edge (list 'edge test-node1 test-node2 "similar" 0.8))

(display "Test 1 - Hypergraph primitives:")
(newline)
(display "Node 1: ")
(display test-node1)
(newline)
(display "Node 2: ")
(display test-node2)
(newline)
(display "Edge: ")
(display test-edge)
(newline)
(newline)

;; Test 2: Create an agent
(define test-agent (list 'agent "cognitive-001" "active" '("learn" "reason") 
                         (list test-node1 test-node2) '("observe" "reason")))

(display "Test 2 - Agent system:")
(newline)
(display "Agent: ")
(display test-agent)
(newline)
(newline)

;; Test 3: Create a namespace and ASFS
(define test-namespace (list 'namespace "cognitive-space" nil nil))
(define test-asfs (list 'asfs test-namespace (list test-node1 test-node2)))

(display "Test 3 - Namespace and ASFS:")
(newline)
(display "Namespace: ")
(display test-namespace)
(newline)
(display "ASFS: ")
(display test-asfs)
(newline)
(newline)

;; Test 4: Attention values
(define test-attention (list 'attention-value 100 50 10))
(display "Test 4 - Attention system:")
(newline)
(display "Attention value: ")
(display test-attention)
(newline)
(newline)

;; Test 5: Cognitive bootstrap initialization
(define cognitive-system (list 'cognitive-system test-asfs (list test-agent) (list test-attention)))

(display "Test 5 - Cognitive bootstrap:")
(newline)
(display "Cognitive System: ")
(display cognitive-system)
(newline)
(newline)

(display "Ghost in the Guile Shell - Basic cognitive grammar test completed!")
(newline)