;; Simple cognitive grammar test for the s0 Lisp

;; Test basic list operations for hypergraph structures
(define make-node (lambda (type value) (list type value)))
(define node-type (lambda (node) (car node)))
(define node-value (lambda (node) (car (cdr node))))

;; Create test nodes
(define cat-node (make-node "concept" "cat"))
(define dog-node (make-node "concept" "dog"))

;; Test the functions
cat-node
dog-node
(node-type cat-node)
(node-value cat-node)

;; Test basic agent structure
(define make-agent (lambda (id state) (list id state)))
(define test-agent (make-agent "agent-1" "active"))
test-agent

;; Test namespace concept  
(define make-namespace (lambda (name) (list name)))
(define test-ns (make-namespace "cognitive-space"))
test-ns

;; Display success message
"Cognitive grammar primitives working!"