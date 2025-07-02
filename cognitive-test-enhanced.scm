;; Test the enhanced Lisp with cognitive grammar primitives

;; Test hypergraph nodes
(make-node "concept" "cat")
(make-node "concept" "dog" "metadata")

;; Test node predicates and accessors
(define test-node (make-node "concept" "animal"))
test-node
(node? test-node)
(node-type test-node)
(node-value test-node)

;; Test hypergraph edges
(define node1 (make-node "concept" "cat"))
(define node2 (make-node "concept" "dog"))
(define test-edge (make-edge node1 node2 "similarity"))
test-edge
(edge? test-edge)

;; Test agents
(define cognitive-agent (make-agent "agent-001" "active"))
cognitive-agent
(agent? cognitive-agent)

;; Test tensor shapes
(define tensor (make-tensor-shape (list 3 4 5) "float32"))
tensor

;; Test hypergraph
(define simple-hg (hypergraph (list node1 node2) (list test-edge)))
simple-hg