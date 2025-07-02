#!/bin/bash
# Ghost in the Guile Shell - Cognitive Grammar Demo
# This script demonstrates the enhanced s0 Lisp with cognitive primitives

cd stage2/High_level_prototypes/lisp

echo "====================================================="
echo "Ghost in the Guile Shell - Cognitive Grammar Demo"
echo "Enhanced OzCog/s0 with AGI-OS Foundation Primitives"
echo "====================================================="
echo

echo "1. Creating Hypergraph Nodes (Cognitive Concepts):"
echo "(make-node 'concept 'cat)"
echo "(make-node 'concept 'cat)" | ./lisp
echo

echo "(make-node 'entity 'dog)"
echo "(make-node 'entity 'dog)" | ./lisp  
echo

echo "2. Testing Node Predicates:"
echo "(node? (make-node 'concept 'animal))"
echo "(node? (make-node 'concept 'animal))" | ./lisp
echo

echo "(node-type (make-node 'concept 'animal))"
echo "(node-type (make-node 'concept 'animal))" | ./lisp
echo

echo "(node-value (make-node 'concept 'animal))"  
echo "(node-value (make-node 'concept 'animal))" | ./lisp
echo

echo "3. Creating Hypergraph Edges (Relationships):"
echo "(make-edge (make-node 'concept 'cat) (make-node 'concept 'dog) 'similarity)"
echo "(make-edge (make-node 'concept 'cat) (make-node 'concept 'dog) 'similarity)" | ./lisp
echo

echo "4. Testing Edge Predicates:"
echo "(edge? (make-edge 1 2 'relation))"
echo "(edge? (make-edge 1 2 'relation))" | ./lisp
echo

echo "5. Creating Cognitive Agents:"
echo "(make-agent 'cognitive-001 'active)"
echo "(make-agent 'cognitive-001 'active)" | ./lisp
echo

echo "(agent? (make-agent 'agent-007 'learning))"
echo "(agent? (make-agent 'agent-007 'learning))" | ./lisp
echo

echo "6. Creating Tensor Shapes (Metadata for AI):"
echo "(make-tensor-shape (list 3 4 5) 'float32)"
echo "(make-tensor-shape (list 3 4 5) 'float32)" | ./lisp
echo

echo "7. Creating Hypergraph Structures:"
echo "(hypergraph (list 1 2 3) (list 4 5 6))"
echo "(hypergraph (list 1 2 3) (list 4 5 6))" | ./lisp
echo

echo "8. Complex Cognitive Structure Example:"
cat << 'EOF' | ./lisp
(hypergraph 
  (list (make-node 'concept 'human) 
        (make-node 'concept 'ai))
  (list (make-edge 
          (make-node 'concept 'human) 
          (make-node 'concept 'ai) 
          'collaboration)))
EOF
echo

echo "====================================================="
echo "Demo Complete - Cognitive Grammar Primitives Working!"
echo
echo "The enhanced s0 Lisp now supports:"
echo "- Hypergraph nodes and edges for knowledge representation"
echo "- Cognitive agents for agentic behaviors"
echo "- Tensor shapes for AI metadata"
echo "- Hypergraph containers for complex structures"
echo
echo "This provides the foundation for:"
echo "- AtomSpace-like hypergraph operations"
echo "- Cognitive grammar pattern matching"
echo "- Agentic kernel spawning"
echo "- Plan9-style namespace mounting"
echo "- ECAN/PLN/MOSES integration pathways"
echo "====================================================="