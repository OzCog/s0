# Ghost in the Guile Shell - Cognitive Grammar System

This document describes the cognitive grammar enhancements to the OzCog/s0 bootstrap system, implementing foundational primitives for the larger AGI-OS vision.

## Overview

The enhanced s0 system now includes cognitive grammar primitives that provide the foundation for:
- Hypergraph-based knowledge representation (AtomSpace-like)
- Agentic cognitive behaviors
- Tensor shape metadata for AI operations
- Distributed namespace management (Plan9-style)
- Integration pathways for ECAN, PLN, and MOSES

## Core Primitives

### Hypergraph Nodes
Hypergraph nodes represent cognitive concepts with type, value, and metadata:

```lisp
;; Create a cognitive concept node
(make-node 'concept 'cat)
;; => (node concept cat nil)

;; Check if something is a node
(node? (make-node 'concept 'animal))
;; => #t

;; Extract node components
(node-type (make-node 'concept 'animal))  ;; => concept
(node-value (make-node 'concept 'animal)) ;; => animal
```

### Hypergraph Edges
Hypergraph edges represent relationships between nodes:

```lisp
;; Create an edge between two nodes
(make-edge (make-node 'concept 'cat) 
           (make-node 'concept 'dog) 
           'similarity)
;; => (edge (node concept cat nil) (node concept dog nil) similarity 1)

;; Check if something is an edge
(edge? (make-edge 1 2 'relation))
;; => #t
```

### Cognitive Agents
Agents represent autonomous cognitive entities:

```lisp
;; Create a cognitive agent
(make-agent 'cognitive-001 'active)
;; => (agent cognitive-001 active nil nil)

;; Check if something is an agent
(agent? (make-agent 'agent-007 'learning))
;; => #t
```

### Tensor Shapes
Tensor shapes provide metadata for AI operations:

```lisp
;; Create tensor shape metadata
(make-tensor-shape (list 3 4 5) 'float32)
;; => (tensor-shape (3 4 5) float32 1)
```

### Hypergraph Containers
Hypergraphs aggregate nodes and edges into cognitive structures:

```lisp
;; Create a simple hypergraph
(hypergraph (list 1 2 3) (list 4 5 6))
;; => (hypergraph (1 2 3) (4 5 6))

;; Complex cognitive structure
(hypergraph 
  (list (make-node 'concept 'human) 
        (make-node 'concept 'ai))
  (list (make-edge 
          (make-node 'concept 'human) 
          (make-node 'concept 'ai) 
          'collaboration)))
```

## Building and Running

### Build the Enhanced Lisp

```bash
cd stage2/High_level_prototypes/lisp
make clean && make
```

### Run the Demo

```bash
./demo-cognitive-grammar.sh
```

### Interactive Use

```bash
cd stage2/High_level_prototypes/lisp
./lisp

# Then try cognitive primitives:
(make-node 'concept 'intelligence)
(make-agent 'thinker 'pondering)
(hypergraph (list 1 2) (list 3 4))
```

## Architecture and Future Extensions

### Current Implementation
- C-level primitives integrated into the s0 Lisp interpreter
- Symbol-based representation for compatibility with bootstrap chain
- Minimal memory footprint suitable for embedded systems

### Extension Pathways

#### AtomSpace Integration
The current hypergraph primitives provide the foundation for full OpenCog AtomSpace integration:
- Node and edge primitives map directly to Atoms and Links
- Tensor shapes support typed, multi-dimensional atom spaces
- Pattern matching can be extended for PLN reasoning

#### Plan9 Namespaces
The agent and hypergraph structures support Plan9-style namespace mounting:
- Each agent can have its own cognitive namespace
- Hypergraphs can be mounted as cognitive filesystems
- Distributed cognition through namespace sharing

#### ECAN (Economic Attention Networks)
Attention allocation can be implemented using the existing structures:
- Nodes carry attention values in their metadata
- Edges propagate attention based on weights
- Agents compete for computational resources

#### PLN (Probabilistic Logic Networks)
The hypergraph foundation supports probabilistic reasoning:
- Edges carry probability weights
- Nodes represent logical propositions
- Inference can operate on hypergraph structures

#### MOSES (Meta-Optimizing Semantic Evolutionary Search)
Evolutionary search can operate on cognitive structures:
- Agents represent candidate solutions
- Hypergraphs encode program structures
- Tensor shapes define search spaces

## Implementation Notes

### Memory Management
The system uses the existing s0 garbage collector for memory management. All cognitive structures are represented as standard Lisp cells.

### Type System
The primitive type system uses symbol markers:
- `node` for hypergraph nodes
- `edge` for hypergraph edges
- `agent` for cognitive agents
- `tensor-shape` for AI metadata
- `hypergraph` for aggregate structures

### Symbol Quoting
Remember to quote symbols when using them as data:
```lisp
;; Correct
(make-node 'concept 'cat)

;; Incorrect (treats concept and cat as variables)
(make-node concept cat)
```

## Contributing

To add new cognitive primitives:

1. Add function declaration to `lisp_eval.c`
2. Implement the function following the existing patterns
3. Register the function in `init_sl3()` using `spinup()`
4. Test with the enhanced Lisp interpreter

## License

This enhancement maintains the original s0 GPL-3+ license. All cognitive grammar primitives are available under the same terms.

---

*"Ghost in the Guile Shell" - Bootstrapping AGI from the metal up.*