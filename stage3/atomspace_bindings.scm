;; Copyright (C) 2024 Copilot AI
;; AtomSpace interface bindings for stage0 cognitive architecture
;;
;; stage0 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; stage0 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with stage0.  If not, see <http://www.gnu.org/licenses/>.

;; AtomSpace prototype implementation
;; Provides basic atom management, truth values, and hypergraph operations

;; Atom types
(define concept-node 'ConceptNode)
(define predicate-node 'PredicateNode)
(define link-node 'Link)
(define inheritance-link 'InheritanceLink)
(define similarity-link 'SimilarityLink)
(define evaluation-link 'EvaluationLink)
(define list-link 'ListLink)
(define and-link 'AndLink)
(define or-link 'OrLink)
(define not-link 'NotLink)
(define implication-link 'ImplicationLink)

;; Truth value structure
(define make-truth-value
  (lambda (strength confidence)
    (list 'truth-value strength confidence)))

(define tv-strength cadr)
(define tv-confidence caddr)

;; Default truth values
(define true-tv (make-truth-value 1.0 1.0))
(define false-tv (make-truth-value 0.0 1.0))
(define unknown-tv (make-truth-value 0.5 0.0))

;; Atom structure
(define make-atom
  (lambda (type name . args)
    (let ((outgoing (if (null? args) nil (car args)))
          (tv (if (and (not (null? args)) (not (null? (cdr args))))
                  (cadr args)
                  unknown-tv)))
      (list 'atom type name outgoing tv))))

(define atom-type cadr)
(define atom-name caddr)
(define atom-outgoing cadddr)
(define atom-tv cadddr)

;; AtomSpace structure
(define make-atomspace
  (lambda ()
    (list 'atomspace (list) 0))) ; atoms, next-id

(define atomspace-atoms cadr)
(define atomspace-next-id caddr)

;; Global atomspace
(define *atomspace* (make-atomspace))

;; Generate unique atom ID
(define next-atom-id
  (lambda (atomspace)
    (let ((id (atomspace-next-id atomspace)))
      (set-car! (cddr atomspace) (+ id 1))
      id)))

;; Add atom to atomspace
(define add-atom
  (lambda (atomspace atom)
    (let ((id (next-atom-id atomspace)))
      (let ((atom-with-id (append atom (list id))))
        (set-car! (cdr atomspace) 
                  (cons atom-with-id (atomspace-atoms atomspace)))
        atom-with-id))))

;; Find atom by type and name
(define find-atom
  (lambda (atomspace type name)
    (filter (lambda (atom)
              (and (eq? (atom-type atom) type)
                   (equal? (atom-name atom) name)))
            (atomspace-atoms atomspace))))

;; Get atom by ID
(define get-atom
  (lambda (atomspace id)
    (filter (lambda (atom)
              (= (car (last atom)) id))
            (atomspace-atoms atomspace))))

;; Remove atom from atomspace
(define remove-atom
  (lambda (atomspace atom-id)
    (set-car! (cdr atomspace)
              (filter (lambda (atom)
                        (not (= (car (last atom)) atom-id)))
                      (atomspace-atoms atomspace)))
    #t))

;; Create concept node
(define concept
  (lambda (name . tv)
    (let ((truth-val (if (null? tv) unknown-tv (car tv))))
      (add-atom *atomspace* 
                (make-atom concept-node name nil truth-val)))))

;; Create predicate node
(define predicate
  (lambda (name . tv)
    (let ((truth-val (if (null? tv) unknown-tv (car tv))))
      (add-atom *atomspace* 
                (make-atom predicate-node name nil truth-val)))))

;; Create inheritance link
(define inheritance
  (lambda (child parent . tv)
    (let ((truth-val (if (null? tv) unknown-tv (car tv))))
      (add-atom *atomspace*
                (make-atom inheritance-link nil 
                          (list child parent) truth-val)))))

;; Create similarity link
(define similarity
  (lambda (atom1 atom2 . tv)
    (let ((truth-val (if (null? tv) unknown-tv (car tv))))
      (add-atom *atomspace*
                (make-atom similarity-link nil 
                          (list atom1 atom2) truth-val)))))

;; Create evaluation link
(define evaluation
  (lambda (predicate args . tv)
    (let ((truth-val (if (null? tv) unknown-tv (car tv))))
      (add-atom *atomspace*
                (make-atom evaluation-link nil 
                          (list predicate args) truth-val)))))

;; Create list link
(define atom-list
  (lambda (atoms)
    (add-atom *atomspace*
              (make-atom list-link nil atoms unknown-tv))))

;; Query atomspace
(define query-atoms
  (lambda (atomspace type)
    (filter (lambda (atom)
              (eq? (atom-type atom) type))
            (atomspace-atoms atomspace))))

;; Get all incoming links for an atom
(define get-incoming
  (lambda (atomspace target-atom)
    (filter (lambda (atom)
              (and (not (null? (atom-outgoing atom)))
                   (member target-atom (atom-outgoing atom))))
            (atomspace-atoms atomspace))))

;; Get all outgoing atoms from a link
(define get-outgoing
  (lambda (atom)
    (atom-outgoing atom)))

;; Truth value operations
(define tv-merge
  (lambda (tv1 tv2)
    (let ((s1 (tv-strength tv1))
          (c1 (tv-confidence tv1))
          (s2 (tv-strength tv2))
          (c2 (tv-confidence tv2)))
      (let ((new-conf (+ c1 c2 (* c1 c2))))
        (if (> new-conf 0)
            (make-truth-value 
              (/ (+ (* s1 c1) (* s2 c2)) (+ c1 c2))
              new-conf)
            unknown-tv)))))

;; Simple inference rules
(define inheritance-rule
  (lambda (atomspace)
    ; If A inherits from B and B inherits from C, then A inherits from C
    (let ((inheritances (query-atoms atomspace inheritance-link)))
      (map (lambda (inh1)
             (map (lambda (inh2)
                    (let ((child1 (car (atom-outgoing inh1)))
                          (parent1 (cadr (atom-outgoing inh1)))
                          (child2 (car (atom-outgoing inh2)))
                          (parent2 (cadr (atom-outgoing inh2))))
                      (when (equal? parent1 child2)
                        (inheritance child1 parent2
                                   (tv-merge (atom-tv inh1) (atom-tv inh2))))))
                  inheritances))
           inheritances))))

;; Simple pattern matching for atoms
(define match-atom
  (lambda (pattern atom)
    (cond
      ((variable? pattern) (list (cons pattern atom)))
      ((and (atom? pattern) (atom? atom))
       (and (eq? (atom-type pattern) (atom-type atom))
            (equal? (atom-name pattern) (atom-name atom))
            (if (null? (atom-outgoing pattern))
                #t
                (match-outgoing (atom-outgoing pattern) 
                              (atom-outgoing atom)))))
      (else #f))))

(define match-outgoing
  (lambda (pattern-out atom-out)
    (cond
      ((and (null? pattern-out) (null? atom-out)) #t)
      ((or (null? pattern-out) (null? atom-out)) #f)
      (else
       (let ((head-match (match-atom (car pattern-out) (car atom-out))))
         (and head-match
              (match-outgoing (cdr pattern-out) (cdr atom-out))))))))

;; AtomSpace statistics
(define atomspace-size
  (lambda (atomspace)
    (length (atomspace-atoms atomspace))))

(define atom-count-by-type
  (lambda (atomspace type)
    (length (query-atoms atomspace type))))

;; Export/import atomspace
(define export-atomspace
  (lambda (atomspace)
    (map (lambda (atom)
           (list (atom-type atom) (atom-name atom) 
                 (atom-outgoing atom) (atom-tv atom)))
         (atomspace-atoms atomspace))))

(define import-atomspace
  (lambda (atomspace data)
    (map (lambda (atom-data)
           (add-atom atomspace
                     (make-atom (car atom-data) (cadr atom-data)
                               (caddr atom-data) (cadddr atom-data))))
         data)))

;; Attention allocation (simplified)
(define make-attention-value
  (lambda (sti lti vlti)
    (list 'attention-value sti lti vlti)))

(define av-sti cadr)
(define av-lti caddr)
(define av-vlti cadddr)

;; Add attention to atom
(define set-attention
  (lambda (atom av)
    (append atom (list av))))

;; Simple attentional focus
(define attentional-focus
  (lambda (atomspace threshold)
    (filter (lambda (atom)
              (and (> (length atom) 5) ; has attention value
                   (> (av-sti (car (last atom))) threshold)))
            (atomspace-atoms atomspace))))

;; Utility predicates
(define atom? (lambda (x) (and (list? x) (eq? (car x) 'atom))))
(define link? (lambda (x) (and (atom? x) (not (null? (atom-outgoing x))))))
(define node? (lambda (x) (and (atom? x) (null? (atom-outgoing x)))))

"AtomSpace bindings loaded successfully"