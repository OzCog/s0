;; Copyright (C) 2024 Copilot AI
;; Plan9-style namespace concepts for stage0
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

;; Plan9-style namespace implementation
;; Provides hierarchical naming, mounting, binding operations

;; Global namespace table
(define global-namespace-table (list))

;; Path utilities
(define path-separator "/")

(define split-path
  (lambda (path)
    (define (split-helper path acc current)
      (cond
        ((null? path) 
         (if (> (string-length current) 0)
             (reverse (cons current acc))
             (reverse acc)))
        ((char=? (car path) #\/)
         (if (> (string-length current) 0)
             (split-helper (cdr path) (cons current acc) "")
             (split-helper (cdr path) acc "")))
        (else
         (split-helper (cdr path) acc 
                       (string-append current (string (car path)))))))
    (if (string? path)
        (split-helper (string->list path) nil "")
        nil)))

(define join-path
  (lambda (components)
    (if (null? components)
        ""
        (fold-left (lambda (acc component)
                     (if (string=? acc "")
                         component
                         (string-append acc path-separator component)))
                   "" components))))

;; Namespace entry structure
(define make-namespace-entry
  (lambda (path type binding)
    (list 'namespace-entry path type binding)))

(define namespace-entry-path cadr)
(define namespace-entry-type caddr)
(define namespace-entry-binding cadddr)

;; Mount table entry
(define make-mount-entry
  (lambda (source target flags)
    (list 'mount source target flags)))

(define mount-source cadr)
(define mount-target caddr)
(define mount-flags cadddr)

;; Global mount table
(define mount-table (list))

;; Bind a name to a service/resource
(define ns-bind
  (lambda (path service)
    (let ((entry (make-namespace-entry path 'service service)))
      (set! global-namespace-table 
            (cons entry global-namespace-table))
      #t)))

;; Unbind a name
(define ns-unbind
  (lambda (path)
    (set! global-namespace-table
          (filter (lambda (entry)
                    (not (string=? (namespace-entry-path entry) path)))
                  global-namespace-table))
    #t))

;; Mount a namespace at a path
(define ns-mount
  (lambda (source target . flags)
    (let ((mount-entry (make-mount-entry source target 
                                         (if (null? flags) nil (car flags)))))
      (set! mount-table (cons mount-entry mount-table))
      #t)))

;; Unmount a namespace
(define ns-unmount
  (lambda (target)
    (set! mount-table
          (filter (lambda (entry)
                    (not (string=? (mount-target entry) target)))
                  mount-table))
    #t))

;; Resolve a path through the namespace
(define ns-resolve
  (lambda (path)
    (define (find-mount path)
      (filter (lambda (mount)
                (let ((target (mount-target mount)))
                  (and (<= (string-length target) (string-length path))
                       (string=? target 
                                (substring path 0 (string-length target))))))
              mount-table))
    
    (define (resolve-helper path)
      (let ((mounts (find-mount path)))
        (if (null? mounts)
            ;; No mount, look in global namespace
            (let ((entry (filter (lambda (entry)
                                   (string=? (namespace-entry-path entry) path))
                                 global-namespace-table)))
              (if (null? entry)
                  nil
                  (namespace-entry-binding (car entry))))
            ;; Found mount, resolve through it
            (let* ((mount (car mounts))
                   (target (mount-target mount))
                   (source (mount-source mount))
                   (relative (substring path (string-length target) 
                                      (string-length path))))
              (resolve-helper (string-append source relative))))))
    
    (resolve-helper path)))

;; Create a new namespace
(define make-namespace
  (lambda (name)
    (let ((ns (list 'namespace name (list))))
      (ns-bind (string-append "/ns/" name) ns)
      ns)))

(define namespace-name cadr)
(define namespace-entries caddr)

;; Add entry to namespace
(define ns-add-entry
  (lambda (namespace path binding)
    (let ((entry (make-namespace-entry path 'binding binding)))
      (set-car! (cddr namespace) 
                (cons entry (namespace-entries namespace)))
      #t)))

;; Process namespace for Plan9-style operations
(define make-process-namespace
  (lambda (pid)
    (list 'process-ns pid (list) (list))))

(define process-ns-pid cadr)
(define process-ns-bindings caddr)
(define process-ns-mounts cadddr)

;; Per-process namespace operations
(define process-bind
  (lambda (process-ns path service)
    (let ((binding (make-namespace-entry path 'service service)))
      (set-car! (cddr process-ns)
                (cons binding (process-ns-bindings process-ns)))
      #t)))

(define process-mount
  (lambda (process-ns source target flags)
    (let ((mount (make-mount-entry source target flags)))
      (set-car! (cdddr process-ns)
                (cons mount (process-ns-mounts process-ns)))
      #t)))

;; Union directory implementation
(define make-union-dir
  (lambda (directories)
    (list 'union-dir directories)))

(define union-dir-entries cadr)

(define union-lookup
  (lambda (union-dir name)
    (define (lookup-in-dirs dirs)
      (cond
        ((null? dirs) nil)
        (else
         (let ((result (ns-resolve (string-append (car dirs) "/" name))))
           (if result
               result
               (lookup-in-dirs (cdr dirs)))))))
    (lookup-in-dirs (union-dir-entries union-dir))))

;; Namespace server interface
(define make-ns-server
  (lambda (name handler)
    (list 'ns-server name handler)))

(define ns-server-name cadr)
(define ns-server-handler caddr)

;; Register a namespace server
(define register-ns-server
  (lambda (path server)
    (ns-bind path server)))

;; File-like interface for namespace operations
(define ns-open
  (lambda (path mode)
    (let ((binding (ns-resolve path)))
      (if binding
          (list 'ns-handle path binding mode)
          nil))))

(define ns-read
  (lambda (handle)
    (let ((binding (caddr handle)))
      (cond
        ((procedure? binding) (binding 'read))
        ((list? binding) binding)
        (else binding)))))

(define ns-write
  (lambda (handle data)
    (let ((binding (caddr handle)))
      (if (procedure? binding)
          (binding 'write data)
          #f))))

(define ns-close
  (lambda (handle)
    (let ((binding (caddr handle)))
      (if (procedure? binding)
          (binding 'close)
          #t))))

;; Environment manipulation
(define current-namespace nil)

(define with-namespace
  (lambda (namespace thunk)
    (let ((old-ns current-namespace))
      (set! current-namespace namespace)
      (let ((result (thunk)))
        (set! current-namespace old-ns)
        result))))

;; Import from other namespaces
(define ns-import
  (lambda (from-path to-path)
    (let ((binding (ns-resolve from-path)))
      (if binding
          (ns-bind to-path binding)
          #f))))

;; Clone namespace for process creation
(define clone-namespace
  (lambda (source-ns)
    (make-process-namespace
      (+ 1 (process-ns-pid source-ns))))) ; Simple PID increment

"Plan9-style namespace library loaded successfully"