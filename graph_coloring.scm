#!/usr/local/bin/csi -s

(use srfi-1)

(define (union s1 s2)
    (unique (append s1 s2)))

(define (unique s)
    (cond
        ((null? s) '())
        ((member (car s) (cdr s)) (unique (cdr s)))
        (else (cons (car s) (unique (cdr s))))))

(define (iremove elem set)
	(remove (lambda (x) (eq? (car x) (car elem))) set))

(define (nodes head tail)
    (define (node)
        (list head (car tail)))
    (cond
        ((null? tail) '())
        ((eq? (cadar tail) '+) (nodes head (cdr tail)))
        (else (cons (node) (nodes head (cdr tail))))))

(define (graph desc)
 (define (mk-nodes i-desc)
  (if (null? i-desc)
        '()
    (let* ((head (car i-desc)) (tail (cdr i-desc)) (direction (cadr head)))
        (if (eq? direction '-)
                (mk-nodes tail)
            (append (nodes head (iremove head desc)) (mk-nodes tail))))))
 (edges (mk-nodes desc)))

(define (cross? n1 n2 nodes)
	(let ((start1 (caar n1)) (end1 (caadr n1))
	      (start2 (caar n2)) (end2 (caadr n2)))
		(print 'hey: start1 end1)	
		(print 'hey: start2 end2))	
	;; search for start1 then from there search for end1 or end,
	;; if found return false else true
 #t)


	
(define (edges nodes)
	(define (iloop node nodes)
		(cond
			((null? nodes) '())
			((overlap node (car nodes))
				(cons 'crash (iloop node (cdr nodes))))
			(else (iloop node (cdr nodes)))))
	(define (iedges ns)
		(cond
			((null? ns) '())
			(else (cons (car ns) (iedges (cdr ns))))))
	(iedges nodes))

; add (car node) to seen list, and if you see the same thing twice
; then return some error
(define (align nodes node)
	(if (eq? (car nodes) node)
			nodes
		(align (cdr nodes) node)))

(define (overlap node1 node2 streets)
	(let* ((aligned-streets (align (apply circular-list streets) (caar node1)))
	       (start (car aligned-streets)) (sentinel (caadr node1))
	      (targets (list (caar node2) (caadr node2))))
		(define (loop streets)
			(let ((street (car streets)))
			(cond
				((member street targets) #t)
				((eq? start street) #f)
				((eq? sentinel street) #f)
				(else (loop (cdr streets)))
			)))
		(loop (cdr aligned-streets))))
	
	
(assert (iremove '(2 +) '((1 +) (2 +) (3 +))) '((1 +) (3 +)))

;(print  (cadr (align (circular-list '1 '2 '3 '4) '4)))
;(print (overlap '(1 2 3 4 5) '(1 2) '(4 5)))

(assert (overlap '((a =) (c =)) '((b =) (d =)) '(a b c d)))
(assert (not (overlap '((a =) (b =)) '((c =) (d =)) '(a b c d))))
(assert (overlap '((a =) (c =)) '((b =) (d =)) '(a b c d)))
(assert (overlap '((a =) (b =)) '((b =) (d =)) '(a b c d)))
(assert (overlap '((a =) (c =)) '((d =) (b =)) '(a b c d)))
(assert (overlap '((e =) (b =)) '((a =) (c =)) '(a b c d e)))
