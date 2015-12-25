#!/usr/local/bin/csi -s

(define (union s1 s2)
    (unique (append s1 s2)))

(define (unique s)
    (cond
        ((null? s) '())
        ((member (car s) (cdr s)) (unique (cdr s)))
        (else (cons (car s) (unique (cdr s))))))

(let ((map '((name bob) (foo bar) (hey now) (name john))))
    1
)

(define (remove elem set)
    (let ((value (car elem)))
        (cond
            ((null? set) '())
            ((eq? value (caar set)) (remove elem (cdr set)))
            (else (cons (car set) (remove elem (cdr set)))))))

(define (nodes head tail)
    (define (node)
        (list head (car tail)))
    (cond
        ((null? tail) '())
        ((eq? (cadar tail) '+) (nodes head (cdr tail)))
        (else (cons (node) (nodes head (cdr tail))))))

(define (graph intersection)
 (define (igraph iter-set)
  (if (null? iter-set)
        '()
    (let* ((head (car iter-set)) (tail (cdr iter-set)) (direction (cadr head)))
        (if (eq? direction '-)
                (igraph tail)
            (append (nodes head (remove head intersection)) (igraph tail))))))
    (igraph intersection))

; 1356
;(print (graph '(a b c  d e )))
;(print (remove '(1 +) '((1 +) (2 +) (3 +))))
;(let ((graph (graph '((a =) (b =) (c -) (d =) (e +)))))
(let ((graph (graph '((a =) (b =) (c =)))))
(print graph)
(print (length graph)))
