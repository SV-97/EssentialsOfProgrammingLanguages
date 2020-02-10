(define exists?
    (lambda (pred lst)
        (cond
            ((null? lst) #f)
            ((pred (car lst)) #t)
            (else (exists? pred (cdr lst))))))

(display (exists? number? '(a b c 3 e)))
(newline)
(display (exists? number? '(a b c d e)))
(newline)
(exit)