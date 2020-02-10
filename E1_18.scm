(define swapper
    (lambda (s1 s2 slist)
        (cond
            ((null? slist) '())
            ((eq? (car slist) s1) (cons s2 (swapper s1 s2 (cdr slist))))
            ((eq? (car slist) s2) (cons s1 (swapper s1 s2 (cdr slist))))
            (else (cons (car slist) (swapper s1 s2 (cdr slist)))))))

(display (swapper 'a 'b '(a b c d c b a b)))
(newline)
