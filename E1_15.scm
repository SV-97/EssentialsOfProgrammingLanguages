(define duple
    (lambda (n x)
        (if (zero? n)
            '()
            (cons x (duple (- n 1) x)))))


(display (duple 2 3))
(newline)
(display (duple 4 '(ha ha)))
(newline)
