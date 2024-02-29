(define not
  (lambda (x) (if x #f #t)))

  (define zero? 
  (let ((= =))
    (lambda (x) (= x 0))))

(define list (lambda x x))

(define list? 
  (let ((null? null?)
	(pair? pair?)
	(cdr cdr))
    (letrec ((list?-loop
	      (lambda (x)
		(or (null? x)
		    (and (pair? x)
			 (list?-loop (cdr x)))))))
      list?-loop)))


(define -
  (let ((apply apply)
	(+ +)
	(null? null?))
    (lambda (x . y)
      (if (null? y)
	  (+ 0 (* -1 x))
	  (+ x (* -1 (apply + y)))))))
	  

