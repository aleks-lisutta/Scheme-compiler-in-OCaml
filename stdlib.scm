
(define fold-left (let ((null? null?) (pair? pair?) (car car) (cdr cdr)) 
	(lambda (func acc lst) (
		if (null? lst)
			acc
		(
			if (pair? lst)
				(fold-left func (func acc (car lst)) (cdr lst))
				(
					func acc lst
				)
		)
	))))
  

(define fold-right(let ((null? null?) (pair? pair?) (car car) (cdr cdr)) 
	(lambda (func lst acc) (
		if (null? lst)
			acc
			(
				if (pair? lst)
				(func (car lst) (fold-right func (cdr lst) acc))
				(func lst ) 
			)
	)))
)


(define cons* (let ((null? null?) (cons cons) (pair? pair?) (car car) (cdr cdr))(
	lambda (a b . c) (
		cond ((null? c) (cons a b)) ((pair? c) (cons a (cons* b (car c) (cdr c)))) (else (cons a (cons b c)))
	)
))
)

(define append
  (let ((null? null?)
	(fold-right fold-right)
	(cons cons))
    (lambda args
      (fold-right
       (lambda (e a)
	 (if (null? a)
	     e
	     (fold-right cons a e)))
       '() args))))

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

(define not
  (lambda (x) (if x #f #t)))


(define gcd
  (let ((gcd gcd) (null? null?)
	(car car) (cdr cdr))
    (letrec ((gcd-loop
	      (lambda (x ys)
		(if (null? ys)
		    x
		    (gcd-loop (gcd x (car ys)) (cdr ys))))))
      (lambda x
	(if (null? x)
	    0
	    (gcd-loop (car x) (cdr x)))))))

(define zero? 
  (let ((= =))
    (lambda (x) (= x 0))))

(define integer?
  (let ((rational? rational?)
	(= =)
	(denominator denominator))
    (lambda (x)
      (and (rational? x) (= (denominator x) 1)))))

(define length
  (let ((fold-left fold-left)
	(+ +))
    (lambda (l)
      (fold-left (lambda (acc e) (+ acc 1)) 0 l))))

(define string->list
  (let ((string-ref string-ref)
	(string-length string-length)
	(< <) (- -) (cons cons))
    (lambda (s)
      (letrec
	  ((s->l-loop
	    (lambda (n a)
	      (if (< n 0)
		  a
		  (s->l-loop (- n 1) (cons (string-ref s n) a))))))
	(s->l-loop (- (string-length s) 1) '())))))

(define equal?
  (let ((= =) (string->list string->list)
	(rational? rational?) (flonum? flonum?)
	(pair? pair?) (char? char?)
	(string? string?) (eq? eq?)
	(car car) (cdr cdr)
	(char->integer char->integer))
    (letrec ((equal?-loop
	      (lambda (x y)
		(cond
		 ((and (rational? x) (rational? y)) (= x y))
		 ((and (flonum? x) (flonum? y)) (= x y))
		 ((and (char? x) (char? y)) (= (char->integer x) (char->integer y)))
		 ((and (pair? x) (pair? y))
		  (and (equal?-loop (car x) (car y)) (equal?-loop (cdr x) (cdr y))))
		 ((and (string? x) (string? y)) (equal?-loop (string->list x) (string->list y)))
		 (else (eq? x y))))))
    equal?-loop)))
