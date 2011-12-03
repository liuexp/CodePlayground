(display "This is a compiled test by Liuexp.")
(display "把我打印出来")
(define LIUEXP 1)
(and
#t ;我是注释
(not #f)
(equal? (* 111111111 111111111) 12345678987654321)
(equal? (+ 11111 11111) 22222)

(
; XD
 (lambda (a b c)
; XD
  (and
; XD
   (equal? (b (a 1 2)) 1)
; XD
   (equal? (c (a 1 2)) 2)
; XD
  )
; XD
 )
; XD
 (lambda (x y)
; XD
  (lambda (m) (m x y))
; XD
 )
; XD
 (lambda (x) (x (lambda (aa bb) aa)))
; XD
 (lambda (x) (x (lambda (aa bb) bb)))
; XD
)
; this is a comment
	;can you recognize me?; zzzzz
; XD
(= 0 0+0i)
;我是 传说中的 中文
(> 3 2.0)
(eqv? "中文" "中文")
(if (equal? (abs -7) 7) #t #f)
(equal? (car '(1 2 3 4 5)) 1)
(equal? (cadr '(1 2 3 4 5)) 2)
(equal? (cadr '(1 2)) 2)
(equal? (cddr '(1 2)) nil)
(equal? (+ 1 2 3 4 5 6 7 8 9 10) 55)
(= (+ 1 2 3 4 5+0i 6 7 8 9 10) 55)
(= (+ 1 2 3 4 5+0i 6/1 7 8 9 10) 55)
(= (+ 1 2 3 4 -5+0i 6/1 7 8 9 10) 45)
(= (+ 3+4i 2+9i) (+ 2+4i 3+9i))

;for closure; note that after execution x would remains undefined as expected.
;one can check that in the interpreter.
(equal? 
 (
  (lambda (x)  (define x 1)
	       (set! x 2)
	       (+ x 1)
   ) 9)
 3
 )
;closure again
(= LIUEXP 1)
(equal?
 ((lambda (x) (set! LIUEXP x)
	      (+ 4 5)
	      (+ 9999 1)
  ) 10)
 10000
 )
(equal? LIUEXP 10)

)

