#lang scheme
; 2016400219

;You can replace #f's with your function definitions and define more helper functions as you need to use this template.
(define bos'())

; Solver function
(define TENTS-SOLUTION(lambda (list) ( if(not(and (eq? (SUM-LIST (car list)) (SUM-LIST (cadr list))) (eq? (SUM-LIST (car list)) (LENGTH (caddr list))))) #f (solve (car list) (cadr list) (caddr list) '() 1 '() )  )))

(define REPLACE-NTH(lambda (list1 n elem)
    (cond
    [(null? list1) '()]
    [(eq? n 1) (cons elem (cdr list1))]
    [(> n 0) (cons (car list1) ( REPLACE-NTH (cdr list1) (- n 1) elem))])))

; Helper functions
(define RETURN-FIRST-NOT-FALSE #f)

(define ADJACENT (lambda (list1 list2) (cond [(< (abs (- (car list1) (car list2))) 2) (cond [(< (abs (- (cadr list1) (cadr list2))) 2) #t] [else #f])] [else #f])))


(define ADJACENT-WITH-LIST (lambda (list1 list2) (if (or (eq?(LENGTH list1) 0) (eq? (LENGTH list2) 0)) #f (if(ADJACENT list1 (car list2)) #t (ADJACENT-WITH-LIST list1 (cdr list2))  ))))

(define NEIGHBOR-LIST (lambda (list2) (append bos (append (append (append (cons (cons (car list2) (cons (- (cadr list2) 1) '())) '()) (cons (cons (- (car list2) 1) (cons (cadr list2) '())) '())) (cons (cons (car list2) (cons (+ (cadr list2) 1) '())) '()) )  (cons (cons (+ (car list2) 1) (cons (cadr list2) '())) '())))   ))

(define TRUE-APPEND (lambda (lst x)(cond((null? lst) (cons x '())) (else (cons (car lst) (append (cdr lst) x))))))

;My helpers

(define LENGTH (lambda (list1) (if(null? list1) 0 (+ (LENGTH (cdr list1)) 1))))
(define SUM-LIST(lambda  (elemList) (if (null? elemList) 0 (+ (car elemList) (SUM-LIST (cdr elemList))))))

(define COLUMN-COUNT-CHECK(lambda (columns solt) (if(null? solt) columns  (COLUMN-COUNT-CHECK (columnHelper columns solt) (cdr solt)) ) ))
(define columnHelper(lambda (columns solt) (REPLACE-NTH columns (cadr (car solt)) (- (TAKE-NTH-ELEMENT columns (cadr (car solt))) 1))))

(define ROW-COUNT-CHECK(lambda (rows solt) (if(null? solt) rows  (ROW-COUNT-CHECK (rowHelper rows solt) (cdr solt)) ) ))
(define rowHelper(lambda (rows solt) (REPLACE-NTH rows (car (car solt)) (- (TAKE-NTH-ELEMENT rows (car (car solt))) 1))))

(define ALL-ELEMENTS-BIGGER-ZERO(lambda (list1) (if(null? list1) #t (if(>= (car list1) 0) (ALL-ELEMENTS-BIGGER-ZERO (cdr list1)) #f ))  ) )

(define TAKE-NTH-ELEMENT(lambda (list1 n) (cond [(eq? n 1) (car list1)] [else (TAKE-NTH-ELEMENT (cdr list1) (- n 1))])  ))

(define IS-POSITION-ON-THE-BOARD(lambda (x y row column) (if(and (> x 0) (> y 0) (<= x row) (<= y column)) #t #f) ))

(define IS-PLACE-EMPTY(lambda (treelist x y) (if(null? treelist) #t (if (and (eq? (car(car treelist)) x) (eq? (cadr(car treelist)) y)) #f (IS-PLACE-EMPTY (cdr treelist) x y)  ))))

(define EQUAL-CANTBE(lambda (cantbe neighborlist) (if(eq? (LENGTH cantbe) 0) #f (if(and (eq? (car cantbe) (car (car neighborlist))) (eq? (cadr cantbe) (cadr (car neighborlist))) ) #t #f )  ) ))
(define CHECKER (lambda (row column trees solt neighborlist cantbe) (if(eq? (LENGTH neighborlist) 0) #f (if(eq? (LENGTH cantbe) 0) (if(and (IS-POSITION-ON-THE-BOARD (car (car neighborlist)) (cadr (car neighborlist)) (LENGTH row) (LENGTH column)) (IS-PLACE-EMPTY trees (car (car neighborlist)) (cadr (car neighborlist))) (IS-PLACE-EMPTY solt (car (car neighborlist)) (cadr (car neighborlist))) (not (EQUAL-CANTBE cantbe neighborlist)) (ALL-ELEMENTS-BIGGER-ZERO (COLUMN-COUNT-CHECK column (cons (car neighborlist) solt))) (ALL-ELEMENTS-BIGGER-ZERO (ROW-COUNT-CHECK row (cons (car neighborlist ) solt))) (not (ADJACENT-WITH-LIST (car neighborlist) solt))) (car neighborlist) (CHECKER row column trees solt (cdr neighborlist) cantbe)) (CHECKER row column trees solt (AFTER-CANT-BE neighborlist cantbe) '()))  )))

(define AFTER-CANT-BE(lambda (neighborlist cantbe) (if(and (eq? (car (car neighborlist)) (car cantbe)) (eq? (cadr (car neighborlist)) (cadr cantbe))) (cdr neighborlist) (AFTER-CANT-BE (cdr neighborlist) cantbe))))

(define solve (lambda (row column trees ert counter cantbe) (if(eq? (LENGTH ert) (SUM-LIST row))
                                                               ert
                                                               (if(not(eq? (CHECKER row column trees ert (NEIGHBOR-LIST (TAKE-NTH-ELEMENT trees counter)) cantbe) #f))
                                                                  (solve row column trees (cons (CHECKER row column trees ert (NEIGHBOR-LIST (TAKE-NTH-ELEMENT trees counter)) cantbe) ert) (+ counter 1) '())
                                                                  (solve row column trees (cdr ert) (- counter 1) (car ert))
                                                                  )   
                                                            )
                ))

(define REMOVE-LAST(lambda (lst) (if (null? (cdr lst)) '() (cons (car lst) (REMOVE-LAST (cdr lst))))))
