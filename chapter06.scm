; Stuff implemented in previous chapters, might be needed later.
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old (cons new (cdr lat))))
     (else (cons (car lat)
                 (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat)
                 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2)) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old
                                (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new
                                (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new
                                (multisubst new old (cdr lat))))
     (else (cons (car lat)
                 (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (o+ (add1 a) (sub1 b))))))

(define o-
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (o- (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

(define o*
  (lambda (a b)
    (cond
     ((zero? b) 0)
     (else (o+ a (o* a (sub1 b)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1)
           (null? tup2)) (quote ()))
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1)
                     (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (a b)
    (cond
     ((zero? a) #f)
     ((zero? b) #t)
     (else (o> (sub1 a) (sub1 b))))))

(define o<
  (lambda (a b)
    (cond
     ((zero? b) #f)
     ((zero? a) #t)
     (else (o< (sub1 a) (sub1 b))))))

(define o=
  (lambda (a b)
    (cond
     ((o> a b) #f)
     ((o< a b) #f)
     (else #t))))

(define o^
  (lambda (a b)
    (cond
     ((zero? b) 1)
     (else (o* a
               (o^ a
                   (sub1 b)))))))

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(define my-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (my-length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((o= n 1) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat)
                                (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2)) (= a1 a2))
     ((or (number? a1)
          (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (eqan? 1 n)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((eqan? a (car l)) (rember* a (cdr l)))
                       (else (cons (car l)
                                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((eqan? old (car l)) (cons old
                                                  (cons new (insertR* new old (cdr l)))))
                       (else (cons (car l)
                                   (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))))))
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eqan? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l))
               (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eqan? old (car l)) (cons new
                                  (subst* new old (cdr l))))
       (else (cons (car l)
                   (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((eqan? old (car l)) (cons new
                                                  (cons (car l)
                                                        (insertL* new old (cdr l)))))
                       (else (cons (car l)
                                   (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eqan? a (car l))
                          (member* a (cdr l))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define o-and
  (lambda (a b)
    (cond
     (a b)
     (else #f))))

(define o-or
  (lambda (a b)
    (cond
     (a #t)
     (else b))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1)
           (null? l2)) #t)
     ((or (null? l1)
          (null? l2)) #f)
     ((and (atom? (car l1))
           (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                  (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1))
          (atom? (car l2))) #f)
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1)
           (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1)
          (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist? ; uses (equal? s1 s2)
  (lambda (l1 l2)
    (cond
     ((and (null? l1)
           (null? l2)) #t)
     ((or (null? l1)
          (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (equal? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? s (car l)) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l)))))))

;;;;;;;;;;;;

; For the purpose of this chapter, an arithmetic expression is either an atom [including atoms],
; or two arithmetic expressions combined by +, * or ^.

(quote a)
; a [the atom]

(quote +)
; + [the atom, not the operation]

(quote *)
; * [the atom, not the operation]

(eq? (quote a) 'a)
; #t

(eq? 'a 'a)
; #t

; (n + 3) is not really an arithmetic expression since it involves parentheses, which our definition
; does not mention.
;
; However, we can all (n + 3) a representation of n+3. It is a good representation of n+3 since it is
; an S-expression that can serve as an argument for a function, and structurally resembers n+3.

; We have not yet implemented (numbered? ...), but we can figure out how it works by looking at a few
; examples:

(numbered? '1)
; #t

(numbered? '(3 + (4 ^ 5)))
; #t

(numbered? '(2 * sausage))
; #f, since 'sausage is not a number

; We can see now that (numbered? ...) determines whether a representation of an arithmetic expression
; contains only numbers, +, *, and ^.

; Here is my implementation:
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (quote +) (car (cdr aexp))) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp))))))
     ((eq? (quote *) (car (cdr aexp))) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp))))))
     ((eq? (quote ^) (car (cdr aexp))) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

; The book's implementation is virtually identical, though it does not have an else clause.
;
; Here are a few examples to make sure it works before we move on:

(numbered? 1)
; #t

(numbered? '(3 + (4 * 5)))
; #t

(numbered? '(2 * sausage))
; #f

; The book presents a simplification, but it doesn't examine the "middle" to see if it is +, *, or ^.
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))))))

; We have not yet implemented (value ...), but we can figure out how it works by looking at a few
; examples:

(value 13)
; 13

(value '(1 + 3))
; 4

(value '(1 + (3 ^ 4)))
; 82

(value 'cookie)
; no answer [error]

; Clearly (value aexp) returns the numerical value of the arithmetic expression 'aexp.
;
; Here is my implementation, assuming that the argument is a valid arithmetic expression:
(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (quote +) (car (cdr aexp))) (o+ (value (car aexp))
                                           (value (car (cdr (cdr aexp))))))
     ((eq? (quote *) (car (cdr aexp))) (o* (value (car aexp))
                                           (value (car (cdr (cdr aexp))))))
     ((eq? (quote ^) (car (cdr aexp))) (o^ (value (car aexp))
                                           (value (car (cdr (cdr aexp)))))))))

; We would like (value aexp) to work in each of the following cases:
; aexp is a number, or one of the atoms '+, '*, '^ followed by two arithmetic expressions.
;
; Here is my implementation of this new version of (value ...):
(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (quote +) (car aexp)) (o+ (value (car (cdr aexp)))
                                     (value (car (cdr (cdr aexp))))))
     ((eq? (quote *) (car aexp)) (o* (value (car (cdr aexp)))
                                     (value (car (cdr (cdr aexp))))))
     ((eq? (quote ^) (car aexp)) (o^ (value (car (cdr aexp)))
                                     (value (car (cdr (cdr aexp)))))))))

; The book gives an incorrect implementation that does not call (car ...) before (value ...). This
; causes problems since, for example, (cdr '(+ 1 3)) will be the list '(1 3), which is not a valid
; arithmetic expression.
;
; Rather than simple use (car ...) as I have done above, the book proposes implementing helper
; functions to pick out the parts of a given arithmetic expression.
;
; The procedure (1st-sub-exp aexp) returns the first arithemtic expression found in a compound
; arithmetic expression [i.e., not just a number].
;
; Here is my implementation:
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; Here is my implementation of the procedure (2nd-sub-exp aexp) that returns the first arithemtic
; expression found in a compound arithmetic expression:

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; Finally, here is my implementation of the procedure (operator aexp) that returns the arithmetic
; operation found in a compound arithmetic expression:

(define operator
  (lambda (aexp)
    (car aexp)))

; We can now rewrite (value ...) using these helper functions:
(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (quote +) (operator aexp)) (o+ (value (1st-sub-exp aexp))
                                          (value (2nd-sub-exp aexp))))
     ((eq? (quote *) (operator aexp)) (o* (value (1st-sub-exp aexp))
                                          (value (2nd-sub-exp aexp))))
     ((eq? (quote ^) (operator aexp)) (o^ (value (1st-sub-exp aexp))
                                          (value (2nd-sub-exp aexp)))))))

; Note that with this implementation of (value ...), we can simply change the definitions of the
; helper functions (operator ...) and (1st-sub-exp ...) if we want (value ...) to be able to handle
; arithmetic expression such as (1 + 3), to wit:

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

; Note that we can represent numbers as follows:
; zero  : ()
; one   : (())
; two   : (() ())
; three : (() () ())
; four  : (() () () ())
; etc.

; Using these representations for numbers, we can redefine our basic arithemtic procedures. For
; example, the following tests for zero:
(define sero?
  (lambda (n)
    (null? n)))

; The following procedure adds one to its argument:
(define edd1
  (lambda (n)
    (cons (quote ()) n)))

; The following procedure subtracts one from its argument:
(define zub1
  (lambda (n)
    (cdr n)))

; The foloowing procedure adds two arguments:
(define oh+
  (lambda (a b)
    (cond
     ((null? b) a)
     (else (oh+ (edd1 a) (zub1 b))))))

; Some examples before we move on:

(sero? '())
; #t

(sero '(() ()))
; #f

(edd1 '())
; (())

(edd1 '(() ()))
; (() () ())

(zub1 '(() ()))
; (())

(oh+ '(()) '(() ()))
; (() () ())

; Finally, we note that while:
(lat? '(1 2 3))
; #t, since (1 2 3) is a list of atoms...
(lat? '((()) (() ()) (() () ())))
; #f, since the representations we have used for the atoms 1, 2, and 3 are not actually atoms.
