; Required for some code in the text
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

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

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

(atom? 14)
; #t since all numbers are atoms
;
; Note: In this chapter, all numbers are nonnegative integers.

; Here is the implementation of (add1 n):
(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)
; 68

; Here is the implementation of (sub1 n):
(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
; 4

; Again, for some reason, we are restricted to nonnegative integers, so (sub1 0) has no answer.

(zero? 0)
; #t

(zero? 1492)
; #f

; We now implement our own addition procedure use (add1 ...) and (sub1 ...):
(define o+
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (o+ (add1 a) (sub1 b))))))

; Here is the book's version:
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12)
; 58

; We now implement our own subtraction procedure using (sub1 ...):
(define o-
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (o- (sub1 a) (sub1 b))))))

;
; Here is the book's version:
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

; A list of numbers is a tuple, or tup. We can add up the members of a tup with (addtup ...), which
; we now implement:
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

(addtup '())
; 0

(addtup '(3 5 2 8))
; 18

(addtup '(15 6 7 12 3))
; 43

; We now implement our own multiplication procedure using (sub1 ...):
(define o*
  (lambda (a b)
    (cond
     ((zero? b) 0)
     (else (o+ a (o* a (sub1 b)))))))

; The book uses m and n, but is otherwise identical.

(o* 5 3)
; 15

(o* 13 4)
; 52

(o* 12 3)
; 36

; The procedure (tup+ tup1 tup2) adds the first element of tup1 and the first element of tup 2, then
; the second element of tup1 and the second element of tup2, etc.
;
; Here is my implementation:
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1)
           (null? tup2) (quote ())))
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
; (11 11 11 11 11)

(tup+ '(2 3) '(4 6))
; (6 9)

(tup+ '(3 7) '(4 6))
; (7 13)

; Note: This implementation of (tup+ tup1 tup2) only works if tup1 and tup2 have the same length. If
; we want an implementation that works when tup1 and tup2 do NOT have the same lenght, treating the
; shorter tup as if it had zeros at the end to make it the same length as the longer tup, then we
; must rewrite the procedure.
;
; Here is my first implementation:
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1)
           (null? tup2) (quote ())))
     ((and (not (null? tup1))
           (not (null? tup2))) (cons (o+ (car tup1) (car tup2))
                                     (tup+ (cdr tup1) (cdr tup2))))
     ((null? tup1) (cons (o+ 0 (car tup2))
                         (tup+ (quote ()) (cdr tup2))))
     (else (cons (o+ (car tup1) 0)
                 (tup+ (cdr tup1) (quote ())))))))
;
; Though this implementation works, it can be simplified quite a bit. Of course, we don't need to
; call o+ just to add zero. Also, if either tup1 or tup2 is empty (but not both), then we can just
; return the rest of whichever is not empyt.
;
; Here is a better implementation:
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1)
           (null? tup2)) (quote ()))
     ((and (not (null? tup1))
           (not (null? tup2))) (cons (o+ (car tup1)
                                         (car tup2))
                                     (tup+ (cdr tup1) (cdr tup2))))
     ((null? tup2) tup1)
     (else tup2))))
;
; Finally, we can simplify it a bit more by reordering the conditions examine in (cond ...).
; Checking the null cases for tup1 or tup2 after the case where both are null eliminates the need to
; check if both are not null.
;
; Here is my final implementation:
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

(tup+ '(3 7) '(4 6 8 1))
; (7 13 8 1)

(tup+ '(3 7 8 1) '(4 6))
; (7 13 8 1)

; We now implement our own greater-than procedure, assuming the given numbers are non-negative:
(define o>
  (lambda (a b)
    (cond
     ((zero? a) #f)
     ((zero? b) #t)
     (else (o> (sub1 a) (sub1 b))))))
;
; We note that the book's initial (incorrect) implementation reverses the order of the two
; (zero? ...) questions. This will yield incorrect results in the case where a=b, since eventually
; (o> a b) will become (o> 0 0), which should be false.

(o> 12 133)
; #f

(o> 120 11)
; #t

(o> 3 3)
; #f

; We now implement our own less-than procedure, assuming the given numbers are non-negative:
(define o<
  (lambda (a b)
    (cond
     ((zero? b) #f)
     ((zero? a) #t)
     (else (o< (sub1 a) (sub1 b))))))

(o< 4 6)
; #t

(o< 8 3)
; #f

(o< 6 6)
; #f

; We now implement our own equality procedure, assuming the given numbers are non-negative:
(define o=
  (lambda (a b)
    (cond
     ((o> a b) #f)
     ((o< a b) #f)
     (else #t))))
;
; I used the previously-defined procedures (o> ...) and (o< ...). The books gives an implementation
; that does not use these procedures:
(define o=
  (lambda (a b)
    (cond
     ((zero? b) (zero? a))
     ((zero? a) #f)
     (else (o= (sub1 a) (sub1 b))))))

; Here are a few examples before we move on:

(o= 1 1)
; #t

(o= 1 2)
; #f

(o= 2 1)
; #f

; We have not yet written the exponentiation function (o^ a b), but we can look at some examples to
; see how it works:

(o^ 1 1)
; 1

(o^ 2 3)
; 8

(o^ 5 3)
; 125

; We now implement (o^ a b), which computes a^b, assuming a and b are non-negative integers:
(define o^
  (lambda (a b)
    (cond
     ((o= 1 b) a)
     (else (o* a
               (o^ a
                   (sub1 b)))))))
;
; This implementation does not handle the case where the exponent is zero. The book's implementation
; does, though it is incorrect in that (o^ 0 0) returns 1, which is technically incorrect since 0^0
; is undefined!
;
; Oh, well. Here is the book's implementation:
(define o^
  (lambda (a b)
    (cond
     ((zero? b) 1)
     (else (o* a
               (o^ a
                   (sub1 b)))))))

;
; Here is a strange procedure:
(define ???
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (??? (o- n m) m))))))
;
; This procedure returns the greatest integer, k, such that k*m<=n. In most programming languages,
; this is what you get if you ask for n/m, where n and m are both integers.
;
; Let's give this procedure a proper name:
(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(o/ 15 4)
; 3

; We have not yet implemented (length ...), but we can figure out how it works from some examples:

(length '(hotdogs with mustard sauerkraut and pickles))
; 6

(length '(ham and cheese on rye))
; 5

; Here is my implementation of (length ...):
(define my-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (my-length (cdr lat)))))))

; We have not yet implemented (pick ...), but we can figure out how it works from some examples:

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
; macaroni

(pick 0 '(a))
; No answer, error

; Here is my implementation of (pick ...):
(define pick
  (lambda (n lat)
    (cond
     ((o= n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

; Here is the book's implementation, which I don't like as much since it subtracts 1 from n no
; matter what, but we should just return (car lat) and that's it if n=1. I guess they like theirs
; better since it uses (zero? ...) instead of (o= ...)
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

; We have not yet implemented (rempick ...), but we can figure out how it works from an example:

(rempick 3 '(hotdogs with hot mustard))
; (hotdogs with mustard)

; Here is my implementation of (rempick ...):
(define rempick
  (lambda (n lat)
    (cond
     ((o= n 1) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

; Again, the book's implementation uses (zero? ...) but is otherwise identical:
(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

(number? 'tomato)
; #f

(number? 76)
; #t

; (number? ...) is a primitive function that returns true if it is given a numeric atom,
; otherwise false.

; We have not yet implemented (no-nums ...), but we can figure out how it works with an example:

(no-nums '(5 pears 6 prunes 9 dates))
; (pears prunes dates)

; Here is my implementation of (no-nums ...):
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

; The following procedure forms a tup by extracting the numbers from a given lat:
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat)
                                (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

; The following procedure returns true if its two arguments are the same atom:
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2)) (= a1 a2))
     (else (eq? a1 a2)))))

; Here is the book's implementation, which returns false if one of the arguments is a number and
; the other is a non-number atom [both can't be a number by the time we get to the second condition].
; I think the only reason the book includes this condition is because it wants us to use (= ...) only
; with numbers and (eq? ...) only with non-numbers, so the mixed case should not call either. Anyway,
; here it is:
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2)) (= a1 a2))
     ((or (number? a1)
          (number? a2)) #f)
     (else (eq? a1 a2)))))

; (eqan? ...) can now be used anywhere (= ...) or (eq? ...) can be used.

; Here is my implementation for (occur ...) which counts the number of times a given atom occurs in
; a lat:
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

; An example before we move on:
(occur 1 '(1 hi 1 there 1 1))
; 4

; Here is my implementation for (one? n) which returns true if n is the number 1, otherwise false.
(define one?
  (lambda (n)
    (eqan? 1 n)))

; The book uses (= 1 n), but is otherwise identical.
;
; Here is my implementation of (rempick ...) which removes the nth atom from a lat:
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(lemon meringue salty pie))
; (lemon meringue pie)
