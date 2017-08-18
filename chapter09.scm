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

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (quote +) (operator aexp)) (o+ (value (1st-sub-exp aexp))
                                          (value (2nd-sub-exp aexp))))
     ((eq? (quote *) (operator aexp)) (o* (value (1st-sub-exp aexp))
                                          (value (2nd-sub-exp aexp))))
     (else (o^ (value (1st-sub-exp aexp))
               (value (2nd-sub-exp aexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oh+
  (lambda (a b)
    (cond
     ((null? b) a)
     (else (oh+ (edd1 a) (zub1 b))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat)
                 (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define set-diff ; book calls this (xxx ...)
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2) (xxx (cdr set1) set2))
     (else (cons (car set1)
                 (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                (quote ())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(define seconds
  (lambda (rel)
    (firsts (revrel rel))))

(define fullfun? ; this is just (one-to-one ...)
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one
  (lambda (fun)
    (fun? (revrel fun))))

(define rember-f
  (lambda (test? s l)
    (cond
     ((null? l) (quote ()))
     ((test? s (car l)) (cdr l))
     (else (cons (car l)
                 (rember-f test? s (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (s l)
      (cond
       ((null? l) (quote ()))
       ((test? s (car l)) (cdr l))
       (else (cons (car l)
                   ((rember-f test?) s (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old) (cons new (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((eq? old (car l)) (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g seqL))

(define insertR
  (insert-g seqR))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy ; this is just rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) o+)
     ((eq? x (quote *)) o*)
     (else o^))))

(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     (else ((atom-to-function (operator aexp))
            (value (1st-sub-exp aexp))
            (value (2nd-sub-exp aexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat)) (multiremberT test? (cdr lat)))
     (else (cons (car lat)
                 (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col (quote ()) (quote ())))
     ((eq? a (car lat)) (multirember&co a (cdr lat) (lambda (newlat seen)
                                                      (col newlat (cons (car lat)
                                                                        seen)))))
     (else (multirember&co a (cdr lat) (lambda (newlat seen)
                                         (col (cons (car lat)
                                                    newlat)
                                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? oldL (car lat)) (cons new
                                 (cons oldL
                                       (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? oldR (car lat)) (cons oldR
                                 (cons new
                                       (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col (quote ()) 0 0))
     ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                             (lambda (newlat L R)
                                               (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                             (lambda (newlat L R)
                                               (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat) L R)))))))

(define first-friend
  (lambda (lat x y)
    lat))

(define second-friend
  (lambda (lat x y)
    x))

(define third-friend
  (lambda (lat x y)
    y))

(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l)) (cond
                       ((even? (car l)) (cons (car l)
                                              (evens-only* (cdr l))))
                       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col (quote ()) 1 0))
     ((atom? (car l)) (cond
                       ((even? (car l))
                        (evens-only*&co (cdr l) (lambda (newl p s)
                                                  (col (cons (car l) newl) (o* (car l) p) s))))
                       (else (evens-only*&co (cdr l) (lambda (newl p s)
                                                       (col newl p (o+ (car l) s)))))))
     (else (evens-only*&co (car l) (lambda (al ap as) ; "a" stands for the "a" in car
                                     (evens-only*&co (cdr l) (lambda (dl dp ds) ; "d" stands for the "d" in cdr
                                                               (col (cons al dl) (o* ap dp) (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

;;;;;;;;;;;;

; We have not yet implemented (looking ...) but we can figure out what it does by looking at a couple
; of examples:

(looking 'caviar '(6 2 4 caviar 5 7 3))
; #t since the first element is 6, and the sixth element is 7, and the seventh element is 3, and the
; third element is 'caviar

(looking 'caviar '(6 2 grits caviar 5 7 3))
; #f since the first element is 6, and the sixth element is 7, and the seventh element is 3, and the
; third element is 'grits, which is not 'caviar

; The book recommends that we implement (looking ...) with the help of (keep-looking ...) and
; (pick ...):
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; Let's try to write (keeping-looking ...):
(define keep-looking
  (lambda (a p lat)
    (cond
     ((null? lat) #f)
     ((eq? a p) #t)
     ((number? p) (keep-looking a (pick p lat) lat))
     (else #f))))

; I think my version is equivalent to the book's, though the books is clearly shorter:
(define keep-looking
  (lambda (a sorn lat) ; "sorn" stands for "symbol or number"
    (cond
     ((number? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

; The book calls (looking ...) a "partial" function because the function does not have a definite
; termination condition. For example:
(looking 'caviar '(7 2 4 7 5 6 3))
; will produce an infinite-loop: 7 -> 3 -> 4 -> 7 -> 3 -> 4 -> 7 ...
;
; All of the functions we have written so far have been "total" functions.
;
; Here is an example of another "partial" function:
(define eternity
  (lambda (x)
    (eternity x)))
;
; (eternity ...) will produce an infinite loop for any input x.

; We have not yet written (shift ...) but we can figure out what it does with a couple of examples:

(shift '((a b) c))
; (a (b c))

(shift '((a b) (c d)))
; (a (b (c d)))

; In words, here is what (shift ...) does: It takes a pair whose first component is a pair and builds
; a pair by shifting the second part of the first component into the second component.
;
; Here is the book's implementation, which uses (first ...), (second ...), and (build ...):
(define shift
  (lambda (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))

; Here is another function:
(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

; Notice that (aling ...) seems like it might be a "partial" function since it does not have a definite
; termination condition.
;
; In particular, the second condition calls (align ...) on the shift of pora, which is not part of the
; original argument. This new argument is not guaranteed to be "smaller"...but what do we mean by
; "smaller"? We need some sort of "measure" before we can say one thing is "smaller" than another.
;
; The book proposes measuring by counting the number of atoms, which is what the following procedure
; does:
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (length* (first pora))
               (length* (second pora)))))))
;
; However, this is not a useful measurement since (align ...) does not create or destroy atoms in the
; given 'pora, it just shifts pairing parentheses to the right while the (first ...) or pora is still a
; pair. Therefore the length* of a pora will always be the same as the shifted pora. In other words, we
; cannot measure any progress [or even any difference] in the input and out of the second condition of
; (align ...).
;
; Instead, consider the following procedure:
(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (o* (weight* (first pora)) 2)
               (weight* (second pora)))))))
;
; To understand why this is a better measurement, and more importantly what this has to do with the
; termination of (align ...), note that the weight* of a shifted pora is strictly less than that of
; the original pora, given that pora is a pair (pora1 pora2) where pora1 is a pair. Therefore, we have
; a strictly decreasing sequence of positive integers associated with each recursion of (align ...)
; in its second condition. This eventually must terminate since such a sequence has a lower bound of
; one. We conclude that we cannot get stuck in the second condition indefinitely.
;
; Similarly, the thir [else] condition sends (second pora) back to (align ...), a "smaller" pora, hence
; we cannot get stuck in this condition indefinitely, either.
;
; It follows that eventually we must reach the first condition, which is the beginning of the end of
; the procedure [just need to (build ...) the final result].
;
; We can now conclude that (align ...) is NOT a "partial" function, and that it returns a value for
; every valid input.

; Here is another function, similar to (align ...), except it uses (revpair ...) instead of
; (shift ...):
(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

; There are some inputs that produce on output. For example:
(shuffle '(a (b c)))
; (a (b c)), since the input is not an atom and its first element is not a pair, so we build a pair
; with 'a and shuffle called on the second element, '(b c). Since again, '(b c) is not an atom and its
; first element is not a pair, we build a pair with 'b and shuffle called on 'c. (shuffle 'c) is just
; 'c, so we have '(b c) for this last build, which is then used to build and return '(a (b c)).

(suffle '(a b))
; (a b), since the input is not an atom and its first element is not a pair, so we build a pair with
; 'a and (suffle 'b). As above, (shuffle 'b) is just 'b, so we build and return the pair '(a b).

(suffle '((a b) '(c d)))
; produces an infinite loop. While the input is not an atom, its first element is a pair, so we reverse
; the pora to '((c d) (a b)) and compute (shuffle '((c d) (a b))). Once again, the input is not an
; atom, but its first element is a pair, so we reverse the pora back to '((a b) (c d)) and compute
; (shuffle '((a b) (c d)))...

; We conclude that (suffle ...) is not a "total" function.

; Here is another function:
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else (cond
            ((even? n) (C (o/ n 2)))
            (else (C (add1 (o* n 3)))))))))

; It is unknown whether or not this function is "total" -- the Collatz conjecture says that the
; function eventually terminates and returns 1 for all positive integer inputs.
;
; We certainly won't be proving the conjecture is true or false today, so let's move on!

; Here is another function:
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))

; Let's test it out:

(A 1 0)
; = (A 0 1) = 2

(A 1 1)
; = (A 0 (A 1 0)) = (A 0 (A 0 1)) = (A 0 2) = 3

(A 2 2)
; = (A 1 (A 2 1)) = (A 1 (A 1 (A 2 0))) = (A 1 (A 1 (A 1 1))) = (A 1 (A 1 (A 0 (A 1 0))))
; = (A 1 (A 1 (A 0 2))) = (A 1 (A 1 3)) = (A 1 (A 0 (A 1 2))) = (A 1 (A 0 (A 0 (A 1 1))))
; = (A 1 (A 0 (A 0 3))) = (A 1 (A 0 4)) = (A 1 5) = (A 0 (A 1 4)) = (A 0 (A 0 (A 1 3)))
; = (A 0 (A 0 (A 0 (A 1 2)))) = (A 0 (A 0 (A 0 (A 0 (A 1 1))))) = (A 0 (A 0 (A 0 (A 0 3))))
; = (A 0 (A 0 (A 0 4))) = (A 0 (A 0 5)) = (A 0 6) = 7

; The books states that A is a "total" function, but remarks that the computation of (A 4 3) will
; take longer than the decay of the book I'm reading! Having seen how much work it took to compute
; (A 2 2), assuming I didn't make a mistake, I am only midly surprised.

; The book now wonders if it is possible to write a function
(define will-stop?
  (lambda (f)
    ...))
; that returns true iff the fuction f terminates for all valid inputs. This sounds like a tall order,
; so to simplify, let's just focus on deciding whether or not f terminates if given the empty set.
;
; Such a function is "total" in that it returns true if f terminates when given the empty set, and
; returns false if f does not terminate when given the empty set.
;
; For example, since (length '()) is zero, (length ...) is a function that terminates when given the
; empty set. Therefore:
(will-stop? length)
; #t
;
; On the other hand, (eternity '()) does not terminate, so:
(will-stop? eternity)
; #f
;
; Now consider the following function:
(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))
;
; Let's consider the value of:
(last-try '())
; which requires us to compute first:
(will-stop? last-try)
;
; Now if (will-stop? last-try) is false, then the (and ...) terminates with #f, which means
; (last-try '()) returns false and terminates. However, our assumption that (will-stop? last-try) is
; false means that (last-try '()) does NOT terminate. We have a contradiction.
;
; So if must be that (will-stop? last-try) is true. But then the (and ...) continues to the second
; clause (eternity '()). Since this never terminates, the (and ...) never returns a value to
; (last-try '(), hence (last-try '()) never terminates. However, our assumption that
; (will-stop last-try) is true means that (last-try '()) will terminate. We have another contradiction.
;
; We therefore have a "total" function (will-stop? ...) that, if it can be defined with (define ...),
; must return either true or false if it receives the argument last-try. However, we have seen that
; this does not happen, so we are forced to conclude that the problem is with using (define ...) to
; define (will-stop? ...).
;
; In other words, (will-stop? ...) is a "total" function that we can describe precisesly, but that
; cannot be defined in Scheme with (define ...). The existence of such functions is guaranteed by a
; result of Turing and Godel.
;
; We now take a closer look at (define ...) and recursive functions...

; Recall the definition of (length ...), which I called (my-length ...) since (length ...) is already
; implemented [henceforth referred to as simply "length"]:
(define my-length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (my-length (cdr lat)))))))
;
; Note that without (define ...) we could not refer to the name "my-length" in the else clause. Of
; course, no other function could refer to and use (my-length ...) either, so it would seem that
; (define ...) is necessary. However, consider this function:
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))
; This "anonymous" function correctly determines the length of the empty list, and that's it. This
; seems a bit like cheating to me, since we used (define ...) to define (eternity ...) and (add1 ...).
; I'm not sure how primitives like (lambda ...), (cond ...), (null? ...), (else ...), (cdr ...) are
; defined either, for that matter...
;
; If we could name this function using (define ...), length-0 would be a great name since it works like
; (length ...), but only if the input is the empty list.
;
; With this function, we could create an anonymous function that determines the length of lists that
; contain 0 or 1 elements:
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (length-0 (cdr l))))))
;
; If we don't have (define ...) avalaible, then we cannot refer to the name length-0. However, we can
; cut-and-paste its definition into the place where we now have length-0:
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (l)
                  (cond
                   ((null? l) 0)
                   (else (add1 (eternity (cdr l)))))) (cdr l))))))
;
; Once again, if we could name this function using (define ...), length-01 would be a great name since
; it works like length but only if the input has length 0 or 1.
;
; And here is the function that correctly computes the lengths of lists that contain 0, 1 or 2
; elements
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (l)
                  (cond
                   ((null? l) 0)
                   (else (add1 ((lambda (l)
                                  (cond
                                   ((null? l) 0)
                                   (else (add1 (eternity (cdr l)))))) (cdr l)))))) (cdr l))))))
; which we would probably name length-012, if we could use (define ...). Etc.
;
; If we could write an infinite function [a function with an infitnite number of lines of code] similar
; to length-0, length-01, length-012, ..., then we would have length-infinity which would work exactly
; like (length ...).
;
; Of course, we cannot write such a function! This seems to be a conceptual dead-end...

; On the other hand, we do have a lot of cut-and-paste code so let's try to get rid of it. Let's
; start by rewriting what I've called length-0, which takes a list and either returns 0 [if the list is
; empty] or gets stuck in an infinite loop.
;
; This rewrite will be of the form ((lambda (f) ...) eternity). The idea is that this anonymous
; function, which I will call mk-length-0 ["mk" stands for "make"], takes a function [in this case
; eternity] and returns a function that behaves exactly like length-0:
((lambda (else-function)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (else-function (cdr l)))))))
 eternity)
;
; Once again, we could cut-and-paste code. This time, however, we take a copy of this code and paste
; it into the spot held by eternity. So, the inner function, mk-length-0, gets eternity and creates
; length-0, which is then passed to a copy of mk-length-0. This creates a function that behaves just
; like length-01:
((lambda (else-function1)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (else-function1 (cdr l)))))))
 ((lambda (else-function2)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (else-function2 (cdr l)))))))
 eternity))
;
; One more time, this time creating what I might call mk-length-012, a function thta takes a function
; [eternity] and returns a function that behaves exactlyy like length-012:
((lambda (else-function1)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (else-function1 (cdr l)))))))
 ((lambda (else-function2)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (else-function2 (cdr l)))))))
  ((lambda (else-function3)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (else-function3 (cdr l)))))))
   eternity)))
;
; We have used distinct variable argument names for each layer of the composition, but this is not
; necessary due to Scheme's lexical scoping. In other words, the following is equivalent to
; mk-length-012:
((lambda (else-function)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (else-function (cdr l)))))))
 ((lambda (else-function)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (else-function (cdr l)))))))
  ((lambda (else-funtion)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (else-function (cdr l)))))))
   eternity)))
;
; Note that I am using the variable name "else-function" because that's what it is, while the book uses
; "length". Of course, it doesn't matter, but my choice reminds me of what this argument does -- it is
; the function that is applied to (cdr l) if the else clause is evaluated.
;
; Here is the book's implementation, which is equivalent to what I call mk-length-012:
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))) eternity)))
;
; We still have a lot of repetition, so the book suggests that we "name the function that takes length
; as an argument and that returns a function that looks like length."
;
; In other words, instead of cutting-and-pasting the code for mk-length-0 into the spot held by
; eternity in the previous copy of mk-length-0, we could create a new function that takes a function
; and composes it with eternity:
(lambda (mk-length)
  (mk-length eternity))
; If given the function mk-length-0, it would produce the function length-0. We don't have names, so we
; cut-and-paste mk-length-0 and give it to this new function:
((lambda (mk-length)
   (mk-length eternity)) (lambda (else-function)
                           (lambda (l)
                             (cond
                              ((null? l) 0)
                              (else (add1 (else-function (cdr l))))))))
;
; Why is this any better? Well, it's not really, though it takes fewer lines to create the layers of
; composition required to create length-01, length-012, etc. For example, here is a function equivalent
; to length-01:
((lambda (mk-length)
   (mk-length (mk-length eternity))) (lambda (else-function)
                                       (lambda (l)
                                         (cond
                                          ((null? l) 0)
                                          (else (add1 (else-function (cdr l))))))))
;
; and length-012:
((lambda (mk-length)
   (mk-length (mk-length (mk-length eternity)))) (lambda (else-function)
                                                   (lambda (l)
                                                     (cond
                                                      ((null? l) 0)
                                                      (else (add1 (else-function (cdr l))))))))
;
; and even length-0123:
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mkr-length eternity))))) (lambda (else-function)
                                  (lambda (l)
                                    (cond
                                     ((null? l) 0)
                                     (else (add1 (else-function (cdr l))))))))

;
; Again, this is ultimately no better. Despite having fewer lines of code, we still need
; infinitely-many lines of code in order to replicated the function (length ...).
;
; If only we could create another application of mk-length right before eternity is called!
;
; Here is an important point: We are using (eternity ...) even though we hope it never gets called.
; We could put anything there, though we don't want to put something that gives incorrect values if
; the list is longer than the number of applicationsof mk-length.
;
; So why not use mk-length instead of eternity?
;
; Here is this idea applied to length-0:
((lambda (mk-length)
   (mk-length mk-length)) (lambda (else-function)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 (else-function (cdr l))))))))
;
; Again, this code will work like length-0 if given the empty list. The problem is that if the list
; is not empty, the else clause tries to apply a function [equivalent to mk-length-0] to (cdr l), but
; it is expecting a function [not a list].
;
; At this point, the book points out that, due to Scheme's lexical scoping, we can replace the variable
; "length" with "mk-length", which doesn't fix the problem with the else-clause...
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 (mk-length (cdr l))))))))
;
;... however, it does suggest that we could fix this issue by replacing the single mk-length in the
; else clause with (mk-length eternity):
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 ((mk-length eternity) (cdr l))))))))
;
; This code is now equivalent to length-01: If the given list is empty, then the (null? ...) line
; returns zero, as expected.
;
; Otherwise the else-clause creates the function (mk-length eternity), which is equivalent to what I
; would call (mk-length-0 (eternity eternity)), which is behaves like length-0 when given (cdr l). If
; (cdr l) is empty, then (eternity eternity) is never evaluated, and zero is returned to the previous
; layer, which adds 1 to get a final result of 1.
;
; Finally, if the given list has 2 or more elements, (eternity eternity) will be given (cdr (cdr l)),
; and we will have an infinite loop.
;
; As before, we could continue this by just adding more calls to mk-length in the "starter" function
; with a final call to eternity embedded in what I call mk-length-0. However, we then have the same
; problem as before -- we need infinitly-many lines of code.
;
; In order to avoid the situation where we run out of applications of mk-length and have to call
; eternity, let's replace eternity with mk-length!
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 ((mk-length mk-length) (cdr l))))))))
; This is now equivalent to (length ...). The part of this that used to be what I call mk-length-0 now
; has an else clause that calls itself with itself [mk-length-0 with mk-length-0], so that there is
; always one more call to length-0 if needed.
;
; Why do we need the "starter" function (lambda (mk-length) (mk-length mk-length))? Well, it is given
; an anonymous function that cannot call itself by name [since we aren't using (define ...) to name
; things], but the starter function gives it a name [mk-length] which it then passes to another copy,
; which starts the recursion. The recursion continues from that point since due to the fact that the
; else clause has two copies: one for the current layer and one to pass to itself for the next layer.

; The book now seeks to transform the code because "it no longer contains the function that looks like
; length". In other words, the book would like to try to rewrite this function in the form:
(...
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))
; where "..." is a function to be determined. Note that the argument of "..." is what I call
; mk-length-0, since if given eternity we have length-0.

; The book's first attempt is to replace (mk-length mk-length) in the else-clause with "length", wrap
; everything [except the "starter" and the outermost (lambda (mk-length) ...)] with a
; (lambda (length) ...), and give it the argument (mk-length mk-length):
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            ((lambda (length)
                               (lambda (l)
                                 (cond
                                  ((null? l) 0)
                                  (else (add1 (length (cdr l)))))))
                             (mk-length mk-length))))
;
; The problem with this function is that in order to apply it to a list, say '(apples), we would first
; need to evalute the starter function with its given input. In other words, we need to evaluate:
((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (mk-length mk-length)))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))
; This expression has two parts. Though they are identical, the first we can think of as the function
; and the second as the function's argument. So let's pass this argument to the function, recognizing
; that it's last line (mk-length mk-length) will evaluate to this same expression:
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
     (mk-length mk-length)))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
     (mk-length mk-length)))))
; But now we have to do this again for the composition that is the argument of the outermost
; (lambda (length) ...):
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
      (mk-length mk-length))))))
; Etc.
; Clearly this will never end. So we return to the last correct version:
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 ((mk-length mk-length) (cdr l))))))))

; Here is a new idea. Note that if f is a function of a single argument, then so is:
(lambda (x)
  (f x))
; This is trivial. This means we can replace (mk-length mk-length) in the else-clause with:
(lambda (x)
  ((mk-length mk-length) x))
; which results in:
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            (lambda (l)
                              (cond
                               ((null? l) 0)
                               (else (add1 ((lambda (x)
                                              ((mk-length mk-length) x)) (cdr l))))))))
;
; We can now wrap the (lambda (l) ...) part with (lambda (length) ...), as we did before, and give it
; the argument (lambda (x) ((mk-length mk-length) x)):
((lambda (mk-length)
   (mk-length mk-length)) (lambda (mk-length)
                            ((lambda (length)
                               (lambda (l)
                                 (cond
                                  ((null? l) 0)
                                  (else (add1 (length (cdr l)))))))
                             (lambda (x)
                               ((mk-length mk-length) x)))))
; Next, not that the part wrapped in (lambda (length) ....) does not depend on mk-length at all. This
; means that the part that looks just like (length ...) can be extracted and passed to what's left as
; an argument ["le" in what follows]:
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; We've done it! We now have a function that acts just like (length ...), does not (define ...)
; anything, and uses a function that looks like what a length function should do.
;
; There is only one thing left to do -- clean up all this crazy notation. The part that calls the
; length-like function is called the "applicative-order Y combinator":
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))
; In more generic notation, and using (define ...) to name it, we have:
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
;
; Finally, with this definition of the applicative-order Y-combinator, we can re-write our length
; function one last time:
(Y (lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))


