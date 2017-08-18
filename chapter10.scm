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

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat) ; "sorn" stands for "symbol or number"
    (cond
     ((number? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (o* (weight* (first pora)) 2)
               (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else (cond
            ((even? n) (C (o/ n 2)))
            (else (C (add1 (o* n 3)))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))

(define Y ; applicative-order Y combinator
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(Y (lambda (length) ; equivalent to length
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

;;;;;;;;;;;;

; The book defines an "entry" as a pair of lists of equal length whose first list is a set. Here are
; some examples:
'((appetizer entree beverage)
  (pate boeuf vin))

'((appetizer entree beverage)
  (beer beer beer))

'((beverage dessert)
  ((food is) (number one with us)))

; We can build an entry with the following function:
(define new-entry
  build)
; which calls (build ...) on the given arguments.
;
; Here are the three examples above constructed with (new-entry ...):

(new-entry
 '(appetizer entree beverage)
 '(pate boeuf vin))

(new-entry
 '(appetizer entree beverage)
 '(beer beer beer))

(new-entry
 '(beverage dessert)
 '((food is) (number one with us)))

; We have not yet written the function (lookup-in-entry name entry), but we can figure out how it works
; by looking at some examples:
(lookup-in-entry 'entree '((appetizer entree beverage)
                           (food tastes good)))
; 'tastes

(lookup-in-entry 'dessert '((appetizer entree beverage)
                           (food tastes good)))
; unknown, we should leave this up to the user of (lookup-in-entry ...), which can be accomplished by
; adding an addtional argument to (lookup-in-entry ...) that is invoked when 'name is not found in the
; first list of 'entry. The books suggest that this additional argument should be a function that takes
; the argument 'name.
;
; Here is the book's suggestion for doing this:
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
;
; Of course, now we must write (lookup-in-entry-help ...)
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? name (car names)) (car values))
     (else (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)))))

; The book defines a "table" (aka "environment") as a list of entries. For example:
'(((appetizer entre beverage) (pate boeuf vin))
  ((beverage dessert) ((food is) (number one with us))))

; A simple way of inserting an entry into a table is with (cons ...). Here is a named function that
; does exactly that:
(define extend-table
  cons)

; We have not yet written (lookup-in-table ...) but we can figure out what it does by looking at an
; example:

(lookup-in-table 'entree
                 '(((entree dessert) (spaghetti spumoni))
                   ((appetizer entree beverage) (food tases good)))
                 (lambda (name) ...))
; 'spaghetti, since this is the value of the first occurrance of 'entree

; Here is the book's implementation of (lookup-in-table ...):
(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (name)
                              (lookup-in-table name
                                               (cdr table)
                                               table-f)))))))
;
; This is a bit odd, but if we do not find 'name in (car table), then the function given to
; (lookup-in-entry ...) is executed. Since we still need to examine (cdr table), this function should
; be another (lookup-in-entry ...) with (cdr table) and the original table-f function. We need to use
; (lambda ...) with argument 'name, since if this function is executed, it will be given a single
; arugment, 'name.

; At this point the book reminds us that sans serif indicates an atom, however in this file I am using
; the quotation mark, as we would in a Scheme program.

; Recall the previously implemented function (value ...) which returns the natural value of the given
; expression:
(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     (else ((atom-to-function (operator aexp))
            (value (1st-sub-exp aexp))
            (value (2nd-sub-exp aexp)))))))

; The book asks us for the value of (car (quote (a b c))). If by "value" it means (value ...), then:
(value (car (quote (a b c))))
; 'a since (car '(a b c)) is the atom 'a.
; It should be noted that the book answers this by saying that we don't even know what (quote (a b c))
; is, but I think we do: it is a list of the atoms 'a, 'b, and 'c.

; The book next asks us what is the value of:
(cons 'a (cons 'b (cons 'c (quote ()))))
; Again, if by "value" the book means (value ...), then we would have an error since (value ...) is
; given the list '(a b c), but is expecting a list of the form (operator arg1 arg2). Otherwise, its
; value is just the list '(a b c).

; The book nexts asks us for the value of:
(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a (cons 'b (cons 'c (quote ()))))
                   (quote ())))
            (quote ())))
; OK, like the previous example, we do not have a valid input for (value ...), so this is just the list
; '(car (quote (a b c))). Note that since I am using Gambit-Scheme, the output is: (car '(a b c))

; Next up:
(value (car (quote (a b c))))
; a

(value (quote (car (quote (a b c)))))
; error, though I expected the argument of (value ...) to be the atom (car (quote (a b c))), so that
; value would return (car (quote (a b c))).

(value (add1 6))
; 7

(value 6)
; 6

(value (quote nothing))
; 'nothing

(value nothing)
; error since Scheme thinks "nothing" is an unbound variable

(value ((lambda (nothing)
          (cons nothing (quote ())))
        (quote (from nothing comes something))))
; error since the argument to (value ...) is ((from nothing comes something))

(value ((lambda (nothing)
          (cond
           (nothing (quote something))
           (else (quote nothing))))
        #t))
; 'something since (lambda (nothing) ...) is given the value #t, so the first clause of (cond ...) is
; executed, which returns (quote something), which is an atom, so (value ...) returns this atom's
; value, which is 'something

; The book then asks us for the "type" of 6, which it says is "*const", which presumably stands for
; constant.

; Similarly, the "type" of #f is *const.

; Next up:
(value #f)
; #f

; Next the book asks us for the "type" of 'cons, which apparently is *conts.

; Next the book asks us for
(value 'car)
; 'car. Or maybe it wanted:
(value car)
; #<procedure #3 car>. In either case, the book tells us that the answer is: (primitive car).

; Next the book asks for the "type" of
(quote nothing)
; which apparenty is *quote.

; Then the "type" of nothing, which apparently is *identifier.

; Then the "type" of
(lambda (x y)
  (cons x y))
; which apparently is *lambda

; Then the "type" of
((lambda (nothing)
   (cond
    (nothing (quote something))
    (else (quote nothing))))
 #t)
; which apparently is *application

; Then the "type" of
(cond
 (nothing (quote something))
 (else (quote nothing)))
; which apparently is *cond

; According to the book, there are [at least] six "types": *const, *quote, *identifier, *lambda, *cond,
; and *application. The book suggests that these should be represented by function called "actions".
;
; If actions are functions that do "the right thing" when applied to the appropriate type of
; expression,  it would have to find out the type of expression it was given and then use the
; associated action.

; Ok, next up is a function that produces the correct action (or function) for each possible
; S-expression:
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

; Next we need to write (atom-to-action e) which assumes that e is not an ill-formed S-expression:
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

; Next we need to write (list-to-action e):
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e)) (cond
                       ((eq? (car e) (quote quote)) *quote)
                       ((eq? (car e) (quote lambda)) *lambda)
                       ((eq? (car e) (quote cons)) *cond)
                       (else *application)))
     (else *application))))

; Now that we have (expression-to-action e), we can use it to define (value ...) and (meaning ...)
(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; At this point, the book mentions that (value e) is called an "interpreter", and that is approximates
; the function (eval ...) available in Scheme.
;
; Note that "actions" need two arguments: an expression [e] and a table [table].

; It remains to implement the six types. Here is *const:
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))
; Clearly *const returns the value of a number, #t, or #f. All other atoms of *const type represent
; primitives.

; Next is *quote:
(define *quote
  (lambda (e table)
    (text-of e)))
; with helper function:
(define text-of
  second)

; We haven't yet used the table, but it will be needed to remember the values of identifiers.
;
; With that said, next is *identifier:
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
; with helper function:
(define initial-table
  (lambda (name)
    (car (quote ()))))
; The book notes that we hope we never have to use initial-table. If we did need it, it would produce
; an error since you cannot take the car of the empty set.

; Next is *lambda:
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table
                 (cdr e)))))
; For example:
(meaning (lambda (x) (cons x y))
         (((y z) ((8) 9))))
; would first evaluate
(expression-to-action (lambda (x) (cons x y)))
; which, since the argument is not an atom, would then evaluate
(list-to-action (lambda (x) (cons x y)))
; Since the car of the argument is (quote lambda), (list-to-action ...) returns *lambda.
;
; Next, returning to the body of (meaning ...), we would evaluate:
(*lambda (lambda (x) (cons x y))
         (((y z) ((8) 9))))
; which returns:
('non-primitive
 (
  (((y z) ((8) 9))) ; the table
  (x) ; formal arguments
  (cons x y) ; body
  ))

; The book notes that it will be handy to have some helper functions to retrieve the three parts (i.e.,
; the table, the formal arguments, and the body) to make this program easier to read.
(define table-of
  first)

(define formals-of
  second)

(define body-of
  third)

; Next is *cond, but first we need to recall what (cond ...) does. It takes any number of special forms
; and considers each line in turn. If the question on the left is false, it moves to the next line.
; Otherwise , it proceeds to "answer" [evaluate?] the right part. If it comes to an else-line, it
; treats this line as if the left [question] part is true.
;
; Here is the implementation of this idea:
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))
; with helper functions:
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))
; and
(define question-of
  first)
; and
(define answer-of
  second)

; The book notes that we have violated the First Commandment since we don't ask if (null? lines), so
; one of the questions better be true.

; We can now write *cond:
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
; with helper function
(define cond-lines-of
  cdr)

; In order to better understand *cond, let's look at an example:
(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t))
        ((klatsch party) (5 (6)))))
;
; Going straight to (evcon ...), first we need to evaluate:
(cond-lines-of (cond (coffee klatsch) (else party)))
; which just takes the cdr of its argument, so:
((coffee klatsch) (else party))
; Next we evaluate:
(evcon ((coffee klatsch) (else party)) ; lines
       (((coffee) (#t)) ; table
        ((klatsch party) (5 (6)))))
; which begins a (cond ...) statement, whose first question is
(else? (question-of (car ((coffee klatsch) (else party)))))
; which requires:
(question-of (coffee klatsch))
; which is 'coffee, so we evaluate:
(else? 'coffee)
; Since 'coffee is an atom that is not (eq? ...) to 'else, #f is returned.
;
; We must now move to the second (cond ...) question in (evcon ...), which begins with the question:
(meaning 'coffee
         (((coffee) (#t))
          ((klatsch party) (5 (6)))))
; but this requires us to first evaluate:
(expression-to-action 'coffee)
; which is given an atom, so we evaluate:
(atom-to-action 'coffee)
; which returns *identifier. Therefore, in order to evaluate (meaning ...), we must evaluate:
(*identifier 'coffee
             (((coffee) (#t))
              ((klatsch party) (5 (6)))))
;
; Now (*identifer ...) must be evaluated by computing:
(lookup-in-table 'coffee ; name
                 '(((coffee) (#t)) ; table
                   ((klatsch party) (5 (6))))
                 initial-table) ; function we hope is never evaluated
; Since the given table is not null, we must compute:
(lookup-in-entry 'coffee
                 ((coffee) (#t))
                 (lambda ...))
;
; We are almost there. (lookup-in-entry ...) immediately calls (lookup-in-entry-help ...):
(lookup-in-entry-help 'coffee ; name
                      '(coffee) ; names
                      '(#t) ; values
                      '(lambda ....)) ; what to do if 'name isn't found in 'names.
; Since 'names is not null, we go to the second question:
(eq? (car '(coffee)) 'coffee)
; which is true! So we return the value #t.
;
; At this point we know that the (meaning ...) question asked in the second line of (cond ...) is #t.
; We therefore must evaluate its answer:
(meaning (answer-of (coffee klatsch))
         (((coffee) (#t))
          ((klatsch party) (5 (6)))))
; which is just:
(meaning 'klatsch
         (((coffee) (#t))
          ((klatsch party) (5 (6)))))
;
; As before, (meaning ...) requires us to compute
(expression-to-action 'klatsch)
; hence
(atom-to-action 'klatsch)
; which returns *identifier. Therefore, in order to evaluate this new call ot (meaning ...), we must
; evaluate:
(*identifier 'klatsch
             (((coffee) (#t))
              ((klatsch party) (5 (6)))))
; Eventually 'klatsch is found and 5 [the value of 'klatsch] is returned. Since this is the answer to
; the second question in the (cond ...) of (evcon ...), the value 'party is returned as the final
; result. Edit: I'm coming back to this after a long time away, so I'm a bit rusty, but I tried this
; and I think I meant 5 is returned, not 'party. At least, that's what was returned when I ran it.

; We now write the function (evlis ...) that takes a list of representations of arguments and a table,
; and returns a list composed of the meaning of each argument.
(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

; Next we write *application:
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
; and its helper functions (function-of ...):
(define function-of
  car)
; and (arguments-of ...)
(define argurments-of
  cdr)

; The states that there are tow kinds of functions: primitives and non-primitives. Primitives are
; represented as:
(primitive primitive-name)
; and non-primitives are represented as:
(non-primitive (table formals body))
; Note that the list (table formals body) is called a closure record.

; We now write (primitive? ...):
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
; and (non-primitive? ...):
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

; We can now write the function (apply ...), which I will call (o-apply ...) since (apply ...) already
; exists:
(define o-apply
  (lambda (fun vals)
    (cond
     ((primitive? fun) (apply-primitive (second fun) vals))
     ((non-primitive? fun) (apply-closure (second fun) vals)))))
; which requires two helper functions: (apply-primitive ...) and (apply-closure ...).

; Here is (apply-primitive ...)
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons)) (cons (first vals) (second vals)))
     ((eq? name (quote car)) (car (first vals)))
     ((eq? name (quote cdr)) (cdr (first vals)))
     ((eq? name (quote null?)) (null? (first vals)))
     ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
     ((eq? name (quote atom?)) (:atom? (first vals)))
     ((eq? name (quote zero?)) (zero? (first vals)))
     ((eq? name (quote add1)) (add1 (first vals)))
     ((eq? name (quote sub1)) (sub1 (first vals)))
     ((eq? name (quote number?)) (number? (first vals))))))
; with helper function (:atom? ...):
(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

; Next the book asks us how we could find the result of (f a b), where f is (lambda (x y) (cons x y)),
; a is 1, and b is (2). This should just be (1 (2)). Our interpreter can find the meaning of (cons x y)
; where table is (((x y) (1 (2)))).
;
; More generally, applying a non-primitive function -- a closure -- to a list of values is the same as
; finding the meaning of the closure's body with its table extended by an entry of the form:
; (formals values), where formals is the formals of the closure and values is the result of evlis.
;
; So, here is (apply-closure ...):
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) (extend-table (new-entry (formals-of closure) val)
                                             (table-of closure)))))

; Let's work through the following example:
(apply-closure ((((u v w) ; closure
                  (1 2 3))
                 ((x y z)
                  (4 5 6)))
                (x y)
                (cons z x))
               ((a b c) (d e f))) ; vals
; We start with (meaning e table), where e is (body-of ...), or (third ...) of closure, which is just
; (cons z x), and table is the body of the closure extended by (formals-of ...), or (second ...) of
; closure, which is (x y) cons'ed with vals, which is ((a b c) (d e f)), then cons'ed with
; (table-of ...), or (first ...) of closure, which is (((u v w) (1 2 3)) ((x y z) (4 5 6))).
;
; Now the meaning of (cons z x), where z is 6 and x is (a b c) should be (6 (a b c)), which can be
; determined from (meaing e table), where e is (cons z x) and table is (((x y) ....) from above.
;
; To find the meaning of all the arguments, consider (evlis args table) where args is (z x) and
; table is (((x y) ...). However, to do this, we need (meaning e table) where e is z and also
; (meaning e table) where e is x.
;
; Now (meaning e table), where e is z is 6, using *identifier. And (meaning e table), where e is x,
; is (a b c), also using *identifier. Hence evlis returns (6 (a b c)), a list of meanings, and the
; expected result.

; If e is cons, then (meaning e table) is (primitive cons), using *const. We are now ready to compute
; (apply fun vals), where fun is (primitive cons) and vals is (6 (a b c)). We use (apply-primitive ...)

; The appropriate cond-line to use is, of course, ((eq? name (quote cons) ...), for obvious reasons.
; This line has us compute the cons of (first vals), which is 6 with (second vals), which is (a b c).
; The result, finally, is (6 (a b c)).

; The book ends by asking whether or not we need (define ...). Apparently not, if we use the
; Y-combinator, but refers us to the next book: The Seasoned Schemer.
