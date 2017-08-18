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
     ((eq? (quote ^) (operator aexp)) (o^ (value (1st-sub-exp aexp))
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

;;;;;;;;;;;;

; We have not yet written (set? lat) but we can figure out how it works by looking at some examples:

(set? '(apple peaches apple plum))
; #f, since 'apple appears more than once

(set? '(apples peaches pears plums))
; #t

(set? '())
; #t, since no atom appears more than once

; Here is my implementation:
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else (and (not (member? (car lat) (cdr lat)))
                (set? (cdr lat)))))))

; The book's implementation is equivalent:
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

; Here is another example:
(set? '(apple 3 pear 4 9 apple 3 4))
; #f
;
; We have not yet written (makeset lat) but we can figure out how it works by looking at an example:

(makeset '(apple peach pear peach plum apple lemon peach))
; (apple peach pear plum lemon)

; Here is my implementation:
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat)
                 (makeset (cdr lat)))))))

; My version works, but the example above produces:
(makeset '(apple peach pear peach plum apple lemon peach))
; (pear plum apple lemon peach)
;
; In other words, my version removes any atom that occurs later in the list. The books'
; implementation is exactly the same. The example above is repeated in the book, and the second, not
; the first output is produced.
;
; We now implement (makeset lat) using (multirember ....):
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat)) (cons (car lat)
                                          (makeset (multirember (car lat) (cdr lat)))))
     (else (cons (car lat)
                 (makeset (cdr lat)))))))

; This version removes duplicates in the reverse direction, meaning that if we find that a given
; atom is in the cdr of the list, we remove all subsequent occurances.
;
; For example:
(makeset '(apple peach pear peach plum apple lemon peach))
; (apple peach pear plum lemon)

; The book's version is simpler:
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat)
                 (makeset (multirember (car lat) (cdr lat))))))))

; Here is the weird example considered above:
(set? '(apple 3 pear 4 9 apple 3 4))
; (apple 3 pear 4 9)

; We have not yet written (subset? set1 set2) but we can figure out how it works by looking at some
; examples:

(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
; #t since each atom of set1 is an atom of set2

(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))
; #f since the atoms '4 and 'of of set1 are not contained in set2

; Here is my implementation of (subset? set1 set2):
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

; The book asks us to write an implementation that uses (and ...). Here is my version:
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

; We have not yet written (eqset? set1 set2) but we can figure out how it works by looking at one
; example:

(eqset? '(6 large chickens with wings) '(6 large chickens with wings))
;#t since set1 is a subset of set2 and set2 is a subset of set1

; Here is my implementation:
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

; We have not yet written (intersect? set1 set2) but we can figure out how it works by looking at one
; example:

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
; #t since at least one atom in set1 is contained in set2

; Here is my implementation:
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1) set2)))))

; The book asks us to write an implementation that uses (or ...). Here is my version:
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

; We have not yet written (intersect set1 set2) but we can figure out how it works by looking at one
; example:

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
; (and macaroni)

; Here is my implementation:
(define intersect
  (lambda (set1 set2)
    (cond
     ((or (null? set1)
          (subset? set1 set2)) set1)
     ((member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

; It is not really necessary to check (subset? set1 set2), since if this is true then the next
; condition will always be true. Basically, my implementation would make (subset? ...) do all this
; work of checking each element of set1. If it fails, then we will check each element again, so
; let's remove it.
;
; Here is the book's version, which is my version with this simplification:
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

; We have not yet written (union set1 set2) but we can figure out how it works by looking at one
; example:

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
; (stewed tomatoes casserole macaroni and cheese)

; Here is my implementation:
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

; I figured set difference would be next, so I implemented it before reading the book's version. This
; procedure should return the elements in set1 that are not in set2:
(define set-diff
  (lambda (set1 set2)
    (cond
     ((null? set2) set1)
     ((member? (car set2) set1) (set-diff (rember (car set2) set1) (cdr set2)))
     (else (set-diff set1 (cdr set2))))))

; My implementation works and is equivalent to the book's, though they work in slightly different
; ways. Mine continues until set2 is empty, deleting the elements of set2 one-by-one, and deleting
; a given element from set1 if it is contained in both. So I used the procedure (rember ...).
;
; The book's implementation, which it calls (xxx ...), is as follows:
(define xxx
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2) (xxx (cdr set1) set2))
     (else (cons (car set1)
                 (xxx (cdr set1) set2))))))

; The book's implementation is better than mine since it uses (member? ...) only once for a given
; element of set1, whereas mine uses it once, then again [effectively] when it calls (rember ...).

; We have not yet written (intersectall l-set) but we can figure out how it works by looking at some
; examples:

(intersectall '((a b c) (c a d e) (e f g h a b)))
; (a)

(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))
; (6 and)

; Here is my implementation, which assumes that the l-set is non-empty:
(define intersectall
  (lambda (l-set)
    (cond
     ; if (cdr l-set) is null, then we can just return the car, which is the only set in the l-set
     ((null? (cdr l-set)) (car l-set))
     ; otherwise intersect the set (car l-set) with the result [set] of intersectall applied
     ; applied to l-set (cdr l-set)
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

; We have not yet written (a-pair ...) but we can figure out how it works by looking at some
; examples:

(a-pair? '(pear pear))
; #t since it is a list with exactly two atoms

(a-pair? '(3 7))
; #t since it is a list with exactly two numbers [atoms]

(a-pair? '((2) (pair)))
; #t since it is a list with exactly two S-expressions

(a-pair? '(full (house)))
; #t since it is a list with exactly two S-expressions

; Here is my implementation:
(define a-pair?
  (lambda (l)
    (cond
     ; if l is an atom, then return false
     ((atom? l) #f)
     ; if l is the empty list, then return false
     ((null? l) #f)
     ; if l is a list with only one element, then return false
     ((null? (cdr l)) #f)
     ; if l is a list with at least two elements, then return true iff there are exactly two elements
     ((null? (cdr (cdr l))) #t)
     (else #f))))

; We now implement three helper functions related to makine a list of a pair of S-expressions. We skip
; the book's first implementation since it needlessly uses (cond ...) when there is only one thing to
; do:
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

; The book challenges us to write (third ...) as a one-liner:
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; Relations [rel] are sets of pairs. For example:

'(apples peaches pumpkin pie)
; is not a relation [it's a set, but not of pairs]

'((apples peaches) (pumpkin pie) (apples peaches))
; is not a relation [it's a list, not a set, or pairs]

'((apples peaches) (pumpkin pie))
; is a relation

'((4 3) (4 2) (7 6) (6 2) (3 4))
; is a relation

; Functions are relations in which the list composed of the first element of each pair is a set,
; that is, no first element repeats. Put another way, (firsts ...) is a set. For example:


'((4 3) (4 2) (7 6) (6 2) (3 4))
; is a relation, but not a function [4 occurs as a first element of a pair twice]

; We have not yet written (fun? rel) but we can figure out how it works by looking at some examples:

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
; #t since (firsts ...) is (8 4 7 6 3), which is a set

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
; #f since (firsts ...) is (d b b e g), which is not a set

; Here is my implementation, which uses (set? ...) and (firsts ...):
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

; We have not yet written (revrel rel) but we can figure out how it works by looking at an example:

(revrel '((8 a) (pumpkin pie) (got sick)))
; ((a 8) (pie pumpkin) (sick got))

; Clearly (revrel rel) reverses the order of each pair in rel.
;
; Here is my implementation, which uses (first ...), (second ...), and (build ...) defined above:
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (build (second (car rel)) (first (car rel)))
                 (revrel (cdr rel)))))))

; The following implementation of (revrel rel) does not use the helper functions, and is therefore
; much more difficult to read and understand:
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (cons (car (cdr (car rel)))
                       (cons (car (car rel))
                             (quote ())))
                 (revrel (cdr rel)))))))

; Note that we would simplify (revrel rel) further by writing a procedure (revpair p) that
; reverses the given pair p:
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

; We have not yet written (fullfun? fun) but we can figure out how it works by looking at some
; examples:

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
; #f

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
; #t

; My guess is that the first (fullfun? ...) is false because the element '2 is repeated in the list
; of second elements of the function, while the second (fullfun? ...) is true because this does not
; happen.

; Here are two more examples:

(fullfun? '((grape raisin) (plum prune) (stewed prune)))
; #f

(fullfun? '((grape raisin) (plum prune) (stewed grape)))
; #t

; Here is my implementation using (revrel ...), (firsts ...) and (set? ...):
(define fullfun?
  (lambda (fun)
    (set? (firsts (revrel fun)))))

; Of course, we can simplify (fullfun? ...) by writing the helper function (seconds ...):
(define seconds
  (lambda (rel)
    (firsts (revrel rel))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

; If both a relation, rel, and its reverse are functions, then we say rel is a one-to-one function.
; So we really ought to rename it:
(define one-to-one?
  (lambda (fun)
    (set? (seconds fun))))

; We can rewrite (one-to-one ...) using (fun? ...) and (revrel ...):
(define one-to-one
  (lambda (fun)
    (fun? (revrel fun))))

; A final example:

(one-to-one? '((chocolate chip) (doughy cookie)))
; #t
