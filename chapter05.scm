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

;;;;;;;;;;;;

; We have not yet implemented (rember* ...), but we can figure out what it does by looking at some
; examples.

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
; ((coffee) ((tea)) (and (hick)))

(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
; (((tomato)) ((bean)) (and ((flying))))

; It is now clear that (rember* a l) removes all occurances of a from list l, recursing into any
; sub-lists.
;
; Here is my implementation:
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((and (atom? (car l))
           (eqan? a (car l))) (rember* a (cdr l)))
     ((and (atom? (car l))
           (not (eqan? a (car l)))) (cons (car l)
                                          (rember* a (cdr l))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

; The book's implementation is equivalent, but uses a nested (cond ...):
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

(lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
; #f

(atom? (car '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
; #f since (car ...) is ((tomato sauce)), which is not an atom

; We have not yet implemented (insertR* ...) but we can figure out what it does by looking at an
; example.

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
                          (if (a) ((wood chuck))) could chuck wood))

; ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast)))
;  (if (a) ((wood chuck roast))) could chuck roast wood))

; Here is my implementation of (insertR* ...):
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

; (rember* ...) and (insertR* ...) are similar in that they ask three questions: (null? l),
; (atom? (car l)), and (else ...).
;
; Also, they both recurs on (car l) if it is found to be a list.
;
; (rember* ...) and (multirember ...) are different in that (multirember ...) does not recurs into
; (car l) while (rember* ...) does if it sees that (car l) is a list.
;
; Apparently *-functions will all ask the three questions mentiond above, and will recurs into
; (car l) if it is found to be a list. This way they will work on any list that is either empty,
; an atom cons-ed onto a list, or a list cons-ed onto a list.

; We have not yet written the procedure (occursomething ...) but we can figure out how it works by
; looking at an example:
(occursomething 'banana
                '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbert))
                  (banana)
                  (bread)
                  (banana brandy)))
; 5

; Since this procedure counts the number of times a given atom occurs in a list, a better name would
; be (occur* ...)
;
; Here is my implementation:
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

; We have not yet written the procedure (subst* ...) but we can figure out how it works by looking
; at an example:
(subst* 'orange 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbert))
          (banana)
          (bread)
          (banana brandy)))
; ((orange)
;  (split ((((orange ice)))
;          (cream (orange))
;          sherbert))
;  (orange)
;  (bread)
;  (orange brandy)))

; Clearly (subst* new old l) substitutes all occurances of 'old with 'new anywhere in the list 'l.
;
; Here is my implementation:
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

; We have not yet written the procedure (insertL* ...) but we can figure out how it works by looking
; at an example:
(insertL* 'pecker 'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))
; (insertL* 'pecker 'chuck
;           '((how much (wood))
;             could
;             ((a (wood) pecker chuck))
;             (((pecker chuck)))
;             (if (a) ((wood pecker chuck)))
;             could pecker chuck wood))

; Clearly (insertL* new old l) inserts 'new to the left of all ocurances of 'old anywhere in the
; list 'l.
;
; Here is my implementation:
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

; The book's implementation is equivalent, though it uses 'old instead of (car l) for the insertion.

; We have not yet written the procedure (member* ...) but we can figure out how it works by looking
; at an example:
(member* 'chips '((potato) (chips ((with) fish) (chips))))
; #t

; Clearly (member* a l) returns true if it finds 'a anywhere in the list 'l)
;
; Here is my (incorrect) implementation:
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (cond
                       ((eqan? a (car l)) #t)
                       (else #f)))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))
; My implementation is incorrect because it returns false if (car l) is an atom that is not equal
; to 'a. Instead, (member* ...) should be called on (cdr l). Brainfart!

; Here is the book's implementation:
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eqan? a (car l))
                          (member* a (cdr l))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

; We have not yet written the procedure (leftmost ...) but we can figure out how it works by looking
; at a few examples:
(leftmost '((potato) (chips ((with) fish) (chips))))
; potato

(leftmost '(((hot) (tuna (and))) cheese))
; hot

(leftmost '(((() four)) 17 (seventeen)))
; no answer [error]

(leftmost (quote ()))
; no answer [error]

; Clearly (leftmost l) returns the leftmost atom in a non-empty list of S-expressions that does
; not contain the empty list.
;
; Technically (leftmost ...) is not a *-function because it will only recurs on (car ...)
;
; Here is my implementation:
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

; Recall that (or ...) asks questions one at a time until it finds one that is true, at which point
; it stops evaluating its arguments and returns true. If no argument evaluates to true, then it
; returns false.
;
; Recall that (and ...) asks questions one at a time until it finds one that is false, at which point
; it stops evaluating its arguments and returns false. If no argument evaluates to false, then it
; returns true.
;
; Clearly it is possible that some arguments in (or ...) and (and ...) might not be evaluated.
;
; Note that it is possible to implement (and ...) and (or ...) using (cond ...). Here are the
; implementations:
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

; We have not yet written the procedure (eqlist? ...) but we can figure out how it works by looking
; at a few examples:
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
; #t

(eqlist? '(strawberry ice cream) '(strawberry cream ice))
; #f

(eqlist? '(banana ((split))) '((banana) (split)))
; #f

(eqlist? '(beef ((sausage)) (and (soda))) ' (beef ((salami)) (and (soda))))
; #f

(eqlist? '(beef ((sausage)) (and (soda))) ' (beef ((sausage)) (and (soda))))
; #t

; Clearly (eqlist ...) will return true if and only if the two given lists are identical.
;
; An implementation of (eqlist l1 l2) will ask: (null? (car l)), (atom? (car l)), and (else ...),
; where l is l1 or l2, and then (and ...) of the results. So there are nine combinations to consider.
;
; Here is my implementation:
(define eqlist?
  (lambda (l1 l2)
    (cond
     ; if both lists are null, return true
     ((and (null? l1)
           (null? l2)) #t)
     ; if both cars are null, recurs into cdrs
     ((and (null? (car l1))
           (null? (car l2))) (eqlist? (cdr l1) (cdr l2)))
     ; if both cars are atoms, compare atoms and recurs into cdrs
     ((and (atom? (car l1))
           (atom? (car l2))) (and (eq? (car l1) (car l2))
                                  (eqlist? (cdr l1) (cdr l2))))
     ; if both cars are not null and not atoms, then recurs into cars and cdrs
     ((and
       (and (not (null? l1))
            (not (atom? (car l1))))
       (and (not (null? l2))
            (not (atom? (car l2))))) (and (eqlist? (car l1) (car l2))
                                          (eqlist? (cdr l1) (cdr l2))))
     ; at this point the cars cannot be the same type, so return false
     (else #f))))

; I think my implementation is correct [works with the above examples], but it is different from the
; book's, which actually asks nine questions:
(define eqlist?
  (lambda (l1 l2)
    (cond
     ; the first three conditions require (null? l1) to be true...
     ; if both lists are null, return true
     ((and (null? l1)
           (null? l2)) #t)
     ; if l1 is null and (car of l2 is an atom), then return false
     ; the next line works because if l2 is null and we have gotten to the question
     ((and (null? l1)
           (atom? (car l2))) #f)
     ; at this point if l1 is null, then l2 cannot also be null [first case considered],
     ; so return false
     ((null? l1) #f)
     ; the next three conditions require (atom? (car l1)) to be true...
     ; at this point, we know l1 is not null, so if its car is an atom and l2 is null, return false
     ((and (atom? (car l1))
           (null? l2)) #f)
     ; if the cars of l1 and l2 are both atoms, then these atoms must be the same, as well as the
     ; cdrs of l1 and l2
     ((and (atom? (car l1))
           (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                  (eqlist? (cdr l1) (cdr l2))))
     ; if the car of l1 is an atom and we have gotten this far, then the car of l2 is NOT an atom
     ; since that case was the previous case
     ((atom? (car l1)) #f)
     ; the next three conditions will only be examined if (car l1) is neither null nor an atom...
     ; at this point we know l1 is not null, so if l2 is null return false
     ((null? l2) #f)
     ; if the car of l2 is an atom, then return false since we've already covered the case where the
     ; cars of l1 and l2 are both atoms [i.e., at this point the car of l1 will not be an atom]
     ((atom? (car l2)) #f)
     ; at this point the cars of l1 and l2 are neither null nor atoms, so recurs into the cars and
     ; cdrs of l1 and l2
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

; Note that in the first three conditions [of the book's implementation], after we have determined
; that it is NOT true that both l1 and l2 are null, then the if either l1 or l2 is null, we must
; return false.
; Similarly, after we have determined that it is NOT true that both (car l1) and (car l2) are atoms,
; then if either is an atom, we must return false
;
; Here is a simpler implementation that incorporates these observations:
(define eqlist?
  (lambda (l1 l2)
    (cond
     ; if both lists are null, return true
     ((and (null? l1)
           (null? l2)) #t)
     ; we now know that it is not the case that both l1 and l2 are null, so return false if one is
     ((or (null? l1)
          (null? l2)) #f)
     ; if the cars of l1 and l2 are both atoms, then these atoms must be the same, as well as the
     ; cdrs of l1 and l2
     ((and (atom? (car l1))
           (atom? (car l2))) (and (eqan? (car l1) (car l2))
                                  (eqlist? (cdr l1) (cdr l2))))
     ; we now know that it is not the case that both (car l1) and (car l2) are atoms, so return false
     ; if one is
     ((or (atom? (car l1))
          (atom? (car l2))) #f)
     ; at this point the cars of l1 and l2 are neither null nor atoms, so recurs into the cars and
     ; cdrs of l1 and l2
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

; The procedure (equal? s1 s2), for given S-expressions s1 and s1, is already implemented in Scheme,
; but here is my version:
(define equal?
  (lambda (s1 s2)
    (cond
     ; if both S-expressions are atoms, use (eqan? ...) to decide equality
     ((and (atom? s1)
           (atom? s2)) (eqan? s1 s2))
     ; at this point both are not atoms, so return false if one is
     ((or (atom? s1)
          (atom? s2)) #f)
     ; at this point neither are atoms, so use (eqlist? ...) to decide equality
     (else (eqlist? s1 s2)))))

; We can now use (equal? ...) to greatly simplify (eqlist? ...):
(define eqlist?
  (lambda (l1 l2)
    (cond
     ; test the null cases as before
     ((and (null? l1)
           (null? l2)) #t)
     ((or (null? l1)
          (null? l2)) #f)
     ; at this point we know that both l1 and l2 are S-expressions, so use (equal? ...)
     ; the atom cases are as before, and the list cases simply call (eqlist? ...) again
     (else (and (equal? (car l1) (car l2))
                (equal? (cdr l1) (cdr l2)))))))

; Here is an implementation of (rember s l), where 's is any S-expression and 'l is any list.
(define rember
  (lambda (s l)
    (cond
     ; if the list is null, there is nothing to do
     ((null? l) (quote ()))
     ; if the car of l is an atom, then check equality and recurs into the cdr of l...
     ((atom? (car l)) (cond
                       ((equal? s (car l)) (cdr l))
                       (else (cons (car l)
                                   (rember s (cdr l))))))
     ; at this point we know that the car of l is a list, so we need to recurs into it...
     (else (cond
            ((equal? s (car l)) (cdr l))
            (else (cons (car l)
                        (rember s (cdr l)))))))))

; We can simplify since (equal? ...) can handle any S-expression:
(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     (else (cond
            ((equal? s (car l)) (cdr l))
            (else (cons (car l)
                        (rember s (cdr l)))))))))


; Note that this version of (rember ...) is not a *-function since it does not recurs into (car l).
;
; We can futher simplify (rember ...) since the nested (cond ...) is unecessary:
(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? s (car l)) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l)))))))

; Finally, note that we could replace all (eq? ...) and (= ...) with (equal? ...), but we cannot
; replace (eqan? ...) with (equal? ...) since we defined (equal? ...) in terms of (eqan? ...) when
; the arguments are atoms.
