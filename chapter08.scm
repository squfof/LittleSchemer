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

;;;;;;;;;;;;

; We have not yet implemented (rember-f test? a l), but we can figure out what it does by looking at
; some examples:

(rember-f = '5 '(6 2 5 3))
; (6 2 3)

(rember-f eq? 'jelly '(jelly beans are good))
; (beans are good)

(rember-f equal? '(pop corn) '(lemondae (pop corn) and (cake)))
; (lemonade and (cake))

; Here is my implementation:
(define rember-f
  (lambda (test? s l)
    (cond
     ((null? l) (quote ()))
     ((test? s (car l)) (cdr l))
     (else (cons (car l)
                 (rember-f test? s (cdr l)))))))

; We have seen the construction (lambda ...) several times. For example, (lambda (a l) ...) defines a
; function with two arguments. So far, these functions return atoms or lists.
;
; It turns out that we can use (lambda ...) to create functions. For example
(lambda (a)
  (lambda (x)
    (eq? x a)))
; is a function that returns a function that tests eq? between its input and 'a.
;
; We now give this procedure a name so we can subsequently use it:
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; Here are some examples of how this can be used:

(eq?-c 'salad)
; returns a function that tests eq? between its input and 'salad. Let's give it a name:
(define eq?-salad
  (eq?-c 'salad))

(eq?-salad 'salad)
; #t

(eq?-salad 'tuna)
; #f

; We can now use this idea to rewrite (rember-f test?) that takes a single input test? and returns a
; procedure that implements (rember ...) using the given test?:
(define rember-f
  (lambda (test?)
    (lambda (s l)
      (cond
       ((null? l) (quote ()))
       ((test? s (car l)) (cdr l))
       (else (cons (car l)
                   ((rember-f test?) s (cdr l))))))))

; Let's try it out:

((rember-f eq?) 'tuna '(tuna salad is good))
; (salad is good)

; Or we can name the function then use it...
(define rember-eq?
  (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))
; (salad is good)

; ... or not:
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
; (shrimp salad and salad)

((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
; (equal? eqan? eqlist? eqpair?)

; We can do the same thing to (insertR ...) and (insertL ...):
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

; We now consider how we might write (insert-g ...) that inserts either at the left or right. The key is
; to note that (insertR-f ...) and (instertL-f ...) really only differ in the second condition. The test
; is the same, only the order of cons-ing 'new and 'old is different.
;
; Here is my implementation of a procedure that takes three arguments and cons-es the first arguement
; onto the result of cons-ing the second onto the third:
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; And here is my implementation of a procedure that takes three arguments and cons-es the second
; argument onto the result of cons-ing the first onto the third:
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; We can now implement (insert-g seq) which returns (insertL ...) if given seqL, and returns
; (insertR ...) if given seqR
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((eq? old (car l)) (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

; We can now define (insertL ...) and (insertR ...):
(define insertL
  (insert-g seqL))

(define insertR
  (insert-g seqR))

; We don't really need to give names to seqL and seqR, since then we need to remember their names and
; what they do. Instead we can define (insertL ...) and (insertR ...) as follows:
(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define insertR
  (insert-g (lambda (new old l)
              (cons old (cons new l)))))

; Recall our implementation of (subst ...):
(define subst
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((eq? old (car l)) (cons new (cdr l)))
     (else (cons (car l)
                 (subst new old (cdr l)))))))

; Note how similar (subst ...) is to (insertL ...) and (insertR ...). Again, only the second condition
; is differnt -- instead of inserting 'new to the left or right of 'old, we replace 'old with 'new. So
; we should define a function (seqS new old l) that cons-es 'new onto 'l and ignores 'old.
;
; We include
; the argument 'old so that the arguemnt list of seqS is the same as that of seqR and seqL:
(define seqS
  (lambda (new old l)
    (cons new l)))

; We can now define (subst new old l) using (insert-g seq):
(define subst
  (insert-g seqS))

; Of course, as with insertR and insertL, we don't really need to define seqS to define subst:
(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

; Consider the following two procedures:
(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

; The procedure (seqrem new old l) just returns l, so unlike seqL, seqR, and seqS, no cons-ing will
; happen, which has the effect of removing 'old from the list ['new is ignored altogether]. This is
; the procedure we would need to define (rember a l) using insert-g.
;
; It follows that (yyy a l) will remove 'a from the list 'l. We need #f for the first argument since any
; function that insert-g produces takes three arguments. seqrem ignores the first two arguments, so no
; problem there; and insert-g only passes along the first argument to seq and itself, and doesn't use
; it otherwise, so no problem there either.
;
; In fact, I think we could replace #f with anything and yyy will still work exactly like rember.
;
; Here is an example:

(yyy 'sausage '(pizza with sausage and bacon))
; (pizza with and bacon)

; Recall the procedure (value ...). My original version was a bit different from the books in that I did
; not use an else clause to handle ^.
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

; There is a lot of cut-and-paste code here. Let's write a procedure (atom-to-function x) that takes
; one argument, x, and returns the function (o+ ...) if (eq? x (quote +)), etc.
(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) o+)
     ((eq? x (quote *)) o*)
     (else o^))))

; For example:

(atom-to-function (operator '(5 3)))
; #<procedure #2 o+>, which is the function o+, not the atom '+

; We can now use (atom-to-function ...) to rewrite (value ...):
(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     (else ((atom-to-function (operator aexp))
            (value (1st-sub-exp aexp))
            (value (2nd-sub-exp aexp)))))))

; Let's now write (multirember-f test?) that produces a (multirember a lat) procedure based on the given
; equality procedure test?:
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

; Now let's test it out:

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
; (shrimp salad salad and)

; We can define multirember-eq?, which is multirember based on the equality test eq?:
(define multirember-eq?
  (multirember-f eq?))

; If we want to create new equality tests, such as one that tests for equality with 'tuna, we can do
; so using the previously defined eq?-c procedure:
(define eq?-tuna
  (eq?-c (quote tuna)))

; We now implement (multiremberT ...) that is similar to (multirember-f ...). However, instead of being
; given a equality procedure test? and returning a multirember procedure based on test?,
; (multiremberT ...) takes a procedure like eq?-tuna, which accepts only a single atom, and a lat, and
; then removes anthing that produces #t in the eq?-tuna procedure [i.e., 'tuna, in this case]:
(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat)) (multiremberT test? (cdr lat)))
     (else (cons (car lat)
                 (multiremberT test? (cdr lat)))))))

; Let's test it out:

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
; (shrimp salad salad and)

; Here is a complicated looking function:
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

; I'm not sure what multirember&co does, but I guess we'll find out!
;
; Here is something simpler:
(define a-friend
  (lambda (x y)
    (null? y)))

; Clearly a-friend takes two arguments and returns true if the second is null.
;
; Let us return to the mysterious procedure multirember&co...

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
; no idea...

(multirember&co 'tuna '() a-friend)
; #t since (null? lat) is true, and (col '() '()) is true since its second argument is null

(multirember&co 'tuna '(tuna) a-friend)
; not sure, but (null? lat) is not true, so we check (eq? a (car lat)) which is, so we recurs on 'tuna
; and (cdr lat), which is nul, and a new function...
;
; We are told that "col" stands for "collector", sometimes called a "continuation". Here is a new
; collector, the function that is passed to multirember&co if (eq? a (car lat)) is true:
(define new-friend
  (lambda (newlat seen)
    (col newlat (cons (car lat) seen)))) ; technically won't work since new-friend doens't know col or lat

; If (car lat) is 'tuna and col is a-friend, then the above definition would be:
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

; At this point, multirember&co receives 'tuna and (cdr lat), which is null, and a collector which is
; new-friend.
;
; So multirember&co asks if its second argument is null, which it is, so it passes '() and '() to
; the new collector new-friend.
;
; Then new-friend passes '() and (cons 'tuna '()), which is '(tuna) to a-friend.
;
; Finally, a-friend returns false since it's second argument '(tuna) is not null. Therefore we can
; conclude that:

(multirember&co 'tuna '(tuna) a-friend)
; #f

; Thinking about this some more, it appears that multirember&co runs through the given 'lat and
; compares each element with 'a, building two lists as it goes. The first list contains those elements
; of 'lat that are not equal to 'a, and the second list contains those elements that are equal to 'a.
;
; Once multirember&co has examined each element of 'lat, it sends the two lists to col. If col is
; a-friend, it will only return true if the second list is empty, meaning that 'a did not appear as a
; element of 'lat.
;
; This explains why:
(multirember&co 'tuna '() a-friend)
; returns true ['() does not contain 'tuna], but:
(multirember&co 'tuna '(tuna) a-friend)
; returns false ['(tuna) does contain 'tuna].
;
; We can now predict that:
(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
; will return false.

; One more example:
(multirember&co 'tuna '(and tuna))
; #f since '(and tuna) contains 'tuna. Let's verify...
;
; First question is (null? '(and tuna)) which is false. Second question is (eq? 'tuna 'and) which is
; also false. So we go to the else condition, which passes 'tuna and '(tuna) and the function
(lambda (newlat seen)
  (a-friend (cons 'and newlat) seen))
; which the books names:
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))
; to multirember&co.
;
; We now consider (multirember&co 'tuna '(tuna) latest-friend). The first question is (null? '(tuna))
; which is false. Second question is (eq? 'tuna 'tuna) which is true. We therefore pass 'tuna and '()
; and the function
(lambda (newlat seen)
  (latest-friend newlat (cons 'tuna seen)))
; which I will name:
(define latest-latest-friend
  (lambda (newlat seen)
    (latest-friend newlat (cons 'tuna seen))))
; to multirember&co.
;
; We now consider (multirember&co 'tuna '() latest-latest-friend). The first question is (null? '())
; which is true, so we evaluate (latest-latest-friend '() '()), which is (latest-friend '() '(tuna)),
; which is (a-friend '(and) '(tuna)), which is false since '(tuna) is not null.

; The first example we considered can now be understood:

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
; #f. The procedure will construct two lists, '(strawberries and swordfish) and '(tuna), which will be
; passed to a-friend, which will return false since '(tuna) is not null.

; Here is another collector:
(define last-friend
  (lambda (x y)
    (length x)))
; which computes the length of the first argument and ignores the second. Let's use it with
; multirember&co:

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)
; 3 since two lists, '(strawberries and swordfish) and '(tuna), will be passed to last-friend, which
; will compute the length of '(strawberries and swordfish), which is 3.


; Recall multiinsertR and multiinsertL:
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

; Let's try to implement multiinsertLR which inserts 'new to the left of 'oldL and to the right of
; 'oldR in 'lat if 'oldL and 'oldR are different:
(define my-multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? oldL (car lat)) (cond
                            ((eq? oldL oldR) (cons oldR
                                                   (cons new (my-multiinsertLR new oldL oldR (cdr lat)))))
                            (else (cons new
                                        (cons oldL (my-multiinsertLR new oldL oldR (cdr lat)))))))
     (else (cons (car lat)
                 (my-multiinsertLR new oldL oldR (cdr lat)))))))
; OK, so looks like I misunderstood what multiinsertLR is supposed to do. Mine inserts 'new to the left
; of 'oldL, unless 'oldL and 'oldR are the same, in which case we insert 'new to the right.
;
; After reading more carefully, I see that we always insert 'new to the left of 'oldL, and then insert
; 'new to the right of 'oldR if 'oldL and 'oldR are different:
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

; Now let's try to implement (multiinsertLR&co new oldL oldR lat col) that will apply 'col on the new
; 'lat, on the number of left insertions, and the number of right insertions:
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col (quote ()) 0 0))
     ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                            (lambda (newlat L R)
                                              ...)))
     ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                             (lambda (newlat L R)
                                               ...)))
     (else (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               ...))))))

; We need to fill in the dots. In the first case, we need to cons new onto the cons of oldL and newlat
; and increment L. In the second case, we need to cons oldR onto the cons of new and newlat and
; increment R. In the third case, we need to cons the car of lat onto newlat and leave L and R
; unchanged:
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

; Before we can try this out, we need a collector. Here are three that are useful in checking out work:
(define first-friend
  (lambda (lat x y)
    lat))

(define second-friend
  (lambda (lat x y)
    x))

(define third-friend
  (lambda (lat x y)
    y))

; Let's use these three "friends" to test out multiinsertLR&co:

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) first-friend)
; (chips salty and salty fish or salty fish and chips salty)

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) second-friend)
; 2

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) third-friend)
; 2

; Here is a function that tests whether or not a given integer is even. The idea is that if it is not,
; then n/2 will not be an integer and (/ n 2) will be less than the exact value of n/2. Therefore
; (* (/ n 2) 2) will be less than n:
(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

; We now write (evens-only* l) which removes all odd numbers from a list of nested lists:
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

; Let's try it out:

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
; ((2 8) 10 (() 6) 2)

; Suppose we wanted to compute the sum of the odd numbers and the product of the even numbers in the
; list ((9 1 2 8) 3 10 ((9 9) 7 6) 2). We could write a collector-version of evens-only* that computes
; these quatities as it moves through the list.
;
; Here is the book's implementation:
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
     (else ...))))

; So far, so good, but we still need to figure out what to do do if (car l) is a list. When we were
; simply building the list of evens-only, we recurs on (car l) and (cdr l) and cons the two. However,
; apparently we will use the collector to handle (cdr l) once we have finished with (car l).
;
; In other words, the implementation will look like:
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
     (else (evens-only*&co (car l) ...)))))

; We what will the collector do? As mentioned above, after visiting each member of (car l) it must use
; evens-only*&co to visit (cdr l), collect a list like (cdr l) but without the odds, as well as the
; product of the evens and the sum of the odds.
;
; This means the collector will start like this:
(lambda (al ap as)
  (evens-only*&co (cdr l) ...))
;
; So what does this collector do once it visits each member of (cdr l)? It cons-es the results for the
; lists in car and cdr into one list of evens-only, and multplies and sums the results of the respective
; products and sums (those from car and cdr). Finally, it passes the final list, product, and sum to
; the old collector.
;
; We can now finish the collector used in the final else clause:
(lambda (al ap as) ; "a" stands for the "a" in car
  (evens-only*&co (cdr l) (lambda (dl dp ds) ; "d" stands for the "d" in cdr
                            (col (cons al dl) (o* ap dp) (o+ as ds)))))
; Finally, let's put this code into our implementation of evens-only*&co:
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

; As before, we need a collector to test out our procedure. The following cons the sum of the odds onto
; the cons of the product of the evens onto the new list will all non-evens removed:
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

; So let's try it out:

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
; (38 1920 (2 8) 10 (() 6) 2)
;
; It works! The contents of the cdr of the cdr of this result are the same as the contents of (evens-only* ...).
; The sum of the odds is: 9 + 1 + 3 + 9 + 9 + 7 = 38.
; The product of the evens is: 2 * 8 * 10 * 6 * 2 = 1920.
