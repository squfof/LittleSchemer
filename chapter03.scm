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

; We haven't yet written (rember ...), but we can still write the code
; and figure out what the output ought to be.

(rember 'mint '(lamb chops and mint jelly))
; (lamb chops and jelly)

(rember 'mint '(lamb chops and mint flavored mint jelly))
; (lamb chops and flavored mint jelly)

(rember 'toast '(bacon lettuce and tomato))
; (bacon lettuce and tomato)

(rember 'cup '(coffee cup tea cup and hick cup))
; (coffee tea cup and hick cup)
;
; (rember a lat) takes an atom and a list of atoms, and returns a new list of atoms with the first
; occurance of the atom a removed from the given list.
;
; Here is the first attempt at (rember ...):
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (rember a (cdr lat))))))))

(rember 'bacon '(bacon lettuce and tomato))
; (lettuce and tomato)

(rember 'and '(bacon lettuce and tomato))
; (tomato)
;
; (rember ...) doesn't work as it should. It has forgotten all the atoms before the searched for
; atom. We must (cons ...) the atoms that we have already examined to the rest of the lat. Here is
; the new version:
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                        (rember a (cdr lat)))))))))

(rember 'and '(bacon lettuce and tomato))
; (bacon lettuce tomato)
;
; (rember ...) now works as it should. It can, however, be simplified somewhat:
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

(rember 'and '(bacon lettuce and tomato))
; (bacon lettuce tomato), as before!

(rember 'sauce '(soy sauce and tomato sauce))
; (soy and tomato sauce)

; We haven't yet written (firsts ...), but we can still write the code
; and figure out what the output ought to be.

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
; (apply plum grape bean)

(firsts '((a b)
          (c d)
          (e f)))
; (a c e)

(firsts '())
; ()

(firsts '((five plums)
          (four)
          (eleven green oranges)))
; (five four eleven)

(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))
; ((five plums) eleven (no))
;
; (firsts ...) takes a list which is either empty or contains only non-empty lists. It then builds
; a new list composed of the first S-expression of each internal list.
;
; Here is how I would write (firsts ...):
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l)) (firsts (cdr l)))))))

; We haven't yet written (insertR ...), but we can still write the code
; and figure out what the output ought to be.

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
; (ice cream with fudge topping for dessert)

(insertR 'jalapeno 'and '(tacos tamales and salsa))
; (tacos tamales and jalapeno salsa)

(insertR 'e 'd '(a b c d f g d h))
; (a b c d e f g d h)
;
; (insertR new old lat) inserts 'new to the right of the first occurance of 'old in 'lat.
;
; Here is the first attempt at (insertR ...):
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cdr lat))
       (else (cons (car lat)
                   (insertR new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
; (ice cream with for dessert)
;
; (insertR ...) does not work as it should. It did not insert 'new and deleted 'old.
; Let's try again, using (cons ...)
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old) (cons new (cdr lat)))
            (else (cons (car lat)
                        (insertR new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
; (ice cream with topping for dessert
;
; (insertR ...) still does not work as intended. It has replaced old with new.
;
; Here is another attempt:
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
; (ice cream with fudge topping for dessert)
;
; It works!
;
; Here is my implementation:
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old (cons new (cdr lat))))
     (else (cons (car lat)
                 (insertR new old (cdr lat)))))))
;
; and for the "left" version:
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (cons old (cdr lat))))
     (else (cons (car lat)
                 (insertL new old (cdr lat)))))))
;
; which can be simplified to:
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat)
                 (insertL new old (cdr lat)))))))
; since (cons new (cons old (cdr lat))) is just (cons new lat), since old was never removed!

; We haven't yet written (subst ...), but we can still write the code
; and figure out what the output ought to be.

(subst 'topping 'fudge '(ice cream with fudge for dessert))
; (ice cream with topping for dessert)

; Here is my implementation:
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst new old (cdr lat)))))))

; The procedure (subst2  new o1 2 lat) should replace the first occurance of either o1 or o2 with new.
;
; Here is my implementation that does not use (or ...):
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) o1) (cons new (cdr lat)))
     ((eq? (car lat) o2) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))
;
; and here is my implementation that does use (or ...):
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2)) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
; (vanilla ice cream with chocolate topping)

; The procedure (multirember a lat) should remove all occurances of the atom 'a in the list of atoms
; 'lat.
;
; Here is my implementation:
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup))
; (coffee tea and hick)

; We now consider the procedure (multiinsertR new old lat), which inserts 'new to the right of all
; occurances of 'old.
;
; Here is my implementation:
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old
                                (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))
;
; and the "left" version:
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new
                                (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertL new old (cdr lat)))))))

(multiinsertR 'fried 'fish '(chips and fish or fish and fried))
; (chips and fish fried or fish fried and fried)

(multiinsertL 'fried 'fish '(chips and fish or fish and fried))
; (chips and fried fish or fried fish and fried)

; The procedure (multisubst new old lat) should replace all occurances of 'old in 'lat with 'new.
;
; Here is my implementation:
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new
                                (multisubst new old (cdr lat))))
     (else (cons (car lat)
                 (multisubst new old (cdr lat)))))))

; A couple of examples before we turn to Chapter 4...
;
(multisubst 'fried 'fish '(chips and fish or fish and fried))
; (chips and fried or fried and fried)

(multisubst 'fish 'fried '(chips and fish or fish and fried))
; (chips and fish or fish and fish)
