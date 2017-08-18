; Required for some code in the text
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; The main focus of Chapter 2 are the following two procedures
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


(lat? '(Jack Sprat could eat no chicken fat))
; #t since (Jack Sprat ...) is a list of atoms

(lat? '((Jack) Sprat could eat no chicken fat))
; #f since (car '((Jack) Sprat ...)) is the list (Jack)

(lat? '(Jack (Sprat could) eat no chicken fat))
; #f since one of the S-expression in the list is a list

(lat? '())
; #t since the [empty] list does not contain a list

(lat? '(bacon and eggs))
; #t since...
; (bacon and eggs) is not empty and (car '(bacon and eggs)) is the atom 'bacon...
; (cdr '(bacon and eggs)) is (and eggs), which is not empty, and (car '(and eggs)) is the atom 'and...
; (cdr '(and eggs)) is (eggs), which is not empty, and (car '(eggs)) is the atom 'eggs...
; (cdr '(eggs)) is (), which is empty, so #t is returned.

(lat? '(bacon (and eggs)))
; #f since...
; (bacon (and eggs)) is not empty and (car '(bacon (and eggs))) is the atom 'bacon...
; (cdr '(bacon (and eggs))) is ((and eggs)), which is not empty...
; but (car '((and eggs))) is (and eggs), which is not an atom, so #f is returned.

(or
 (null? '())
 (atom? '(d e f g)))
; #t since (null? '()) is #t [Note: (atom? '(d e f g)) is not evaluated.]

(or
 (null? '(a b c))
 (null? '()))
; #t since, although (null? '(a b c)) is false, (null? '()) is true

(or
 (null? '(a b c))
 (null? '(atom)))
; #f since both arguments of (or ...) are false

(member? 'tea '(coffee tea or milk))
; #t since the atom 'tea is a member of the lat [list of atoms] '(coffee tea or milk)

(member? 'poached '(fried eggs and scrambled eggs))
; #f since the atom 'poached is not a member of the lat '(fried eggs and scrambled eggs)

(member? 'meat '(mashed potatoes and meat gravy))
; #t since...
; '(mashed potatoes and meat gravy) is not empty...
; and (car '(mashed ...)), the atom 'mashed, is not equal to 'meat...
; so we call (member? ...) on (cdr '(mashed ...)), which is '(potatoes and meat gravy)...
; which is not empty...
; and (car '(potatoes ...), the atom 'potatoes, is not equal to 'meat...
; so we call (member? ...) on (cdr '(potatoes ...), which is '(and meat gravy)...
; which is not empty...
; and (car '(and meat gravy)), the atom 'and, is not equal to 'meat...
; so we call (member? ...) on (cdr '(and meat gravy)), which is '(meat gravy)...
; which is not empty...
; and (car '(meat gravy)) is equal to 'meat, hence #t is returned.

(member? 'liver '(bagels and lox))
; #f since...
; '(bagels and lox) is not empty...
; and (car '(bagels and lox)), the atom 'bagels, is not equal to 'liver...
; so we call (member? ...) on (cdr '(bagels and lox)), which is '(and lox)...
; which is not empty...
; and (car '(and lox)), the atom 'and, is not equal to 'liver...
; so we call (member? ...) on (cdr '(and lox)), which is '(lox)...
; which is not empty...
; and (car '(lox)), the atom 'lox, is not equal to 'liver...
; so we call (member? ...) on (cdr '(lox)), which is '()...
; which is empty, hence #f is returned.

