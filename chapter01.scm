; Required for some code in the text
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'atom)
; #t

(atom? 'turkey)
; #t

(atom? 1492)
; #t

(atom? 'u)
; #t

(atom? '*abc$)
; #t

'(atom)
; the list (atom)

'(atom turkey or)
; the list (atom turkey or)

'(atom turkey) or
; not a list, produces an error

'((atom turkey) or)
; the list ((atom turkey) or)

'xyz
; the S-expression [since it's an atom] xyz

'(x y z)
; the S-expression [since it's a list] (x y z)

'((x y) z)
; the S-expression [since it's a list] ((x y) z)

'(how are you doing so far)
; the list of S-expressions (how are you doing so far)

(length '(how are you doing so far))
; there are six S-expressions in the given list: how, are, you, doing, so, far

'(((how) are) ((you) (doing s)) far)
; the list of S-expressions (((how) are) ((you) (doing s)) far)

(length '(((how) are) ((you) (doing so)) far))
; there are three S-expression in the given list: ((how) are), ((you) (doing so)), far

'()
; (), the list that contains zero S-expressions

(atom? '())
; #f, since () is a list not an atom

'(() () () ())
; (() () () ()), the list that contains the four S-expressions [empty list] ()

(car '(a b c))
; a

(car '((a b c) x y z))
; (a b c)

(car 'hotdog)
; produces an error, hotdog is an atom, not a list

(car '())
; produces an error, the list is empty and has no first element

(car '(((hotdogs)) (and) (pickle) relish))
; ((hotdogs)), the first S-expression of the given list

(car (car '(((hotdogs)) (and) (pickle) relish)))
; (hotdogs), the first [only] S-expression of the first S-expression of the given list

(cdr '(a b c))
; (b c), the list without (car '(a b c))

(cdr '((a b c) x y z))
; (x y z), the list without (car '((a b c) x y z))

(cdr '(hamburger))
; (), the [empty] list without (car '(hamburger))

(cdr '((x) t r))
; (t r), the list without (car '((x) t r))

(cdr 'hotdogs)
; produces an error since hotdogs is an atom, not a list

(cdr '())
; prduces an error since () is empty

(car (cdr '((b) (x y) ((c)))))
; (x y), since (cdr '((b) (x y) ((c)))) is ((x y) ((c))), and (car '((x y) ((c)))) is (x y)

(cdr (cdr '((b) (x y) ((c)))))
; (((c))), since (cdr '((b) (x y) ((c)))) is ((x y) ((c))), and (cdr '((x y) ((c)))) is (((c)))

(cdr (car '(a (b (c)) d)))
; produces an error since (car '(a (b (c)) d)) is the atom a, which is not a list so (cdr a) fails

; as we can see (car ...) and (cdr ...) take any non-empty list

(cons 'peanut '(butter and jelly))
; (peanut butter and jelly)

(cons '((help) this) '(is very ((hard) to learn)))
; (((help) this) is very ((hard) to learn))

; (cons ...) takes two arguments. The first is any S-expression, and the second is any list.

(cons (a b (c)) ())
; ((a b (c)))

(cons 'a '())
; (a)

(cons '((a b c)) 'b)
; shouldn't work since b is not a list, but does produce (((a b c)) . b)

(cons 'a 'b)
; again, shouldn't work since b is not a list, but does produce (a . b)

(cons 'a (car '((b) c d)))
; (a b) since (car '((b) c d)) is (b), then (cons 'a '(b)) is (a b)

(cons 'a (cdr '((b) c d)))
; (a c d) since (cdr '((b) c d)) is (c d), then (cons 'a '(c d)) is (a c d)

(null? (quote ()))
; #t since () is empty, equivalent to (null? '())

(null? '(a b c))
; #f since (a b c) is non-empty

(null? 'spaghetti)
; #f, intended only for list but (null? ...) will return false for everything except the empty list

(atom? 'Harry)
; #t

(atom? '(Harry had a heap of apples))
; #f

; (atom? ...) takes only one argument, an S-expression

(atom? (car '(Harry had a heap of apples)))
; #t since (car '(Harry had a heap of apples)) is Harry, an atom

(atom? (cdr '(Harry had a heap of apples)))
; #f since (cdr '(Harry had a heap of apples)) is (had a heap of apples), an atom

(atom? (cdr '(Harry)))
; #f since (cdr '(Harry)) is the empty list, which is not an atom

(atom? (car (cdr '(swing low sweet cherry oat))))
; #t since (cdr '(swing low sweet cherry oat)) is (low sweet cherry oat),
; and (car '(low swee cherry oat)) is low, an atom

(atom? (car (cdr '(swing (low sweet) cherry oat))))
; #f, since (cdr '(swing (low sweet) cherry oat)) is ((low sweet) cherry oat),
; and (car '((low sweet) cherry oat)) is (low sweet), which is a list, not an atom

(eq? 'Harry 'Harry)
; #t the atoms are the same

(eq? 'butter 'margarine)
; #f since the atoms are not the same

; (eq? ...) takes two arguments, both of which must be non-numeric atoms
; not exacty. in practice, lists may be arguments, and two lists are eq? if they are the same list

(eq? '() '(strawberry))
; #f either because () and (strawberry) are not non-numeric atoms, or because the lists are not the same

(eq? 6 7)
; #f, though according to Little Schemer, this should be an error since 6 and 7 are numeric

(eq? (car '(Mary had a littel lamb chop)) 'Mary)
; #t since (car '(Mary had a little lamb chop)) is Mary

(eq? (cdr '(soured milk)) 'milk)
; #f since (cdr '(sour milk)) is (milk), not milk
; though according to Little Schemer, this should be an error since (milk) is not a non-numeric atom

(eq?
 (car '(beans beans we need jelly beans))
 (car (cdr '(beans beans we need jelly beans))))
; #t since (car '(beans beans we need jelly beans)) is beans [the first in the list]
; and (cdr '(beans beans we need jelly beans)) is (beans we need jelly beans)
; and (car '(beans we need jelly beans)) is beans [the second from the original list]
