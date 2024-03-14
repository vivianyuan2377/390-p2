;;; Test cases for Scheme, Phase 1.

;;; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

(load "env.scm")
(load "assert.scm")

; dictionary tests

(display "Running dictionary tests...")
(newline)

(define dict1 (dictionary))
(assert-equal (dict1 'length) 0)
(assert-false (dict1 'contains 3))

(dict1 'insert 3 5)
(assert-equal (dict1 'length) 1)
(assert-true (dict1 'contains 3))
(assert-equal (dict1 'get 3) 5)

(dict1 'insert -1 -3)
(assert-equal (dict1 'length) 2)
(assert-true (dict1 'contains 3))
(assert-true (dict1 'contains -1))
(assert-equal (dict1 'get 3) 5)
(assert-equal (dict1 'get -1) -3)

(dict1 'insert 'a #f)
(dict1 'insert 'b '())
(dict1 'insert 'c 0)
(dict1 'insert 'd "")
(assert-true (dict1 'contains 'a))

(define dict2 (dictionary))
(dict2 'insert 'x 3)
(dict2 'insert 'y 5)
(dict2 'insert 'y 8)
(assert-true (dict2 'contains 'x))
(assert-true (dict2 'contains 'y))
(assert-equal (dict2 'length) 2)
(assert-equal (dict2 'get 'x) 3)
(assert-equal (dict2 'get 'y) 8)

(dict2 'insert 'a #f)
(dict2 'insert 'b '())
(dict2 'insert 'c 0)
(dict2 'insert 'd "")
(assert-true (dict2 'contains 'a))
(assert-true (dict2 'contains 'b))
(assert-true (dict2 'contains 'c))
(assert-true (dict2 'contains 'd))
(assert-equal (dict2 'get 'a) #f)
(assert-equal (dict2 'get 'b) '())
(assert-equal (dict2 'get 'c) 0)
(assert-equal (dict2 'get 'd) "")

; frame tests

(display "Running frame tests...")
(newline)

(define env (frame '()))
(assert-false (env 'contains 'x))

(env 'insert 'x 3)
(env 'insert 'lst (list 4))
(assert-true (env 'contains 'x))
(assert-equal (env 'get 'x) 3)

(define subenv1 (frame env))
(assert-equal (subenv1 'get 'x) 3)
(assert-equal (subenv1 'get 'lst) '(4))

(subenv1 'insert 'y 5)
(assert-false (env 'contains 'y))
(assert-true (subenv1 'contains 'y))

(define subenv2 (frame env))
(assert-false (subenv2 'contains 'y))

(subenv2 'insert 'y 6)
(assert-equal (subenv1 'get 'y) 5)
(assert-equal (subenv2 'get 'y) 6)

(define subenv3 (frame subenv2))
(assert-equal (subenv3 'get 'x) 3)
(assert-equal (subenv3 'get 'y) 6)

(subenv3 'insert 'z 7)
(assert-false (env 'contains 'z))
(assert-false (subenv1 'contains 'z))
(assert-false (subenv2 'contains 'z))
(assert-true (subenv3 'contains 'z))

(subenv3 'insert 'y 8)
(assert-equal (subenv1 'get 'y) 5)
(assert-equal (subenv2 'get 'y) 6)
(assert-equal (subenv3 'get 'y) 8)

(subenv3 'insert 'y 9)
(assert-equal (subenv1 'get 'y) 5)
(assert-equal (subenv2 'get 'y) 6)
(assert-equal (subenv3 'get 'y) 9)

(env 'insert 'a #f)
(env 'insert 'b '())
(env 'insert 'c 0)
(env 'insert 'd "")
(assert-true (subenv3 'contains 'a))
(assert-true (subenv3 'contains 'b))
(assert-true (subenv3 'contains 'c))
(assert-true (subenv3 'contains 'd))
(assert-equal (subenv3 'get 'a) #f)
(assert-equal (subenv3 'get 'b) '())
(assert-equal (subenv3 'get 'c) 0)
(assert-equal (subenv3 'get 'd) "")

(display "Done.")
(newline)
