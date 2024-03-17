(load "test.scm")

(global-env 'insert 'x 3)
(global-env 'insert 'y -1)

(define proc1 (primitive-procedure 'cons 2 cons))
(proc1 'call global-env 3)
