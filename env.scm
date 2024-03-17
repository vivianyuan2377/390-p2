; env.scm -- provides a dictionary abstraction, as well as a frame ADT
; that represents an environment.
;
; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

; Returns a dictionary object that can be used by passing messages to
; it.
;
; Does not do any error checking.
;
; > (define d (dictionary))
; > (d 'contains 'x)
; #f
; > (d 'insert 'x 3)
; > (not (d 'contains 'x))
; #f
; > (d 'get 'x)
; 3
; > (d 'insert 'x 4)
; > (d 'get 'x)
; 4
; > (d 'length)
; 1
;
; You will need to use some subset of set!, set-car!, set-cdr!. You
; may find the assoc procedure useful.
(define (dictionary)
  (let ((dic '()))
    (lambda (message . args) 
      (case message
        ((contains)
          (assoc (car args) dic))
        ((insert)
          (let ((pair-found (assoc (car args) dic)))
            (if pair-found
              (set-cdr! pair-found (cadr args))
              (set! dic (cons (cons (car args) (cadr args)) dic))
            )
          ))
        ((get)
          (let ((pair-found (assoc (car args) dic)))
            (if pair-found
              (cdr pair-found)
              (error "Key not found in dictionary")
            )
          ))
        ((length)
          (length dic))
        (else
          (error "Unsupported operation"))
      )
    )
  )
)


; Returns an object that represents a frame in an environment, with
; the given parent. The parent must either be the empty list or
; another frame object.
;
; Does not do any error checking.
;
; > (define f1 (frame '()))
; > (define f2 (frame f1))
; > (f1 'insert 'x 3)
; > (f1 'get 'x)
; 3
; > (f2 'get 'x)
; 3
; > (f2 'insert 'x 4)
; > (f1 'get 'x)
; 3
; > (f2 'get 'x)
; 4
; > (f2 'insert 'y -7)
; > (f2 'get 'y)
; -7
; > (f1 'contains 'y)
; #f
(define (frame parent)
  (let ((d (dictionary)))
    (lambda (message . args)  
      (case message
        ((contains)
          (if (d 'contains (car args))
            #t 
            (and (not (null? parent)) (parent 'contains (car args)))
          )
        )
        ((insert)
          (apply d (cons 'insert args))
        )
        ((get)
          (if (d 'contains (car args))
            (d 'get (car args))
            (and (not (null? parent)) (parent 'get (car args)))
          )
        )
      )
    )
  )
)
