#lang racket

(define l (list 7 5 3 1 9 8 -1))

(struct root (data left right) #:transparent #:mutable)

(define (create root l)
   (let ((ele (car l)))
     (cond
       ((equal? ele -1)
        root
       )
       (else
        (create (putNodeInBST ele root) (cdr l))
        )
     )
   )
)


(define (putNodeInBST ele roott)
 (cond
  (
   (null? roott)
   (root ele null null)
  )
  (
   (<= ele (root-data roott))
    (cond
      ((null? (root-left roott))
       (set-root-left! roott (root ele null null))       
      )
      (else
       (putNodeInBST ele (root-left roott))
      )
    )
    roott
  )
  (else
   (cond
      ((null? (root-right roott))
       (set-root-right! roott (root ele null null))
      )
      (else
       (putNodeInBST ele (root-right roott))
      )
    )
   roott
  )
 )
)

(define (inorder tree)
  (if (null? tree)
      '()
      (append
        (inorder (root-left tree))
        (list (root-data tree))
        (inorder (root-right tree))
      )
  )
)


;(inorder (create null l))
;'(1 3 5 7 8 9)




