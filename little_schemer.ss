(define atom? 
  (lambda (x) 
    (and (not (pair? x)) (not (null? x)))))

(define add1 
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;===============================
(define member? 
  (lambda (a lat) 
    (cond 
      ((null? lat) #f ) 
      (else (or (eq? (car lat) a) 
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
    (lambda (l)
        (cond
        ((null? l) l)
        (else (cons (car (car l)) (firsts (cdr l)))))))


(define insertR
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) (cons (car lat)
                                        (cons new (cdr lat))))
             (else (cons (car lat)
                         (insertR new old (cdr lat)))))))))

(define insertL
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) (cons new lat))
             (else (cons (car lat)
                         (insertL new old (cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat))))
    )))

(define multiinsertR
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) 
              (cons (car lat)
                (cons new
                  (multiinsertR new old (cdr lat)))))
             (else 
              (cons (car lat)
                (multiinsertR new old (cdr lat)))))
         ))))

(define multiinsertL
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) 
              (cons new
                (cons (car lat)
                  (multiinsertL new old (cdr lat)))))
             (else 
              (cons (car lat)
                (multiinsertL new old (cdr lat)))))
         ))))

(define multisubst
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) 
              (cons new (multisubst new old (cdr lat))))
             (else 
              (cons (car lat)
                (multisubst new old (cdr lat))))))
         )))

; ==============================


(define ++
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (++ (sub1 a) (add1 b)))
    )))

(define --
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (-- (sub1 a) (sub1 b))))))

(define sumtup
  (lambda (s)
    (cond
      ((null? s) 0)
      (else (++ (car s) (addtup (cdr s)))))))

(define **
  (lambda (a b)
    (cond 
      ((zero? a) 0)
      (else (++ b (** (sub1 a) b))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2) 
      ((null? tup2) tup1)
      (else (cons (++ (car tup1) (car tup2)) 
                  (tup+ (cdr tup1) (cdr tup2))))
    )))


(define gt
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (gt (sub1 a) (sub1 b)))
    )))

(define lt
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (lt (sub1 a) (sub1 b)))
    )))

(define ==
  ; equality for numbers
  (lambda (a b)
    (cond
      ((or (gt a b) (lt a b)) #f)
      (else #t))))

(define ^^
  (lambda (z a)
    (cond
      ((zero? a) 1)
      (else (** z (^^ z (sub1 a))))
    )))

(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div ( - n m) m )))
    )))

;===============================

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat))))
    )))

(define pick
  (lambda (n lat)
    (cond 
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat)))
    )))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) 
                  (rempick (sub1 n) (cdr lat))))
    )))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat))))
    )))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat)))
    )))

(define eqan?
  ; equality of atoms, just like eq? 
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (== a1 a2))
      ((or  (number? a1) (number? a2)) #f)
      (else                            (eq? a1 a2))
    )))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat)))
    )))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

; ==============================
(define rember*
  (lambda (a l)
    (cond
      ((null? l) l)
      ((atom? (car l)) 
       (cond 
         ((eqan? a (car l)) 
          (rember* a (cdr l)))
         (else 
           (cons (car l) (rember* a (cdr l))))
       ))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l))))
    )))
; ----- test data --------->
(list (list (list 'tomato 'sauce))
      (list (list 'bean) 'sauce)
      (list 'and (list (list 'flying)) 'sauce))
; <----- test data ---------      
(define insertR*
  (lambda (old new l)
    (cond
      ((null? l) l)
      ((atom? (car l))
       (cond
         ((eqan? old (car l))
          (cons old
                (cons new
                      (insertR* old new (cdr l)))))
         (else (cons (car l)
                     (insertR* old new (cdr l))))
       ))
      (else (cons (insertR* old new (car l))
                  (insertR* old new (cdr l))))
    )))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) 
       (cond 
         ((eqan? a (car l))
          (add1 (occur* a (cdr l))))
         (else 
          (occur* a (cdr l)))))
      (else (++ (occur* a (car l))
                (occur* a (cdr l))))
    )))

(define member1*
  (lambda (a l)
    (gt (occur* a l) 0)))

(define member* 
  (lambda (a l) 
    (cond
      (( null? l) #f ) 
      (( atom? (car l)) 
       (or (eq? (car l) a) 
           (member* a ( cdr l))))
      (else 
       (or (member* a (car l)) 
           (member* a ( cdr l))))
    )))


; ==============================
(define equal? 
  ; equality of S-expressions
  (lambda (s1 s2) 
    (cond 
      ((and (atom? s1 ) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1 ) (atom? s2)) #f )
      (else (eqlist? s1 s2))
    )))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (equal? (car l1 ) (car l2))
             (eqlist? (cdr l1 ) (cdr l2))))
    )))

(define remberS
  ; remove member S-expression
  (lambda (s l)
    (cond
      ((null? l) l)
      ((equal? (car l) s) (remberS s (cdr l)))
      (else (cons (car l) (remberS s (cdr l))))
    )))

; ==============================
(define numbered?
; infix
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote ++))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote **))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^^))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
    )))

(define numbered2?
; infix
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp))))))
    )))

(define value
; infix
  (lambda (aexp)
    (cond 
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) (quote ++))
       (++  (value (car aexp))
            (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote **))
       (**  (value (car aexp))
            (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^^))
       (^^  (value (car aexp))
            (value (car (cdr (cdr aexp))))))
    )))


(define valuep
; prefix
  (lambda (aexp)
    (cond 
      ((atom? aexp) aexp)
      ((eq? (car aexp) (quote ++))
       (++  (valuep (car (cdr aexp)))
            (valuep (car (cdr (cdr aexp))))))
      ((eq? (car aexp) (quote **))
       (**  (valuep (car (cdr aexp)))
            (valuep (car (cdr (cdr aexp))))))
      ((eq? (car aexp) (quote ^^))
       (^^  (valuep (car (cdr aexp)))
            (valuep (car (cdr (cdr aexp))))))
    )))


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define valuep2
; prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp) 
      ((eq? (operator nexp) (quote ++))
       (++ (valuep2 (1st-sub-exp nexp))
            (valuep2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote **))
       (** (valuep2 (1st-sub-exp nexp))
           (valuep2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote ^^))
       (^^ (valuep2 (1st-sub-exp nexp))
           (valuep2 (2nd-sub-exp nexp))))
    )))

; ==============================
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define ++s
  (lambda (a b)
    (cond
      ((sero? a) b)
      (else (++s (zub1 a) (edd1 b)))
    )))

(define --s
  (lambda (a b)
    (cond
      ((sero? b) a)
      (else (--s (zub1 a) (zub1 b))))))

(define **s
  (lambda (a b)
    (cond 
      ((sero? a) (quote ()))
      (else (++s b (**s (zub1 a) b))))))   
;===============================

(define set?
  (lambda (lat)
    (cond 
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat)))
    )))

(define makeset 
  (lambda (lat) 
    (cond 
      ((null? lat) (quote ())) 
      (else 
        (cons (car lat) 
              (makeset 
                (multirember (car lat) (cdr lat)))))
    )))

(define subset?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      (else (and (member? (car set1) set2) 
                 (subset? (cdr set1) set2)))
    )))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect
  (lambda (set1 set2)
    (cond 
      ((null? set1) (quote ()))
      ((member? (car set1) set2) 
        (cons (car set1) 
              (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2))
    )))

(define union 
  (lambda (set1 set2) 
    (cond 
      ((null? set1) set2) 
      ((member? (car set1 ) set2) 
        (union (cdr set1 ) set2)) 
      (else (cons (car set1 ) 
                  (union (cdr set1 ) set2)))
    )))

; ==============================
(define a-pair? 
  (lambda (x) 
    (cond 
      ((atom? x) #f ) 
      ((null? x) #f ) 
      ((null? (cdr x)) #f )
      ((null? (cdr (cdr x))) #t ) 
      (else #f ) 
    )))

(define first
  (lambda (pair) (car pair)))

(define second
  (lambda (pair) (car (cdr pair))))

(define build
  (lambda (a1 a2)
    (cons a1 (cons a2 (quote ())))
  ))

(define revpair
  (lambda (pair)
    (build (second pair) (first  pair))
  ))

(define fun? 
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons (revpair (car rel))
                  (revrel (cdr rel))))
    )))

(define seconds
  (lambda (rel)
    (firsts (revrel rel))
  ))

(define fullfun?
  (lambda  (rel)
    (and (set? (firsts rel)) 
         (set? (seconds rel)))))

(define one-to-one
  (lambda (fun)
    (fun? (revrel fun))))

; ==============================

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) l)
      ((test? (car l) a) (rember-f test? a (cdr l)))
      (else (cons (car l) (rember-f test? a (cdr l))))
    )))

(define make-rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) l)
        ((test? (car l) a) (rember-f test? a (cdr l)))
        (else (cons (car l) (rember-f test? a (cdr l))))
      ))))

(define make-insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) old) 
          (cons new
            (cons old (cdr lat))))
        (else 
          (cons (car lat)
            ((make-insertL-f test?) new old (cdr lat))))
      ))))

(define make-insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) old) 
          (cons old
            (cons new (cdr lat))))
        (else 
          (cons (car lat)
            ((make-insertR-f test?) new old (cdr lat))))
      ))))

(define make-insert-g
  (lambda (test? seqI)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) old) 
          (seqI new lat))
        (else 
          (cons (car lat)
            ((make-insert-g test? seqI) 
                      new old (cdr lat))))
      ))))

(define seqR
  (lambda (new lat)
    (cons (car lat)
      (cons new (cdr lat)))))

(define seqL
  (lambda (new lat) 
    (cons new lat)))

(define seqS
  (lambda (new lat)
    (cons new (cdr lat))))

(define seqRem
  (lambda (new l)
    (cdr l)))

(define valueA
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else 
        ((atom-2-func (operator nexp))   ; first == called
           (valueA (1st-sub-exp nexp))
           (valueA (2nd-sub-exp nexp))))
    )))

(define atom-2-func
  (lambda (a)
    (cond
     ((eq? a (quote ++)) ++)
     ((eq? a (quote **)) **)
     ((eq? a (quote ^^)) ^^)
    )))


; ==============================
; Collectors

(define multirember&co 
  (lambda (a lat col) 
    (cond 
      ((null? lat) (col (quote ()) (quote ()))) 
      ((eq? (car lat) a) 
        (multirember&co a (cdr lat) 
          (lambda (newlat seen) 
            (col newlat (cons (car lat) seen)))))
      (else 
        (multirember&co a (cdr lat) 
          (lambda (newlat seen) 
            (col (cons (car lat) newlat) seen))))
    )))

(define a-friend
  (lambda (x y)
    (null? y)))

(define n~new-friend
  (lambda (newlat seen)
    (col newlat (cons (car lat) seen))))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons (quote tuna) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote and) newlat) seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define collect-friends
  (lambda (newlat seen)
    (cons newlat (cons seen (quote ())))))


(define n~multiinsertLR
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        (else
          (cond
           ((eq? (car lat) old)
            (cons new 
              (cons (car lat)
                (cons new
                  (n~multiinsertLR new old (cdr lat))))))
           (else 
            (cons (car lat)
              (n~multiinsertLR new old (cdr lat))))))
      )))

(define n~multiinsertLR&co
    (lambda (new old lat col)
      (cond
        ((null? lat) (quote ()))
        (else
          (cond
           ((eq? (car lat) old)
            (cons new 
              (cons (car lat)
                (cons new
                  (n~multiinsertLR new old (cdr lat)
                    (lambda (newlat seen) 
                      (col newlat (cons (car lat) seen)))
                  )))
            ))
           (else 
            (cons (car lat)
              (n~multiinsertLR new old (cdr lat)
                (lambda (newlat seen) 
                  (col (cons (car lat) newlat) seen)))
            ))
          ))
      )))


(define multiinsertR
    (lambda (new old lat)
        (cond
         ((null? lat) (quote ()))
         (else
            (cond
             ((eq? (car lat) old) 
              (cons (car lat)
                (cons new
                  (multiinsertR new old (cdr lat)))))
             (else 
              (cons (car lat)
                (multiinsertR new old (cdr lat)))))
         ))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) oldL) 
          (cons new
            (cons (car lat)
                (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR) 
          (cons (car lat)
            (cons new
                (multiinsertLR new oldL oldR (cdr lat)))))
          (else 
          (cons (car lat)
            (multiinsertLR new oldL oldR (cdr lat)))))
      ))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      (else
        (cond
          ((eq? (car lat) oldL) ;left
           (multiinsertLR&co new oldL oldR (cdr lat)
              (lambda (newlat nL nR)
                (col (cons new (cons oldL newlat)) (add1 nL) nR))))
          ((eq? (car lat) oldR)  ; right
           (multiinsertLR&co new oldL oldR (cdr lat)
              (lambda (newlat nL nR)
                (col (cons oldR (cons new newlat)) nL (add1 nR)))))
          (else 
           (multiinsertLR&co new oldL oldR (cdr lat)
              (lambda (newlat nL nR)
                (col (cons (car lat) newlat) nL nR))))))
    )))

(define first-col
  (lambda (newlat nL nR)
    newlat))

(define all-col
  (lambda (newlat nL nR)
    (cons (cons nL (cons nR (quote ()))) newlat)))

(define even?
  (lambda (n)
   (= (mod n 2) 0)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond 
          ((even? (car l))
            (cons (car l) (evens-only* (cdr l))))
          (else 
            (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l))))
    )))

(define evens-only*&co
  (lambda (l col)
    (cond 
      ((null? l) 
        (col (quote ()) 1 0))
      ((atom? (car l))
        (cond 
          ((even? (car l)) 
            (evens-only*&co (cdr l)
              (lambda (newl p s)
                (col (cons (car l) newl) (** p (car l)) s))))
          (else 
            (evens-only*&co (cdr l)
              (lambda (newl p s) 
                (col newl p (++ s (car l))))))))
      (else 
        (evens-only*&co (car l)             
          (lambda (al ap as)        
            (evens-only*&co (cdr l)        
              (lambda (bl bp bs)        
                (col (cons al bl) (** ap bp) (++ as bs)))))))
    )))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
      (cons product newl))))

;===============================

(define looking 
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)
  ))

(define keep-looking
  (lambda (a n lat) 
      (cond
        ((eq? (pick n lat) a) #t)
        ((number? (pick n lat)) 
            (keep-looking a (pick n lat) lat))
        (else #f)
      )))
      
(define shift
  (lambda (x)
    (build (first (first x)) 
      (build (second (first x)) (second x)))
  ))

(define align
  (trace-lambda align (pora)
    (cond 
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (align (shift pora)))
      (else (build (first pora) 
                   (align (second pora))))
     )))


(define length* 
 (lambda (pora)
  (cond 
    ((atom? pora) 1)
    (else 
      (+ (length* (first pora))
         (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond 
      ((atom? pora) 1)
      (else
        (+ (* (weight* (first pora)) 2)
           (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond 
      ((atom? pora) pora)
      ((a-pair? (first pora)) 
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora))))
    )))

(define C
  (trace-lambda C (n)
  (cond 
    ((one? n) 1)
    (else 
      (cond 
        ((even? n) (C (/ n 2)))
        (else (C (add1 (* 3 n))))
      ))
  )))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m))))
    )))

; ==============================
; ||      Y - combinator      ||
; ==============================

(define eternity
  (lambda (x)
    (eternity x)))

(define length 
  (lambda (l) 
    (cond 
      ((null? l) 0) 
      (else (add1 (length (cdr l)))))))

(define length_0 
  (lambda (l) 
    (cond 
      ((null? l) 0) 
      (else (add1 (eternity (cdr l)))))))

(define length_1+ 
  (lambda (l) 
    (cond 
      ((null? l) 0) 
      (else (add1 (length_0 (cdr l)))))))

(define length_1
  (lambda (l) 
    (cond 
      ((null? l) 0) 
      (else 
        (add1 
          (
            ; length_0
            (lambda (l) 
              (cond 
                ((null? l) 0) 
                (else (add1 
                        (eternity (cdr l)))))) 
             ; length_0            
                          (cdr l)))))))
; just like length 
((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
  eternity)
          
          ;1
 ((lambda (f) 
    (lambda (l)  ; 2
      (cond 
        ((null? l) 0)
                    ;1 
        (else (add1 (f (cdr l)))))))
  
  ((lambda (g)         ; 1
    (lambda (l) 
      (cond 
        ((null? l) 0) ;eternity
        (else ( add1 (g (cdr l)))))))
    eternity)) 


((lambda (mk-length) (mk-length mk-length)) ; 0
 (lambda (length)                           ; 1
  (lambda (l)                               ; 2
    (cond 
      ((null? l) 0) 
      (else (add1 (length (cdr l))))
    ))))

(define Y 
  (lambda (le) 
    ((lambda (f) (f f)) 
     (lambda (f)
      (le (lambda (x) ((f f) x)))))))

; ==============================
; ||          VALUE           ||
; ==============================


(define o~new-entry 
  (lambda (names values)
    (build names values)))

(define new-entry build)

(define lookup-in-entry 
  (lambda (name entry entry-f) 
    (lookup-in-entry-help name 
                          (first entry) 
                          (second entry) 
                          entry-f)
  ))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond 
      ((null? names) (entry-f name))
      ((null? values) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name 
                          (cdr names)
                          (cdr values)
                          entry-f))
    )))


(define extend-table cons)


(define o~lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      ((null? (lookup-in-entry 
                name (car table)
                (lambda (name) (quote ()))))
              (lookup-in-table  name (cdr table) table-f))
      (else (lookup-in-entry name (car table)
                (lambda (name) (quote ()))))
  
    )))


(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                (lambda (name)
                  (lookup-in-table 
                    name 
                    (cdr table) 
                    table-f))
            ))
  
    )))

(define expression-2-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-2-action e))
      (else (list-2-action e))
    )))

(define atom-2-action
  (lambda (a)
    (cond 
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const) 
      ((eq? e (quote cdr)) *const) 
      ((eq? e (quote null?)) *const) 
      ((eq? e (quote eq?)) *const) 
      ((eq? e (quote atom?)) *const) 
      ((eq? e (quote zero?)) *const) 
      ((eq? e (quote add1)) *const) 
      ((eq? e (quote sub1)) *const) 
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-2-action
  (lambda (l)
    (cond
      (atom? (car l) 
        (cond 
          ((eq? (car l) (quote quote)) *quote)
          ((eq? (car l) (quote lambda)) *lambda)
          ((eq? (car l) (quote cond)) *cond)
          (else *application)))
      (else *application)    
    )))

(define values
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-2-action e) e table)))


(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else 
        (build (quote primitive) e))
     )))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)


(define *identifier
  (lambda (e table)
    lookup-in-table e table intial-table))

(define intial-table
  (lambda (name) 
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
      (cons table (cdr e)))))

