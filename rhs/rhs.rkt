#lang racket

#|

Port of the rhs library to Racket used by scos and rsc3.

Written by Rohan Drape (http://rd.slavepianos.org/), © 2008-2012

http://rd.slavepianos.org/

Licensed under GPL (2 or 3? FIXME)


|#

(require rnrs)

(provide (all-defined-out))


;; to fix rnrs compatibility

#|
(define exact inexact->exact)

(define inexact exact->inexact)

(define mod remainder)
|#

;; JBC, 2014-- looks like most of these library functions have
;; equivalents in Racket....


;; prelude.scm ;;;;;;;;;;;;;;;;;;;;;;

;; enumFromThenTo :: a -> a -> a -> [a]
(define enum-from-then-to
  (letrec ((efdt
            (lambda (f i x k)
              (cond ((= i k) (list1 k))
                    ((f i k) null)
                    (else (cons i (efdt f (+ i x) x k)))))))
    (lambda (i j k)
      (let ((x (- j i)))
        (efdt (if (> x 0) > <) i x k)))))

;; enumFromTo :: a -> a -> [a]
(define enum-from-to
  (lambda (i j)
    (enum-from-then-to i (succ i) j)))

;; even :: (Integral a) => a -> Bool
(define even
  even?)

;; odd :: (Integral a) => a -> Bool
(define odd
  odd?)

;; pred :: a -> a
(define pred
  (lambda (x)
    (- x 1)))

;; signum :: Num a => a -> a
(define signum
  (lambda (x)
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (else 0))))

;; succ :: a -> a
(define succ
  (lambda (x)
    (+ x 1)))

;; undefined :: a
(define undefined
  (lambda ()
    (error "undefined" "undefined")))



;; tuple.scm ;;;;;;;;;;;;


;; curry :: ((a, b) -> c) -> a -> b -> c
(define curry
  (lambda (f)
    (lambda (x y)
      (f (tuple2 x y)))))

(struct duple (p q))

;; fst :: (a, b) -> a
(define fst
  duple-p)

;; snd :: (a, b) -> b
(define snd
  duple-q)

;; (,) :: a -> b -> (a, b)
(define tuple2
  duple)

;; uncurry :: (a -> b -> c) -> (a, b) -> c
(define uncurry
  (lambda (f)
    (lambda (xy)
      (f (fst xy) (snd xy)))))


;; data/ord.scm ;;;;;;;;;;;;;;;;


;; data Ordering = LT | EQ | GT

;; compare :: (Ord a) => a -> a -> Ordering
(define compare
  (lambda (x y)
    (cond ((> x y) 'gt)
          ((< x y) 'lt)
          (else 'eq))))

;; max :: a -> a -> a
(define max2
  (lambda (x y)
    (if (> x y) x y)))

;; min :: a -> a -> a
(define min2
  (lambda (x y)
    (if (< x y) x y)))



;; data/function.scm ;;;;;;;;;;;;;;;;;;


;; (.) :: (b -> c) -> (a -> b) -> a -> c
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; const :: a -> b -> a
(define const
  (lambda (x)
    (lambda (_)
      x)))

;; flip :: (a -> b -> c) -> b -> a -> c
(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

;; id :: a -> a
(define id
  (lambda (x)
    x))



;; data/list.scm ;;;;;;;;;;;;;;;;;;;;;;

;; all :: (a -> Bool) -> [a] -> Bool
(define all
  (lambda (f l)
    (if (null? l)
        #t
        (and (f (head l)) (all f (tail l))))))

;; and :: [Bool] -> Bool
(define all-true
  (lambda (l)
    (if (null? l)
        #t
        (and (head l) (all-true (tail l))))))

;; any :: (a -> Bool) -> [a] -> Bool
(define any
  (lambda (f l)
    (if (null? l)
        #f
        (or (f (head l)) (any f (tail l))))))

;; (++) :: [a] -> [a] -> [a]
(define append2
  (lambda (a b)
    (if (null? a)
        b
        (cons (head a) (append2 (tail a) b)))))

;; break :: (a -> Bool) -> [a] -> ([a],[a])
(define break
  (lambda (p l)
    (span (compose not p) l)))

;; concat :: [[a]] -> [a]
(define concat
  (lambda (l)
    (foldr append2 nil l)))

;; concatMap :: (a -> [b]) -> [a] -> [b]
(define concat-map
  (lambda (f l)
    (concat (map1 f l))))

;; deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
(define delete-by
  (lambda (f x l)
    (if (null? l)
        nil
        (if (f x (head l))
            (tail l)
            (cons (head l) (delete-by f x (tail l)))))))

;; delete :: (Eq a) => a -> [a] -> [a]
(define delete
  (lambda (x l)
    (delete-by equal? x l)))

;; drop :: Int -> [a] -> [a]
(define drop
  (lambda (n l)
    (cond ((<= n 0) l)
          ((null? l) nil)
          (else (drop (- n 1) (tail l))))))

;; dropWhile :: (a -> Bool) -> [a] -> [a]
(define drop-while
  (lambda (p l)
    (if (null? l)
        nil
        (if (p (head l))
            (drop-while p (tail l))
            l))))

;; elem :: (Eq a) => a -> [a] -> Bool
(define elem
  (lambda (x l)
    (any (lambda (y) (equal? x y)) l)))

;; elemIndex :: Eq a => a -> [a] -> Maybe Int
(define elem-index
  (lambda (x l)
    (find-index (lambda (y) (equal? x y)) l)))

;; elemIndices :: Eq a => a -> [a] -> [Int]
(define elem-indices
  (lambda (x l)
    (find-indices (lambda (y) (equal? x y)) l)))

;; find :: (a -> Bool) -> [a] -> Maybe a
#;(define find
  (lambda (f l)
    (if (null? l)
        #f
        (if (f (head l))
            (head l)
            (find f (tail l))))))


;; findIndex :: (a -> Bool) -> [a] -> Maybe Int
(define find-index
  (letrec ((g (lambda (f l n)
                (if (null? l)
                    #f
                    (if (f (head l))
                        n
                        (g f (tail l) (+ n 1)))))))
    (lambda (f l)
      (g f l 0))))

;; findIndices :: (a -> Bool) -> [a] -> [Int]
(define find-indices
  (letrec ((g (lambda (f l n)
                (if (null? l)
                    nil
                    (if (f (head l))
                        (cons n (g f (tail l) (+ n 1)))
                        (g f (tail l) (+ n 1)))))))
    (lambda (f l)
      (g f l 0))))

;; filter :: (a -> Bool) -> [a] -> [a]
#;(define filter
  (lambda (f l)
    (if (null? l)
        nil
        (let ((x (head l))
              (xs (tail l)))
          (if (f x)
              (cons x (filter f xs))
              (filter f xs))))))

;; foldl :: (a -> b -> a) -> a -> [b] -> a
(define foldl
  (lambda (f z l)
    (if (null? l)
        z
        (foldl f (f z (head l)) (tail l)))))

;; foldl1 :: (a -> a -> a) -> [a] -> a
(define foldl1
  (lambda (f l)
    (foldl f (head l) (tail l))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
        z
        (f (head l) (foldr f z (tail l))))))

;; foldr1 :: (a -> a -> a) -> [a] -> a
(define foldr1
  (lambda (f l)
    (if (null? (tail l))
        (head l)
        (f (head l) (foldr1 f (tail l))))))

;; groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
(define group-by
  (lambda (f l)
    (if (null? l)
        (list)
        (let* ((x (car l))
               (yz (span (lambda (e) (f e x)) (cdr l))))
          (cons (cons x (fst yz)) (group-by f (snd yz)))))))

;; head :: [a] -> a
(define head car)

;; init :: [a] -> [a]
(define init
  (lambda (l)
    (let ((x (head l))
          (xs (tail l)))
      (if (null? xs)
          nil
          (cons x (init xs))))))

;; insert :: Ord a => a -> [a] -> [a]
(define insert
  (lambda (e l)
    (insert-by compare e l)))

;; insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
(define insert-by
  (lambda (f x l)
    (if (null? l)
        (list1 x)
        (if (equal? (f x (head l)) 'gt)
            (cons (head l) (insert-by f x (tail l)))
            (cons x l)))))

;; intercalate :: [a] -> [[a]] -> [a]
(define intercalate
  (lambda (xs xss)
    (concat (intersperse xs xss))))

;; intersperse :: a -> [a] -> [a]
(define intersperse
  (lambda (x l)
    (cond ((null? l) nil)
          ((null? (tail l)) l)
          (else (cons (head l) (cons x (intersperse x (tail l))))))))

;; isInfixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-infix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (or (is-prefix-of p q)
                     (is-infix-of p (tail q)))))))

;; isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-prefix-of
  (lambda (p q)
    (cond ((null? p) #t)
          ((null? q) #f)
          (else (and (equal? (head p) (head q))
                      (is-prefix-of (tail p) (tail q)))))))

;; isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
(define is-suffix-of
  (lambda (p q)
    (is-prefix-of (reverse p) (reverse q))))

;; last :: [a] -> a
(define last
  (lambda (l)
    (let ((xs (tail l)))
      (if (null? xs)
          (head l)
          (last xs)))))

;; mlength :: [a] -> Int
(define mlength
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (tail l))))))



;; list1 :: a -> [a]
(define list1
  (lambda (x)
    (cons x nil)))

;; list2 :: a -> a -> [a]
(define list2
  (lambda (x y)
    (cons x (cons y nil))))

;; list3 :: a -> a -> a -> [a]
(define list3
  (lambda (x y z)
    (cons x (cons y (cons z nil)))))

;; list4 :: a -> a -> a -> a -> [a]
(define list4
  (lambda (x y z a)
    (cons x (cons y (cons z (cons a nil))))))

;; list5 :: a -> a -> a -> a -> a -> [a]
(define list5
  (lambda (x y z a b)
    (cons x (cons y (cons z (cons a (cons b nil)))))))

;; (!!) :: [a] -> Int -> a
#;(define list-ref
  (lambda (l n)
    (if (= n 0)
        (head l)
        (list-ref (tail l) (- n 1)))))

;; lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
(define lookup
  (lambda (x l)
    (if (null? l)
        #f
        (if (equal? (fst (head l)) x)
            (snd (head l))
            (lookup x (tail l))))))

;; map :: (a -> b) -> [a] -> [b]
(define map1 map)
#;(define map1
  (lambda (f l)
    (if (null? l)
        nil
        (cons (f (head l)) (map1 f (tail l))))))

;; mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-l
  (lambda (f s l)
    (if (null? l)
        (tuple2 s nil)
        (let* ((x (head l))
               (xs (tail l))
               (s_y (f s x))
               (s_ (fst s_y))
               (y (snd s_y))
               (s__ys (map-accum-l f s_ xs))
               (s__ (fst s__ys))
               (ys (snd s__ys)))
          (tuple2 s__ (cons y ys))))))

;; mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
(define map-accum-r
  (lambda (f s l)
    (if (null? l)
        (tuple2 s nil)
        (let* ((x (head l))
               (xs (tail l))
               (s_ys (map-accum-r f s xs))
               (s_ (fst s_ys))
               (ys (snd s_ys))
               (s__y (f s_ x))
               (s__ (fst s__y))
               (y (snd s__y)))
          (tuple2 s__ (cons y ys))))))

;; maximum :: (Ord a) => [a] -> a
(define maximum
  (lambda (l)
    (foldl1 max2 l)))

;; minimum :: (Ord a) => [a] -> a
(define minimum
  (lambda (l)
    (foldl1 min2 l)))

;; nub :: (Eq a) => [a] -> [a]
(define nub
  (lambda (l)
    (nub-by equal? l)))

;; nubBy :: (a -> a -> Bool) -> [a] -> [a]
(define nub-by
  (lambda (f l)
    (if (null? l)
        nil
        (let ((x (head l))
              (xs (tail l)))
          (cons x (nub-by f (filter (lambda (y) (not (f x y))) xs)))))))

;; nil :: [a]
(define nil
  (list))

;; notElem :: (Eq a) => a -> [a] -> Bool
(define not-elem
  (lambda (x l)
    (all (lambda (y) (not (equal? x y))) l)))

;; null :: [a] -> Bool
#;(define null?
  (lambda (x)
    (equal? x nil)))

;; or :: [Bool] -> Bool
(define any-true
  (lambda (l)
    (if (null? l)
        #f
        (or (head l) (any-true (tail l))))))

;; partition :: (a -> Bool) -> [a] -> ([a], [a])
(define partition*
  (let ((select (lambda (p)
                  (lambda (x tf)
                    (let ((t (fst tf))
                          (f (snd tf)))
                      (if (p x)
                          (tuple2 (cons x t) f)
                          (tuple2 t (cons x f))))))))
    (lambda (p xs)
      (foldr (select p) (tuple2 nil nil) xs))))

;; product :: (Num a) => [a] -> a
(define product
  (lambda (l)
    (foldl * 1 l)))

;; replicate :: Int -> a -> [a]
(define replicate
  (lambda (n x)
    (if (= n 0)
        nil
        (cons x (replicate (- n 1) x)))))

;; reverse :: [a] -> [a]
#;(define reverse
  (lambda (l)
    (foldl (flip cons) nil l)))

;; scanl :: (a -> b -> a) -> a -> [b] -> [a]
(define scanl
  (lambda (f q l)
    (cons q (if (null? l)
                nil
                (scanl f (f q (head l)) (tail l))))))

;; scanl1 :: (a -> a -> a) -> [a] -> [a]
(define scanl1
  (lambda (f l)
    (if (null? l)
        nil
        (scanl f (head l) (tail l)))))

;; scanr :: (a -> b -> b) -> b -> [a] -> [b]
(define scanr
  (lambda (f q0 l)
    (if (null? l)
        (list1 q0)
        (let ((qs (scanr f q0 (tail l))))
          (cons (f (head l) (head qs)) qs)))))

;; scanr1 :: (a -> a -> a) -> [a] -> [a]
(define scanr1
  (lambda (f l)
    (if (null? l)
        nil
        (if (null? (tail l))
            l
            (let ((qs (scanr1 f (tail l))))
              (cons (f (head l) (head qs)) qs))))))

;; sort :: (Ord a) => [a] -> [a]
(define sort
  (lambda (l)
    (sort-by compare l)))

;; sortBy :: (a -> a -> Ordering) -> [a] -> [a]
(define sort-by
  (lambda (f l)
    (mergesort f l)))

;; mergesort :: (a -> a -> Ordering) -> [a] -> [a]
(define mergesort
  (lambda (f l)
    (mergesort* f (map1 list1 l))))

;; mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
(define mergesort*
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (tail l)) (head l))
          (else (mergesort* f (merge-pairs f l))))))

;; merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
(define merge-pairs
  (lambda (f l)
    (cond ((null? l) nil)
          ((null? (tail l)) l)
          (else (cons (merge f (head l) (head (tail l)))
                      (merge-pairs f (tail (tail l))))))))

;; merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
(define merge
  (lambda (f l r)
    (cond ((null? l) r)
          ((null? r) l)
          (else (if (equal? (f (head l) (head r)) 'gt)
                    (cons (head r) (merge f l (tail r)))
                    (cons (head l) (merge f (tail l) r)))))))

;; span :: (a -> Bool) -> [a] -> ([a],[a])
(define span
  (lambda (p l)
    (if (null? l)
        (tuple2 nil nil)
        (if (p (head l))
            (let ((r (span p (tail l))))
              (tuple2 (cons (head l) (fst r)) (snd r)))
            (tuple2 nil l)))))

;; splitAt :: Int -> [a] -> ([a],[a])
(define split-at
  (lambda (n l)
    (tuple2 (take n l) (drop n l))))

;; sum :: (Num a) => [a] -> a
(define sum
  (lambda (l)
    (foldl + 0 l)))

;; tail :: [a] -> [a]
(define tail cdr)

;; take :: Int -> [a] -> [a]
(define take
  (lambda (n l)
    (cond ((<= n 0) nil)
          ((null? l) nil)
          (else (cons (head l) (take (- n 1) (tail l)))))))

;; takeWhile :: (a -> Bool) -> [a] -> [a]
(define take-while
  (lambda (p l)
    (if (null? l)
        nil
        (if (p (head l))
            (cons (head l) (take-while p (tail l)))
            nil))))

;; transpose :: [[a]] -> [[a]]
(define transpose
  (lambda (l)
    (let ((protect
           (lambda (f)
             (lambda (x)
               (if (null? x)
                   nil
                   (f x))))))
      (cond ((null? l) nil)
            ((null? (head l)) (transpose (tail l)))
            (else (let* ((e (head l))
                         (x (head e))
                         (xs (tail e))
                         (xss (tail l)))
                    (cons (cons x
                                (filter (compose not null?)
                                        (map1 (protect head) xss)))
                          (transpose (cons xs
                                           (map1 (protect tail) xss))))))))))

;; unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
(define unfoldr
  (lambda (f x)
    (let ((r (f x)))
      (if r
          (cons (fst r) (unfoldr f (snd r)))
          nil))))

;; (unfoldr (lambda (b) (if (= b 0) #f (tuple2 b (- b 1)))) 10)
;; => (10 9 8 7 6 5 4 3 2 1)

;; union :: (Eq a) => [a] -> [a] -> [a]
(define union
  (lambda (a b)
    (union-by equal? a b)))

;; unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
(define union-by
  (lambda (f xs ys)
    (let ((g (lambda (x y) (delete-by f y x))))
      (append2 xs (foldl g (nub-by f ys) xs)))))

;; zip :: [a] -> [b] -> [(a, b)]
(define zip
  (lambda (a b)
    (zip-with tuple2 a b)))

;; zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
(define zip-with
  (lambda (f a b)
    (cond ((null? a) nil)
          ((null? b) nil)
          (else (cons (f (head a) (head b))
                      (zip-with f (tail a) (tail b)))))))

;; zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(define zip-with3
  (lambda (f a b c)
    (cond ((null? a) nil)
          ((null? b) nil)
          ((null? c) nil)
          (else (cons (f (head a) (head b) (head c))
                      (zip-with3 f (tail a) (tail b) (tail c)))))))



;; control/monad.scm ;;;;;;;;;;;;;;;;;;;;

;; replicateM :: (Monad m) => Int -> m a -> m [a]
(define-syntax replicate-m
  (syntax-rules ()
    ((_ i x)
     (replicate-m* i (lambda () x)))))

;; int -> (() -> a) -> [a]
(define replicate-m*
  (lambda (i x)
    (if (<= i 0)
        nil
        (cons (x) (replicate-m* (- i 1) x)))))


;; data/tree.scm ;;;;;;;;;;;;;;

;; Tree a -> [a]
(define flatten
  (letrec ((f (lambda (t r)
		(cond ((null? t) r)
		      ((pair? t) (f (head t) (f (tail t) r)))
		      (else (cons t r))))))
    (lambda (t)
      (f t nil))))

;; Tree a -> [[a]]
(define levels
  (lambda (t)
    (if (null? t)
	nil
	(let ((lr (partition* (compose not pair?) t)))
	  (cons (fst lr) (levels (concat (snd lr))))))))

