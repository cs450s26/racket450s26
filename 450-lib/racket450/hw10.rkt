#lang racket

(provide NaN
         NaN?
         undefined-var-err?
         undefined-var-err
         not-fn-err
         not-fn-err?
         exn:fail:cs450:broadcast?
         exn:fail:syntax:cs450
         exn:fail:syntax:cs450?
         Index? ; remove?
         hw10-slice
         (rename-out
          [Atom? hw10-Atom?]
          [ErrorResult? hw10-ErrorResult?]
          [undefined-var-err mk-undefined-var-err]
          [not-fn-err mk-not-fn-err]
          [Array? hw10-Array?]
          [mk-Slice/testing hw10-mk-Slice]
          [Slice? hw10-Slice?]
          [mk-array-op hw10-mk-array-op]))

;; An Atom is one of:
;; - Number
;; - Boolean
;; - String
;; - NaN

(struct 450nan [])
(define NaN (450nan))
(define (NaN? x) (equal? x NaN))
;; Note: nan? is built-in racketfn, where input must be real?, so use different name

;; Note: Atom (no procedure or ErrorResult) is different from Result
(define (Atom? x)
  ((disjoin number?
            string?
            boolean?
            NaN?)
   x))

;; 450syntax errors
(struct exn:fail:syntax:cs450 exn:fail:syntax [])

;; An ErrorResult is one of
;; - ERROR-RESULT
;; - UNDEFINED-ERROR
;; - ARITY-ERROR
;; - CIRCULAR-ERROR
(struct ErrorResult [] #:transparent)
(struct arity-err ErrorResult [] #:transparent)
(struct undefined-var-err ErrorResult [name] #:transparent)
(struct not-fn-err ErrorResult [val] #:transparent)
(struct circular-err ErrorResult [val] #:transparent)
(define ERROR-RESULT (ErrorResult))
(define UNDEFINED-ERROR (undefined-var-err 'unknown))
(define ARITY-ERROR (arity-err))
(define NOT-FN-ERROR (not-fn-err 'unknown))
(define CIRCULAR-ERROR (circular-err 'unknown))

;; An Array is an:
;; - Atom
;; - ArrayList
;; Represents: multidimensional data similar to NumPy array
;; invariant: is rectangular, ie not jagged

;; An ArrayList is one of
;; - empty
;; - (cons Array ArrayList)

(define (Array? x)
  (or (Atom? x)
      (ArrayList? x)))

(define (ArrayList? x)
  (list? x))

;; An Index is one of:
;; - nonneg int
;; - neg int
;; Represents:
;; - nonneg: 0-based index for Array,
;; - neg: counts from the back where -1 = last element

(define (PosIndex? x) (exact-nonnegative-integer? x))

(define (Index? x)
  (and (integer? x)
       (or (negative? x)
           (PosIndex? x))))

;; neg step value unsupported, for now
(define (Step? x) (exact-nonnegative-integer? x))

;; A Dim is an exact-nonnegative-int
(define (Dim? x) (exact-nonnegative-integer? x))

;; Array -> exact-nonneg-int
(define (ndim a)
  (cond
    [(Atom? a) 0]
    [else (ndim-alst a)]))

;; arraylist -> exact nonneg int
(define (ndim-alst a)
  (cond
    [(empty? a) 1]
    [else (+ 1 (ndim (first a)))]))

(define/contract (shape a)
  (-> Array? (listof Dim?))
  (cond
    [(Atom? a) empty]
    [else (shape-alst a)]))
(define/contract (shape-alst a)
  (-> ArrayList? (listof Dim?))
  (cond
    [(empty? a) (list 0)]
    [else
     (cons (length a) (shape (first a)))]))

(struct exn:fail:cs450:broadcast exn:fail [])

;; mk-array-op
;; Creates a mk-array-op fn, given a function for Array elements
(define/contract ((mk-array-op array-elt-op) a1 a2)
  (-> (-> Atom? Atom? Atom?) (-> Array? Array? Array?))
  ((curry array-op array-elt-op) a1 a2))

;; array-op
;; Applies the given Array element function to the two given Arrays if they are broadcastable
(define/contract (array-op array-elt-op l-arr r-arr)
  (-> (-> Atom? Atom? Atom?) Array? Array? Array?)
  (array-op/compatible array-elt-op (broadcast l-arr r-arr) (broadcast r-arr l-arr)))

;; array-op/compatible
;; Applies the given Array element function to the two given Arrays
;; Invariant: Arrays are of compatible shape
(define/contract (array-op/compatible array-elt-op l-arr r-arr)
  (-> (-> Atom? Atom? Atom?) Array? Array? Array?)
  (match* (l-arr r-arr)
    [((? Atom? l) (? Atom? r)) (array-elt-op l r)]
    [((? Atom? l) (? ArrayList? r)) (arraylist-op/atom array-elt-op r l)]
    [((? ArrayList? l) (? Atom? r)) (arraylist-op/atom array-elt-op l r)]
    [((? ArrayList? l) (? ArrayList? r)) (arraylist-op array-elt-op l r)]))

;; arraylist-op/atom
;; Applies the given Array element operation to the given Atom and ArrayList
(define/contract (arraylist-op/atom array-elt-op al atom)
  (-> (-> Atom? Atom? Atom?) ArrayList? Atom? ArrayList?)
  (map (curry array-op/compatible array-elt-op atom) al))

;; arraylist-op
;; Applies the given Array element operation to the two given ArrayLists
;; Invariant: ArrayLists are of compatible shape
(define/contract (arraylist-op array-elt-op l-al r-al)
  (-> (-> Atom? Atom? Atom?) ArrayList? ArrayList? ArrayList?)
  (cond
    [(and (empty? l-al) (empty? r-al)) empty]
    [else
     (match* ((length l-al) (length r-al))
       [(e e) (map (curry array-op/compatible array-elt-op) l-al r-al)]
       [(1 _) (map (curry array-op/compatible array-elt-op (first l-al)) r-al)]
       [(_ 1) (map (curry array-op/compatible array-elt-op (first r-al)) l-al)])]))

;; broadcast
;; Broadcasts from-arr to a shape compatible with to-arr
(define/contract (broadcast from-arr to-arr)
  (-> Array? Array? Array?)
  ;; broadcast/a
  ;; from: the reverse shape of the from-arr so far
  ;; to: the reverse shape of the to-arr so far
  (define/contract (broadcast/a b-arr from to)
    (-> Array? (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) Array?)
    (cond
      [(and (empty? from) (empty? to)) b-arr]
      [(and (cons? from) (empty? to)) b-arr]
      [(and (empty? from) (cons? to)) (broadcast/a (list b-arr) empty (rest to))]
      [else
       (if (valid-broadcast? (first from) (first to))
           (broadcast/a b-arr (rest from) (rest to))
           (raise (exn:fail:cs450:broadcast
                   (format "ValueError: operands could not be broadcast together with shapes ~a ~a"
                           (shape from-arr)
                           (shape to-arr))
                   (current-continuation-marks))))]))
  (broadcast/a from-arr (reverse (shape from-arr)) (reverse (shape to-arr))))

;; valid-broadcast?
;; Determines whether the two given ArrayList lengths are broadcastable
(define/contract (valid-broadcast? l-len r-len)
  (-> exact-nonnegative-integer? exact-nonnegative-integer? boolean?)
  (or (equal? l-len 1) (equal? r-len 1) (equal? l-len r-len)))

(define/contract (ref/flat a . indices)
  (->* (Array?) () #:rest (listof Index?) Array?)
  (cond
    [(empty? indices) a]
    [else
     (apply ref/flat
            (list-ref a (index-fn (first indices) (length a)))
            (rest indices))]))

;; for inclusive, ie non-stop indices
;; n is max exclusive value of i, i.e., length of outer Array dim
;; better name? nonneg-idx
(define/contract (index-fn i n)
  (-> Index? exact-nonnegative-integer? PosIndex?)
  (cond
    [(exact-nonnegative-integer? i) i]
    [(negative? i) (+ n i)]))

;; a Slice is a
;; index
;; (mk-Slice start MaybeStop step)
;; - start and stop can be neg, step cannot
;; - start is inclusive, stop is exclusive
;; Represents: Slice range
(define (Slice? x)
  (or (Index? x) (SliceRange? x)))

;; A MaybeStop is either #f or an Index (exclusive)
(define (MaybeStop? x) (or (false? x) (Index? x)))

(struct SliceRange [i j step] #:transparent)
;; careful! stop is exclusive, so = -1 is not the same as #f
(define/contract (mk-Slice i [j #f] #:step [step 1])
  (->* (Index?) (MaybeStop? #:step Step?) Slice?)
  (SliceRange i j step))

(define/contract (mk-Slice/testing #:start [start 0]
                                   #:stop [stop #f]
                                   #:step [step 1])
  (->* () (#:start Index? #:stop MaybeStop? #:step Step?) Slice?)
  (mk-Slice start stop #:step step))

(define/contract (hw10-slice a slices)
  (-> Array? (listof Slice?) Array?)
  (apply slice a slices))

(define/contract (slice a . slices)
  (->* (Array?) () #:rest (listof Slice?) Array?)
  (cond
    [(empty? slices) a]
    [else
     (define slice-res
       (slice1 a (first slices)))
     (if (<= (ndim slice-res) 1)
         (apply slice slice-res (rest slices))
         (map
          (lambda (aa)
            (apply slice aa (rest slices)))
          slice-res))]))

;; sets Stop index to len if #f or > len
(define/contract (adjust-stop j len)
  (-> MaybeStop? exact-nonnegative-integer? Index?)
  (cond
    [(false? j) len]
    [else (if (> j len) len j)]))

(define/contract (slice1 a s)
  (-> Array? Slice? Array?)
  (match s
    [(? number? s) (ref/flat a s)]
    [(SliceRange i j step)
     (get-slice a i (adjust-stop j (length a)) #:step step)]))

;; get-slice : arraylist
(define/contract (get-slice a i [j (length a)] #:step [step 1])
  (->* (ArrayList? Index?) (Index? #:step Step?) ArrayList?)
  (define pos-i (index-fn i (length a)))
  (define pos-j (index-fn j (length a)))
  (define dropped (drop a pos-i))
  (define taked (take dropped (- pos-j pos-i)))
  (filter/step taked step))

;; filters list such that only every "step" elements remain
;; first element is always kept
(define/contract (filter/step lst step)
  (-> list? Step? list?)

  ;; accumulator curr-step, represents num elts left to skip
  ;; invariant: curr-step < step
  (define/contract (filter-step/a lst curr-step step)
    (-> list? Step? Step? list?)
    (cond
      [(empty? lst) lst]
      [else
       (if (= curr-step 1)
           (cons (first lst) (filter-step/a (rest lst) step step))
           (filter-step/a (rest lst) (sub1 curr-step) step))]))

  (filter-step/a lst 1 step))

(module+ test
  (require rackunit)
  ;; check auto array stop truncate
  (check-equal?
   (hw10-slice '[[1 2 3 4] [5 6 7 8]]
              (list
               (mk-Slice/testing #:start 0 #:stop 3)))
   '[[1 2 3 4]
     [5 6 7 8]])
  
  ;; check auto array stop truncate
  (check-equal?
   (hw10-slice '[[1 2 3 4] [5 6 7 8]]
              (list
               (mk-Slice/testing #:start 0 #:stop 3 #:step 2)))
   '[[1 2 3 4]]))
