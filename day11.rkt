#lang racket

(define SIZE 10);

(define (map-map f lst) ; map . map
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define (char->num chr) ; i hate this
  (second (assoc chr (list '(#\0 0) '(#\1 1) '(#\2 2) '(#\3 3) '(#\4 4) '(#\5 5) '(#\6 6) '(#\7 7) '(#\8 8) '(#\9 9)))))

(define (subtract l1 l2) ; retun list of items that are only in l1
  (filter (λ (x) (not (member x l2))) l1))

(define grid ; file -> flat vector of the grid digits
  (list->vector
   (flatten
    (map-map char->num
             (map string->list
                  (file->lines "day11.txt"))))))

(define (display-grid grid)
  (for-each (λ (i)
              (printf "~s " (vector-ref grid i))
              (if (= 0 (modulo (add1 i) SIZE)) (printf "~n") (printf "")))
            (range 0 (* SIZE SIZE))))

(define (neighbors idx) ; find all the neighboring indicies, filtering out out-of-bounds
  (filter (λ (idx) (and (>= idx 0) (< idx (* SIZE SIZE)))) ; remove ones off the top or bottom of board
          (filter identity ; remove #f
                  (flatten (list (if (= (modulo idx SIZE) 0) #f ; if it's on the left edge, #f
                                     (list (- idx (add1 SIZE)) (+ idx (sub1 SIZE)) (sub1 idx)))
                                 (if (= (modulo (add1 idx) SIZE) 0) #f ; if it's on the right edge, #f
                                     (list (- idx (sub1 SIZE)) (+ idx (add1 SIZE)) (add1 idx)))
                                 (list (- idx SIZE) (+ idx SIZE)))))))

(define (flash grid tens seent flash-count) ; recursively flash tens
  (for-each (λ (i) (hash-set! seent i #t)) tens) ; mark these tens as having been seen
  (let* ([ns (flatten (map neighbors tens))] ; find all the neighbors of the tens
         [ns-filtered (filter (λ (i) (not (hash-has-key? seent i))) ns)] ; filter out the ones we've flashed
         [new-flash-count (+ flash-count (length tens))])
    (for-each (λ (i) (vector-set! grid i (add1 (vector-ref grid i)))) ns-filtered) ; add 1 to all the neighbors
    (let* ([new-tens (remove-duplicates (filter (λ (x) (<= 10 (vector-ref grid x))) ns-filtered))])
      (if (< 0 (length new-tens))
          (flash grid new-tens seent new-flash-count) ; if so, flash em
          (list new-flash-count (vector-map! (λ (x) (if (>= x 10) 0 x)) grid)))))) ; otherwise set our 10s to 0 and return the vector

(define (vector-find vect proc) ; returns list of indicies where val is found in vect
  (foldr (λ (i acc) (if (proc (vector-ref vect i)) (cons i acc) acc)) '() (range 0 (* SIZE SIZE))))

(define (step grid) ; grid is actually '(count grid)....
  (let* ([g (second grid)]
         [flash-count (first grid)]
         [incd (vector-map add1 g)]
         [seent (make-hash)]
         [tens (vector-find incd (λ (n) (>= n 10)))]
         [res (flash incd tens seent flash-count)])
    res))

(define part1
  (let loop ([n 100] [g grid] [flash-count 0])
    (if (= n 0)
        (list g flash-count)
        (let* ([res (step (list flash-count g))]
               [new-count (first res)]
               [new-grid (second res)])
          (loop (sub1 n) new-grid new-count)))))

part1
(display-grid (first part1))

(define (all-flashing? grid)
  (= 100
     (vector-length (vector-filter (λ (x) (= x 0)) grid))))

(define part2
  (+ 1 (let loop ([n 0] [g grid] [flash-count 0])
    (let* ([res (step (list flash-count g))]
           [new-count (first res)]
           [new-grid (second res)])
      (if (all-flashing? new-grid)
          n
          (loop (add1 n) new-grid new-count))))))

part2