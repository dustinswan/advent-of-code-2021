#lang racket

(define (map-map f lst) ; map . map
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define (char->num chr) ; i hate this
  (second (assoc chr (list '(#\0 0) '(#\1 1) '(#\2 2) '(#\3 3) '(#\4 4) '(#\5 5) '(#\6 6) '(#\7 7) '(#\8 8) '(#\9 9)))))

(define grid ; file -> list of lists of numbers
  (map-map char->num
           (map string->list
                (file->lines "day9.txt"))))

(define (grid-value grid row col) ; value of grid at (row, col)
  (if (or (< row 0) (> row (sub1 (length grid))) (< col 0) (> col (sub1 (length (first grid))))) ; if out of bounds
      #f
      (list-ref (list-ref grid row) col)))

(define (neighbors grid row col) ; list of values of neighbors to (row, col)
  (filter identity ; filter out #f
          (list (grid-value grid row (sub1 col))
                (grid-value grid row (add1 col))
                (grid-value grid (sub1 row) col)
                (grid-value grid (add1 row) col))))
  
(define (is-lowest? grid row col) ; is (row, col) a low point
  (let ([ns (neighbors grid row col)]
        [val (grid-value grid row col)])
    (foldl (λ (n acc) (and (< val n) acc)) #t ns)))

(define (low-point-coords grid) ; list of all low point coordinates in the grid
  (let ([width (length (first grid))]
        [height (length grid)])
    (apply append ; flatten the list one level
     (for/list ([row (range 0 height)]) ; using a for. i caved
       (filter identity ; filter out #f
               (for/list ([col (range 0 width)])
                 (if (is-lowest? grid row col)
                     (list row col)
                     #f)))))))

(define (low-point-values grid) ; the values of all of the low points in a grid
  (let* ([points (low-point-coords grid)])
    (map (λ (p) (grid-value grid (first p) (second p))) points)))

(define part1
  (let* ([points (low-point-values grid)]
         [risk-levels (map add1 points)])
    (foldl + 0 risk-levels)))

part1

; PART 2

(define (neighbors-coords row col) ; list of _coordinates_ of neighbors to (row, col)
  (list
   (list row (sub1 col))
   (list row (add1 col))
   (list (sub1 row) col)
   (list (add1 row) col)))

(define (find-basin-around-point grid row col)
  (define visited (make-hash))
  (let loop ([r row] [c col])
    (let* ([ns (neighbors-coords r c)] ; get all the neighboring coordinates
           [ns-to-visit (filter (λ (n) (and
                                   (grid-value grid (first n) (second n)) ; filter out out-of-bounds
                                   (not (equal? (grid-value grid (first n) (second n)) 9)) ; and 9s
                                   (not (hash-ref visited n #f)))) ns)]) ; and the ones we've seen
      (hash-set! visited (list r c) (grid-value grid r c)) ; add this to the list of coordinates we've visited
      (for-each (λ (n) (loop (first n) (second n))) ns-to-visit) ; recurse over the neighbors
      (hash->list visited))))
             
(define (find-all-basins grid)
  (let* ([points (low-point-coords grid)])
    (map (λ (p) (find-basin-around-point grid (first p) (second p))) points)))

(define part2
  (let* ([basins (find-all-basins grid)]
         [top3 (take (sort basins (λ (x y) (> (length x) (length y)))) 3)]
         [lengths (map length top3)])
   (foldl * 1 lengths)))

part2