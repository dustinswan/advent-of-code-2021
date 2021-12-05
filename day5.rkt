#lang racket

(define (map-map f lst) ; map . map
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define/match (add-dx-dy points)
  [((list (list x1 y1) (list x2 y2)))
   (let* ([dx (- x2 x1)]
          [dy (- y2 y1)]
          [len (max (abs dx) (abs dy))])
     (list (list x1 y1) (list x2 y2) dx dy len))])

(define (vectorize line)
  (let* ([parts (string-split line " -> ")] ; '("0,9 -> 5,9") -> '("0,9" "5,9")
         [strings (map (λ (part) (string-split part ",")) parts)] ; '("0,9" "5,9") -> '(("0" "9") ("5" "9"))
         [nums (map-map string->number strings)] ; '(("0" "9") ("5" "9")) -> '((0 9) (5 9))
         [vectors (add-dx-dy nums)]) ; '((x1 y2) (x2 y2) dx dy length)
    vectors))

(define/match (non-diagonal? vect)
  [((list _ _ dx dy _)) (or (= dx 0) (= dy 0))])

(define/match (get-points-from-vect vect) ; takes vect '((x1 y1) (x2 y2) dx dy len) and returns a list of points it hits '((x1 y1) (x2 y2) ..)
  [((list (list x1 y1) (list x2 y2) dx dy len))
   (let loop ([i len])
     (if (= i 0) ; at the end
         (list (list x1 y1)) ; return this point
         (append (list (list (+ x1 (/ (* dx i) len)) (+ y1 (/ (* dy i) len)))) ; return the next point..
                 (loop (sub1 i)))))]) ; appended to the result of recursing with i - 1

(define (add-point ht point)
  (let ([dups (hash-ref ht point 0)])
    (hash-set! ht point (+ 1 dups))))

(define (travel-and-add ht vects)
  (for ([i (in-list vects)])
    (let ([points (get-points-from-vect i)])
      (map (λ (p) (add-point ht p)) points))))

(define part1
  (let* ([lines (file->lines "day5.txt")]
         [vectors (map vectorize lines)]
         [non-diagonals (filter non-diagonal? vectors)]
         [ht (make-hash)]
         [points-hit (travel-and-add ht non-diagonals)]
         [counts (hash-values ht)])
         (length (filter (λ (x) (> x 1)) counts))))

part1

(define part2
  (let* ([lines (file->lines "day5.txt")]
         [vectors (map vectorize lines)]
         [ht (make-hash)]
         [points-hit (travel-and-add ht vectors)]
         [counts (hash-values ht)])
         (length (filter (λ (x) (> x 1)) counts))))

part2