#lang racket

(define (map-map f lst) ; map . map
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define data
  (let* ([input (file->lines "day13.txt")]
         [point-lines (filter (λ (l) (string-contains? l ",")) input)]
         [points (map-map string->number (map (λ (p) (string-split p ",")) point-lines))] ; e.g. '(6 10)
         [fold-lines (filter (λ (l) (string-contains? l "fold")) input)]
         [folds (map (λ (l) (let* ([matches (regexp-match #rx"(.)=(.+)" l)]) ; e.g. '("y" 7)
                              (list (second matches) (string->number (third matches))))) fold-lines)])
    (list points folds)))

(define points (first data))
(define folds (second data))

(define (draw points)
  (let* ([width (apply max (map first points))] [height (apply max (map second points))])
    (for-each (λ (row) (begin
                (for-each (λ (col) (printf "~a" (if (member (list col row) points) "#" "."))) (range 0 (+ 1 width)))
                (printf "~n"))) (range 0 (+ 1 height)))))

(define (move-point point dir loc)
  (let* ([which (if (equal? "x" dir) first second)] ; which coordinate to change. x -> fold left -> change the first digit, etc.
         [diff (* 2 (- (which point) loc))] ; calculate the amount the point should move left/up
         [diffX (if (equal? "x" dir) (λ (x) (- x diff)) identity)]
         [diffY (if (equal? "y" dir) (λ (x) (- x diff)) identity)])
    (if (< (which point) loc) ; if it is below the fold
        point ; just return it
        (list (diffX (first point)) (diffY (second point)))))) ; otherwise, move the point

(define (fold points f) ; should have picked a different name..
  (let* ([direction (first f)]
         [location (second f)]
         [new-points (map (λ (point) (move-point point direction location)) points)])
   (remove-duplicates new-points)))

(define part1
  (let* ([first-fold (first folds)]
         [next (fold points first-fold)])
    (length next)))

part1

; PART 2

(define part2
  (draw
   (foldl (λ (f sheet) (fold sheet f)) points folds)))

part2