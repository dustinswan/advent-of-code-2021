#lang racket

(define (tick fishes)
  (foldl (λ (fish lst)
           (if (= fish 0)
               (cons 6 (cons 8 lst))
               (cons (sub1 fish) lst))) '() fishes))

(define (generations fishes num)
  (if (= num 0)
      fishes
      (generations (tick fishes) (sub1 num))))

(define part1
  (let* ([line (first (file->lines "day6.txt"))]
         [fishes (map string->number (string-split line ","))])
        (length (generations fishes 80))))

part1

; part2

(define (memo2 fn arg1 arg2)
  (let* ([ht (make-hash)])
    (hash-ref! ht (list fn arg1 arg2) (fn arg1 arg2))))

(define/match (count-one digit time-left)
  [(n 0) 1]
  [(0 t) (+ (count-one 6 (sub1 t)) (count-one 8 (sub1 t)))]
  [(n t) (count-one (sub1 n) (sub1 t))])

(define count-one-memo (λ (x y) (memo2 count-one x y)))

(define part2-slow
  (let* ([line (first (file->lines "day6.txt"))]
         [fishes (map string->number (string-split line ","))])
    (foldl (λ (x acc) (+ acc (count-one-memo x 100))) 0 fishes)))

; okay so memoing didn't help. ahhh i know what to do

(define (tick2 ht)
  (define temp (hash-copy ht))
  (hash-set! ht 0 (hash-ref temp 1 0))
  (hash-set! ht 1 (hash-ref temp 2 0))
  (hash-set! ht 2 (hash-ref temp 3 0))
  (hash-set! ht 3 (hash-ref temp 4 0))
  (hash-set! ht 4 (hash-ref temp 5 0))
  (hash-set! ht 5 (hash-ref temp 6 0))
  (hash-set! ht 6 (+ (hash-ref temp 0 0) (hash-ref temp 7 0)))
  (hash-set! ht 7 (hash-ref temp 8 0))
  (hash-set! ht 8 (hash-ref temp 0 0)))

(define (add-num ht num)
  (hash-set! ht num (+ 1 (hash-ref ht num 0))))

(define (init ht nums)
  (for ([num (in-list nums)])
    (add-num ht num)))

(define (loop ht max)
  (if (= 0 max)
      ht
      (begin
        (tick2 ht)
        (loop ht (sub1 max)))))

(define part2
  (let* ([line (first (file->lines "day6.txt"))]
         [fishes (map string->number (string-split line ","))]
         [ht (make-hash)])
    (begin
     (init ht fishes)
     (loop ht 256)
     (foldl + 0 (hash-values ht)))))

part2
