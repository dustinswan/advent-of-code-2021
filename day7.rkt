#lang racket

(define crabs
  (map string->number
       (string-split
        (first
         (file->lines "day7.txt")) ",")))

(define (distances crabs position)
  (foldl (λ (x acc) (+ acc (abs (- position x)))) 0 crabs))

(define (min-distance crab)
  (let* ([rng (range (apply min crabs) (apply max crabs))]
         [ds (map (λ (d) (distances crabs d)) rng)])
    (apply min ds)))

(define part1 (min-distance crabs))

part1

; part 2

(define (big-distance d)
  (if (= d 0) 0 (+ d (big-distance (sub1 d)))))

(define (memo1 fn arg1)
  (let* ([ht (make-hash)])
    (hash-ref! ht (list fn arg1) (fn arg1))))

(define bd2-memo (λ (x) (memo1 big-distance x)))

(define (distances2 crabs position)
  (foldl (λ (x acc) (+ acc (bd2-memo (abs (- position x))))) 0 crabs))

(define (min-distance2 crab)
  (let* ([rng (range (apply min crabs) (apply max crabs))]
         [ds (map (λ (d) (distances2 crabs d)) rng)])
    (apply min ds)))

(define part2 (min-distance2 crabs))

part2