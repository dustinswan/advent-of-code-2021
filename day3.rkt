#lang racket

; utility functions

(define (char->number char)
  (if (equal? char #\0) 0 1))

(define (number->char num)
  (if (= num 0) #\0 #\1))

(define (string->numbers str)
  (map char->number (string->list str)))

(define (transpose xss)
  (apply map list xss))

; IO

(define lines
  (file->lines "day3.txt"))

(define chars
  (map string->numbers lines))

; part 1

(define (count-ones-in-list xss)
  (foldl + 0 (filter positive? xss)))

(define counts
  (map count-ones-in-list (transpose chars)))

(define gamma-bin-list
  (map (λ (count)
         (if (> count (/ (length chars) 2)) 1 0))
       counts))

(define (flip-bits num)
  (map (λ (n) (modulo (+ 1 n) 2)) num))

(define epsilon-bin-list (flip-bits gamma-bin-list))

(define (binary-list->decimal num)
  (string->number (list->string (map number->char num)) 2))

(define gamma-dec (binary-list->decimal gamma-bin-list))
(define epsilon-dec (binary-list->decimal epsilon-bin-list))
(define part1 (* gamma-dec epsilon-dec))

; part 2
; sounds like a fold to me

(define (most-common-digit column xs type)
  (define col-values (map (λ (x) (list-ref x column)) xs))
  (define ones (count-ones-in-list col-values))
  (define op (if (equal? type 'o2) >= <)) ; 'o2 uses >=, 'co2 uses <
  (if (op ones (/ (length xs) 2)) 1 0))

(define (get-rating xs type)
  (flatten
   (foldl (λ (column remaining)
            (define most-common (most-common-digit column remaining type))
            (if (= (length remaining) 1) ; if there's only one left
                remaining ; return it
                (filter (λ (ys)
                          (= (list-ref ys column) most-common))
                        remaining)))
          xs 
          (range (length (first xs))))))

(define o2-rating (binary-list->decimal (get-rating chars 'o2)))
(define co2-rating (binary-list->decimal (get-rating chars 'co2)))
(define part2 (* o2-rating co2-rating))
