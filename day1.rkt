#lang racket

(define test-data (list
199
200
208
210
200
207
240
269
260
263
))

; part 1
(define (sum data)
  (cdr
   (foldl (λ (el acc)
           (let ([prev (car acc)]
                 [total (cdr acc)]) 
             (if (> el prev)
                 (cons el (+ 1 total))
                 (cons el total))))
          (cons (car data) 0)
          data)))

(define part1 (sum test-data))

; part 2
; fold over the data
; initial value will be a pair of 2 lists
; first list is the original list, 2nd is the new sliding window
(define sliding-window
  (reverse
   (cdr
    (foldl (λ (el acc)
             ;(writeln acc) ; debug
             (let ([orig (car acc)] ; grab the rest of the original list from the accumulator
                   [window (cdr acc)] ; grab the window list that we have so far
                   [one (car (car acc))] ; first element of original list
                   [two (car (rest (car acc)))]
                   [three (car (rest (rest (car acc))))])
               (cons (rest orig) (cons (+ one two three) window)))) ; return a pair - first elem is the new window list. 2nd elem is the rest of the original list
           (cons (append test-data (list 0 0)) empty) ; start with an empty window list an the original list. add 2 0s at the end to cheat
           test-data))))

(define part2 (sum sliding-window))