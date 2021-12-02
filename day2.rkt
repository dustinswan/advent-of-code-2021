#lang racket

(define file "day2.txt")

(define lines (file->lines file))

(define steps (map string-split lines))

; part 1

(define (modify-position step position)
  ; (writeln position)
  (let ([instruction (car step)]
        [magnitude (string->number (second step))]
        [forwardness (car position)]
        [depth (cdr position)])
    (cond [(equal? instruction "forward") (cons (+ forwardness magnitude) depth)]
          [(equal? instruction "up") (cons forwardness (- depth magnitude))]
          [(equal? instruction "down") (cons forwardness (+ depth magnitude))])))

(define position (foldl modify-position (cons 0 0) steps)) ; (forwardness, depth)

(define part1 (* (car position) (cdr position)))

; part 2

(define (modify-position2 step position)
  ; (writeln position)
  (let ([instruction (first step)]
        [magnitude (string->number (second step))]
        [forwardness (first position)]
        [depth (second position)]
        [aim (third position)])
    (cond [(equal? instruction "forward") (list (+ forwardness magnitude) (+ depth (* aim magnitude)) aim)]
          [(equal? instruction "up") (list forwardness depth (- aim magnitude))]
          [(equal? instruction "down") (list forwardness depth (+ aim magnitude))])))

(define position2 (foldl modify-position2 (list 0 0 0) steps)) ; (forwardness, depth, aim)

(define part2 (* (first position2) (second position2)))